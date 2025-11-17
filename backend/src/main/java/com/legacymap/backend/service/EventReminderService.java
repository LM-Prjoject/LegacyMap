package com.legacymap.backend.service;

import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.legacymap.backend.dto.request.EventCreateRequest;
import com.legacymap.backend.dto.response.EventReminderResponse;
import com.legacymap.backend.entity.Event;
import com.legacymap.backend.entity.EventReminder;
import com.legacymap.backend.entity.Person;
import com.legacymap.backend.entity.PersonUserLink;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.EventReminderRepository;
import com.legacymap.backend.repository.EventRepository;
import com.legacymap.backend.repository.PersonRepository;
import com.legacymap.backend.repository.PersonUserLinkRepository;
import com.legacymap.backend.repository.UserRepository;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@RequiredArgsConstructor
public class EventReminderService {

    private final EventReminderRepository eventReminderRepository;
    private final EventRepository eventRepository;
    private final UserRepository userRepository;
    private final NotificationService notificationService;
    private final EmailService emailService;
    private final ObjectMapper objectMapper;
    private final PersonRepository personRepository;
    private final PersonUserLinkRepository personUserLinkRepository;

    @Transactional
    public void createRemindersForEvent(Event event) {
        EventCreateRequest.ReminderConfig reminderCfg = objectMapper.convertValue(
                event.getReminder(), EventCreateRequest.ReminderConfig.class
        );
        Integer daysBefore = reminderCfg.getDaysBefore();
        List<String> methods = reminderCfg.getMethods();
        if (daysBefore == null || methods == null || methods.isEmpty()) return;

        OffsetDateTime reminderTimeUtc = daysBefore == 0
                ? OffsetDateTime.now(ZoneOffset.UTC)
                : event.getStartDate().minusDays(daysBefore);

        createReminderForRecipient(event, EventReminder.RecipientType.user, event.getCreatedBy().getId(), methods, reminderTimeUtc);

        if (event.getRelatedPersons() != null) {
            List<EventCreateRequest.RelatedPerson> related = objectMapper.convertValue(
                    event.getRelatedPersons(),
                    objectMapper.getTypeFactory().constructCollectionType(List.class, EventCreateRequest.RelatedPerson.class)
            );

            for (EventCreateRequest.RelatedPerson rp : related) {
                createReminderForRecipient(event, EventReminder.RecipientType.person, rp.getId(), methods, reminderTimeUtc);
            }
        }
    }

    private void createReminderForRecipient(Event event, EventReminder.RecipientType recipientType,
                                            UUID recipientId, List<String> methods, OffsetDateTime reminderTime) {
        for (String method : methods) {
            EventReminder.SendMethod sendMethod = switch (method.toLowerCase()) {
                case "email" -> EventReminder.SendMethod.email;
                case "notification" -> EventReminder.SendMethod.notification;
                case "both" -> EventReminder.SendMethod.both;
                default -> throw new IllegalArgumentException("Invalid method: " + method);
            };

            EventReminder reminder = EventReminder.builder()
                    .event(event)
                    .recipientType(recipientType)
                    .recipientId(recipientId)
                    .sendMethod(sendMethod)
                    .scheduledAt(reminderTime)
                    .status(EventReminder.ReminderStatus.pending)
                    .build();

            eventReminderRepository.save(reminder);
        }
    }

    @Transactional
    public void updateRemindersForEvent(Event event) {
        List<EventReminder> existingReminders = eventReminderRepository.findByEventId(event.getId());
        if (!existingReminders.isEmpty()) {
            eventReminderRepository.deleteAll(existingReminders);
            log.info("Deleted {} existing reminders for event {}", existingReminders.size(), event.getId());
        }
        createRemindersForEvent(event);
    }

    @Scheduled(fixedRateString = "${app.reminders.poll-interval-ms:300000}")
    @Transactional
    public void processPendingReminders() {
        OffsetDateTime nowUtc = OffsetDateTime.now(ZoneOffset.UTC);

        List<EventReminder> pending = eventReminderRepository.findPendingReminders(nowUtc);

        if (pending.isEmpty()) return;

        log.info("Processing {} pending reminder(s)", pending.size());

        for (EventReminder r : pending) {
            try {
                sendReminder(r);
                r.setStatus(EventReminder.ReminderStatus.sent);
                r.setSentAt(OffsetDateTime.now(ZoneOffset.UTC));
            } catch (Exception e) {
                log.error("Failed reminder {} – {}", r.getId(), e.toString());
                r.setStatus(EventReminder.ReminderStatus.failed);
                r.setSentAt(OffsetDateTime.now(ZoneOffset.UTC));
            }
            eventReminderRepository.save(r);
        }
    }

    private void sendReminder(EventReminder reminder) {
        Event event = reminder.getEvent();
        boolean notifSuccess = true, emailSuccess = true;

        String email = null;
        UUID userId = null;

        // Xác định người nhận
        if (reminder.getRecipientType() == EventReminder.RecipientType.user) {
            User user = userRepository.findById(reminder.getRecipientId()).orElse(null);
            if (user != null) {
                email = user.getEmail();
                userId = user.getId();
            }
        } else if (reminder.getRecipientType() == EventReminder.RecipientType.person) {
            Person person = personRepository.findById(reminder.getRecipientId()).orElse(null);
            if (person != null && person.getEmail() != null && !person.getEmail().isBlank()) {
                email = person.getEmail();
            }
            // Nếu có liên kết user
            Optional<PersonUserLink> link = personUserLinkRepository
                    .findByPersonIdAndLinkType(reminder.getRecipientId(), PersonUserLink.LinkType.self);
            userId = link.map(l -> l.getUser().getId()).orElse(null);
        }

        EventReminder.SendMethod method = reminder.getSendMethod();

        // Notification
        if ((method == EventReminder.SendMethod.notification || method == EventReminder.SendMethod.both) && userId != null) {
            try {
                notificationService.sendEventReminderNotification(
                        userId, event.getTitle(), event.getStartDate(), event.getId()
                );
            } catch (Exception e) {
                notifSuccess = false;
            }
        }

        // Email
        if ((method == EventReminder.SendMethod.email || method == EventReminder.SendMethod.both) && email != null) {
            try {
                String subject = "Nhắc nhở sự kiện: " + event.getTitle();
                String message = buildReminderMessage(event);
                emailService.sendEmail(email, subject, message);
            } catch (Exception e) {
                emailSuccess = false;
            }
        }

        boolean isSent = switch (method) {
            case both -> notifSuccess || emailSuccess;
            case notification -> notifSuccess;
            case email -> emailSuccess;
        };

        reminder.setStatus(isSent ? EventReminder.ReminderStatus.sent : EventReminder.ReminderStatus.failed);
        reminder.setSentAt(OffsetDateTime.now(ZoneOffset.UTC));
    }

    @Transactional(readOnly = true)
    public List<EventReminderResponse> getUserReminders(UUID userId) {
        List<EventReminder> reminders = eventReminderRepository
                .findByRecipientTypeAndRecipientIdOrderByScheduledAtAsc(
                        EventReminder.RecipientType.user, userId);

        return reminders.stream()
                .map(this::mapReminderToResponse)
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public List<EventReminderResponse> getEventReminders(UUID eventId, UUID userId) {
        Event event = eventRepository.findById(eventId)
                .orElseThrow(() -> new AppException(ErrorCode.EVENT_NOT_FOUND));

        if (!event.getCreatedBy().getId().equals(userId) && !event.getIsPublic()) {
            throw new AppException(ErrorCode.UNAUTHORIZED);
        }

        return eventReminderRepository.findByEventId(eventId)
                .stream()
                .map(this::mapReminderToResponse)
                .collect(Collectors.toList());
    }

    @Transactional
    public void deleteRemindersForEvent(UUID eventId, UUID userId) {
        Event event = eventRepository.findById(eventId)
                .orElseThrow(() -> new AppException(ErrorCode.EVENT_NOT_FOUND));

        if (!event.getCreatedBy().getId().equals(userId)) {
            throw new AppException(ErrorCode.UNAUTHORIZED);
        }

        List<EventReminder> reminders = eventReminderRepository.findByEventId(eventId);
        if (!reminders.isEmpty()) {
            eventReminderRepository.deleteAll(reminders);
            log.info("Deleted {} reminders for event {}", reminders.size(), eventId);
        }
    }

    private EventReminderResponse mapReminderToResponse(EventReminder reminder) {
        return EventReminderResponse.builder()
                .id(reminder.getId())
                .eventId(reminder.getEvent().getId())
                .recipientType(reminder.getRecipientType())
                .recipientId(reminder.getRecipientId())
                .sendMethod(reminder.getSendMethod())
                .scheduledAt(reminder.getScheduledAt())
                .sentAt(reminder.getSentAt())
                .status(reminder.getStatus())
                .createdAt(reminder.getCreatedAt())
                .build();
    }

    private String buildReminderMessage(Event event) {
        StringBuilder message = new StringBuilder();
        message.append("Sự kiện: ").append(event.getTitle()).append("\n");

        if (event.getDescription() != null && !event.getDescription().isEmpty()) {
            message.append("Mô tả: ").append(event.getDescription()).append("\n");
        }

        message.append("Thời gian: ").append(formatDateTime(event.getStartDate()));

        if (event.getLocation() != null && !event.getLocation().isEmpty()) {
            message.append("\nĐịa điểm: ").append(event.getLocation());
        }

        message.append("\n\nHãy chuẩn bị cho sự kiện này!");
        return message.toString();
    }

    private String formatDateTime(OffsetDateTime dateTime) {
        ZonedDateTime vietnamTime = dateTime.atZoneSameInstant(ZoneId.of("Asia/Ho_Chi_Minh"));
        return vietnamTime.format(DateTimeFormatter.ofPattern("HH:mm dd/MM/yyyy"));
    }

    @Scheduled(cron = "0 0 6 * * ?")
    @Transactional
    public void checkAndCreateRecurringEventReminders() {
        OffsetDateTime nowUtc = OffsetDateTime.now(ZoneOffset.UTC);
        OffsetDateTime nextWeekUtc = nowUtc.plusWeeks(1);

        List<Event> recurringEvents = eventRepository.findRecurringEventsInRange(nowUtc, nextWeekUtc);

        for (Event event : recurringEvents) {
            try {
                List<EventReminder> existing = eventReminderRepository.findByEventId(event.getId())
                        .stream()
                        .filter(r -> r.getScheduledAt().isAfter(nowUtc.minusDays(1)) && r.getScheduledAt().isBefore(nextWeekUtc))
                        .toList();

                if (existing.isEmpty()) {
                    createRemindersForEvent(event);
                    log.info("Created reminder for recurring event {}", event.getId());
                }
            } catch (Exception e) {
                log.error("Error processing recurring event {}", event.getId(), e);
            }
        }
    }
}