package com.legacymap.backend.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.legacymap.backend.dto.request.EventCreateRequest;
import com.legacymap.backend.dto.response.EventReminderResponse;
import com.legacymap.backend.entity.Event;
import com.legacymap.backend.entity.EventReminder;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.EventReminderRepository;
import com.legacymap.backend.repository.EventRepository;
import com.legacymap.backend.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

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

    @Transactional
    public void createRemindersForEvent(Event event) {
        try {
            EventCreateRequest.ReminderConfig reminderCfg = objectMapper.convertValue(
                    event.getReminder(), EventCreateRequest.ReminderConfig.class
            );
            Integer daysBefore = reminderCfg.getDaysBefore();
            List<String> methods = reminderCfg.getMethods();
            if (daysBefore == null || methods == null || methods.isEmpty()) {
                log.warn("Invalid reminder config for event {}", event.getId());
                return;
            }

            OffsetDateTime reminderTimeUtc;
            if (daysBefore == 0) {
                // Gửi NGAY LẬP TỨC nếu chọn "bây giờ"
                reminderTimeUtc = OffsetDateTime.now(ZoneOffset.UTC);
            } else {
                reminderTimeUtc = event.getStartDate().minusDays(daysBefore);
            }

            createReminderForUser(event, event.getCreatedBy(), methods, reminderTimeUtc);
        } catch (IllegalArgumentException e) {
            log.error("Error parsing reminder config for event {}", event.getId(), e);
            throw new AppException(ErrorCode.INVALID_INPUT_DATA);
        }
    }

    private void createReminderForUser(Event event, User user, List<String> methods, OffsetDateTime reminderTime) {
        if (user == null) {
            log.warn("User is null when creating reminder for event {}", event.getId());
            return;
        }

        for (String method : methods) {
            try {
                EventReminder.SendMethod sendMethod = switch (method.toLowerCase()) {
                    case "email" -> EventReminder.SendMethod.email;
                    case "notification" -> EventReminder.SendMethod.notification;
                    case "both" -> EventReminder.SendMethod.both;
                    default -> throw new IllegalArgumentException("Invalid method: " + method);
                };

                EventReminder reminder = EventReminder.builder()
                        .event(event)
                        .user(user)
                        .scheduledAt(reminderTime)
                        .sendMethod(sendMethod)
                        .status(EventReminder.ReminderStatus.pending)
                        .build();

                eventReminderRepository.save(reminder);
                log.info("Created reminder for event {} for user {} with method {}",
                        event.getId(), user.getId(), method);
            } catch (IllegalArgumentException e) {
                log.warn("Invalid send method: {} for event {}", method, event.getId());
            }
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

//    @Scheduled(fixedRate = 60000)
//    @Transactional
//    public void processPendingReminders() {
//        OffsetDateTime nowUtc = OffsetDateTime.now(ZoneOffset.UTC);
//        // Lấy các reminder đã đến HOẶC quá 1 phút (để xử lý "gửi ngay")
//        OffsetDateTime threshold = nowUtc.minusMinutes(1);
//        List<EventReminder> pendingReminders = eventReminderRepository.findPendingRemindersBefore(nowUtc);
//
//        if (pendingReminders.isEmpty()) {
//            return;
//        }
//
//        log.info("Processing {} pending reminders (now: {}, threshold: {})",
//                pendingReminders.size(), nowUtc, threshold);
//
//        for (EventReminder reminder : pendingReminders) {
//            try {
//                log.info("Sending reminder {} scheduled for {}", reminder.getId(), reminder.getScheduledAt());
//                sendReminder(reminder);
//                reminder.setStatus(EventReminder.ReminderStatus.sent);
//                reminder.setSentAt(OffsetDateTime.now(ZoneOffset.UTC));
//                eventReminderRepository.save(reminder);
//                log.info("Successfully sent reminder {}", reminder.getId());
//            } catch (Exception e) {
//                log.error("Failed to send reminder {}", reminder.getId(), e);
//                reminder.setStatus(EventReminder.ReminderStatus.failed);
//                reminder.setSentAt(OffsetDateTime.now(ZoneOffset.UTC));
//                eventReminderRepository.save(reminder);
//            }
//        }
//    }

    @Scheduled(fixedRate = 30_000)
    @Transactional
    public void processPendingReminders() {
        OffsetDateTime nowUtc = OffsetDateTime.now(ZoneOffset.UTC);

        // Lấy tất cả reminder <= now (bao gồm "bây giờ")
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
        User user = reminder.getUser();
        boolean notifSuccess = true, emailSuccess = true;
        EventReminder.SendMethod method = reminder.getSendMethod();

        if (method == EventReminder.SendMethod.notification || method == EventReminder.SendMethod.both) {
            try {
                notificationService.sendEventReminderNotification(
                        user.getId(), event.getTitle(), event.getStartDate(), event.getId()
                );
                log.info("Web notification queued for reminder {}", reminder.getId());
            } catch (Exception e) {
                log.error("Failed to queue web notification for reminder {}", reminder.getId(), e);
                notifSuccess = false;
            }
        }

        if (method == EventReminder.SendMethod.email || method == EventReminder.SendMethod.both) {
            try {
                String subject = "Nhắc nhở sự kiện: " + event.getTitle();
                String message = buildReminderMessage(event);
                emailService.sendEmail(user.getEmail(), subject, message);
                log.info("Email sent for reminder {}", reminder.getId());
            } catch (Exception e) {
                log.error("Failed to send email for reminder {}", reminder.getId(), e);
                emailSuccess = false;
            }
        }

        boolean isSent = switch (method) {
            case both -> notifSuccess || emailSuccess;
            case notification -> notifSuccess;
            case email -> emailSuccess;
        };

        reminder.setStatus(isSent ? EventReminder.ReminderStatus.sent : EventReminder.ReminderStatus.failed);
        // Luôn ghi sent_at khi xử lý
        reminder.setSentAt(OffsetDateTime.now(ZoneOffset.UTC));
    }

    private void sendEmail(String toEmail, String subject, String message) {
        try {
            emailService.sendEmail(toEmail, subject, message);
        } catch (Exception e) {
            log.error("Failed to send email to {}", toEmail, e);
        }
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

    @Transactional(readOnly = true)
    public List<EventReminderResponse> getUserReminders(UUID userId) {
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));

        return eventReminderRepository.findByUserAndStatusOrderByScheduledAtAsc(
                        user, EventReminder.ReminderStatus.pending)
                .stream()
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
                .userId(reminder.getUser().getId())
                .sendMethod(reminder.getSendMethod())
                .scheduledAt(reminder.getScheduledAt())
                .sentAt(reminder.getSentAt())
                .status(reminder.getStatus())
                .createdAt(reminder.getCreatedAt())
                .build();
    }

    // Helper method để kiểm tra và tạo lại reminders cho events sắp tới
    @Scheduled(cron = "0 0 6 * * ?")
    @Transactional
    public void checkAndCreateRecurringEventReminders() {
        OffsetDateTime nowUtc = OffsetDateTime.now(ZoneOffset.UTC);
        OffsetDateTime nextWeekUtc = nowUtc.plusWeeks(1);

        List<Event> recurringEvents = eventRepository.findRecurringEventsInRange(nowUtc, nextWeekUtc);

        for (Event event : recurringEvents) {
            try {
                // Kiểm tra xem đã có reminder cho event này trong 7 ngày tới chưa
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