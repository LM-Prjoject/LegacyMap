package com.legacymap.backend.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
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

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
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
            Map<String, Object> reminderConfig = objectMapper.convertValue(
                    event.getReminder(),
                    objectMapper.getTypeFactory().constructMapType(Map.class, String.class, Object.class)
            );

            Integer daysBefore = (Integer) reminderConfig.get("days_before");
            @SuppressWarnings("unchecked")
            List<String> methods = (List<String>) reminderConfig.get("methods");

            if (daysBefore == null || methods == null || methods.isEmpty()) {
                log.warn("Invalid reminder config for event {}", event.getId());
                return;
            }

            LocalDateTime reminderTime = event.getStartDate().minusDays(daysBefore);

            createReminderForUser(event, event.getCreatedBy(), methods, reminderTime);

        } catch (IllegalArgumentException e) {
            log.error("Error parsing reminder config for event {}", event.getId(), e);
            throw new AppException(ErrorCode.INVALID_INPUT_DATA);
        }
    }

    private void createReminderForUser(Event event, User user, List<String> methods, LocalDateTime reminderTime) {
        for (String method : methods) {
            try {
                EventReminder reminder = EventReminder.builder()
                        .event(event)
                        .user(user)
                        .scheduledAt(reminderTime)
                        .sendMethod(EventReminder.SendMethod.valueOf(method.toUpperCase()))
                        .status(EventReminder.ReminderStatus.PENDING)
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
        // Delete existing reminders and create new ones
        List<EventReminder> existingReminders = eventReminderRepository.findByEventId(event.getId());
        if (!existingReminders.isEmpty()) {
            eventReminderRepository.deleteAll(existingReminders);
            log.info("Deleted {} existing reminders for event {}", existingReminders.size(), event.getId());
        }
        createRemindersForEvent(event);
    }

    @Scheduled(fixedRate = 60000)
    @Transactional
    public void processPendingReminders() {
        List<EventReminder> pendingReminders = eventReminderRepository.findPendingReminders(LocalDateTime.now());

        if (pendingReminders.isEmpty()) {
            return;
        }

        log.info("Processing {} pending reminders", pendingReminders.size());

        for (EventReminder reminder : pendingReminders) {
            try {
                sendReminder(reminder);
                reminder.setStatus(EventReminder.ReminderStatus.SENT);
                reminder.setSentAt(LocalDateTime.now());
                eventReminderRepository.save(reminder);
                log.info("Successfully sent reminder {}", reminder.getId());
            } catch (Exception e) {
                log.error("Failed to send reminder {}", reminder.getId(), e);
                reminder.setStatus(EventReminder.ReminderStatus.FAILED);
                eventReminderRepository.save(reminder);
            }
        }
    }

    private void sendReminder(EventReminder reminder) {
        Event event = reminder.getEvent();
        User user = reminder.getUser();

        if (event == null || user == null) {
            log.error("Event or User not found for reminder {}", reminder.getId());
            return;
        }

        notificationService.sendEventReminderNotification(
                user.getId(),
                event.getTitle(),
                event.getStartDate(),
                event.getId()
        );

        if (reminder.getSendMethod() == EventReminder.SendMethod.EMAIL ||
                reminder.getSendMethod() == EventReminder.SendMethod.BOTH) {

            String emailTitle = "Nhắc nhở sự kiện: " + event.getTitle();
            String emailMessage = buildReminderMessage(event);
            sendEmail(user.getEmail(), emailTitle, emailMessage);
        }
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

    private String formatDateTime(LocalDateTime dateTime) {
        return dateTime.format(java.time.format.DateTimeFormatter.ofPattern("HH:mm dd/MM/yyyy"));
    }

    @Transactional(readOnly = true)
    public List<EventReminderResponse> getUserReminders(UUID userId) {
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));

        return eventReminderRepository.findByUserAndStatusOrderByScheduledAtAsc(
                        user, EventReminder.ReminderStatus.PENDING)
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
        EventReminderResponse response = new EventReminderResponse();
        response.setId(reminder.getId());
        response.setEventId(reminder.getEvent().getId());
        response.setUserId(reminder.getUser().getId());
        response.setSendMethod(reminder.getSendMethod());
        response.setScheduledAt(reminder.getScheduledAt());
        response.setSentAt(reminder.getSentAt());
        response.setStatus(reminder.getStatus());
        response.setCreatedAt(reminder.getCreatedAt());
        return response;
    }

    // Helper method để kiểm tra và tạo lại reminders cho events sắp tới
    @Scheduled(cron = "0 0 6 * * ?")
    @Transactional
    public void checkAndCreateRecurringEventReminders() {
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime nextWeek = now.plusWeeks(1);

        List<Event> recurringEvents = eventRepository.findRecurringEventsInRange(now, nextWeek);

        for (Event event : recurringEvents) {
            try {
                List<EventReminder> existingReminders = eventReminderRepository.findByEventId(event.getId());
                if (existingReminders.isEmpty()) {
                    createRemindersForEvent(event);
                    log.info("Created reminders for recurring event {}", event.getId());
                }
            } catch (Exception e) {
                log.error("Error creating reminders for recurring event {}", event.getId(), e);
            }
        }
    }
}