package com.legacymap.backend.service;

import com.legacymap.backend.dto.request.EventCreateRequest;
import com.legacymap.backend.dto.request.EventUpdateRequest;
import com.legacymap.backend.dto.response.EventResponse;
import com.legacymap.backend.entity.Event;
import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.EventRepository;
import com.legacymap.backend.repository.FamilyTreeRepository;
import com.legacymap.backend.repository.UserRepository;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
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
public class EventService {

    private final EventRepository eventRepository;
    private final FamilyTreeRepository familyTreeRepository;
    private final UserRepository userRepository;
    private final EventReminderService eventReminderService;
    private final ObjectMapper objectMapper;

    private User loadUserOrThrow(UUID userId) {
        return userRepository.findById(userId)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));
    }

    private FamilyTree findTreeWithAccessOrThrow(UUID treeId, UUID userId) {
        User user = loadUserOrThrow(userId);
        return familyTreeRepository.findByIdAndCreatedBy(treeId, user)
                .orElseThrow(() -> new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND));
    }

    @Transactional
    public EventResponse create(UUID treeId, UUID userId, EventCreateRequest request) {
        User creator = loadUserOrThrow(userId);
        FamilyTree tree = null;

        if (treeId != null) {
            tree = findTreeWithAccessOrThrow(treeId, userId);
        }

        Event event = Event.builder()
                .familyTree(tree)
                .personalOwner(tree == null ? creator : null)
                .createdBy(creator)
                .title(request.getTitle())
                .description(request.getDescription())
                .eventType(request.getEventType())
                .startDate(request.getStartDate())
                .endDate(request.getEndDate())
                .isFullDay(request.getIsFullDay() != null ? request.getIsFullDay() : false)
                .calendarType(request.getCalendarType() != null ? request.getCalendarType() : Event.CalendarType.SOLAR)
                .isRecurring(request.getIsRecurring() != null ? request.getIsRecurring() : false)
                .recurrenceRule(request.getRecurrenceRule() != null ? request.getRecurrenceRule() : Event.RecurrenceRule.NONE)
                .location(request.getLocation())
                .isPublic(request.getIsPublic() != null ? request.getIsPublic() : true)
                .status(Event.EventStatus.ACTIVE)
                .build();

        try {
            if (request.getRelatedPersons() != null) {
                event.setRelatedPersons(objectMapper.valueToTree(request.getRelatedPersons()));
            }
            if (request.getLocationCoordinates() != null) {
                event.setLocationCoordinates(objectMapper.valueToTree(request.getLocationCoordinates()));
            }
            if (request.getReminder() != null) {
                event.setReminder(objectMapper.valueToTree(request.getReminder()));
            }
        } catch (IllegalArgumentException e) {
            log.error("Error parsing JSON data", e);
            throw new AppException(ErrorCode.INVALID_INPUT_DATA);
        }

        Event savedEvent = eventRepository.save(event);

        // Tạo reminders nếu có
        if (request.getReminder() != null) {
            eventReminderService.createRemindersForEvent(savedEvent);
        }

        return mapToResponse(savedEvent);
    }

    @Transactional
    public EventResponse update(UUID eventId, UUID userId, EventUpdateRequest request) {
        Event event = eventRepository.findById(eventId)
                .orElseThrow(() -> new AppException(ErrorCode.EVENT_NOT_FOUND));

        if (!event.getCreatedBy().getId().equals(userId)) {
            throw new AppException(ErrorCode.UNAUTHORIZED);
        }

        if (request.getTitle() != null) event.setTitle(request.getTitle());
        if (request.getDescription() != null) event.setDescription(request.getDescription());
        if (request.getEventType() != null) event.setEventType(request.getEventType());
        if (request.getStartDate() != null) event.setStartDate(request.getStartDate());
        if (request.getEndDate() != null) event.setEndDate(request.getEndDate());
        if (request.getIsFullDay() != null) event.setIsFullDay(request.getIsFullDay());
        if (request.getCalendarType() != null) event.setCalendarType(request.getCalendarType());
        if (request.getIsRecurring() != null) event.setIsRecurring(request.getIsRecurring());
        if (request.getRecurrenceRule() != null) event.setRecurrenceRule(request.getRecurrenceRule());
        if (request.getLocation() != null) event.setLocation(request.getLocation());
        if (request.getIsPublic() != null) event.setIsPublic(request.getIsPublic());
        if (request.getStatus() != null) event.setStatus(request.getStatus());

        try {
            if (request.getRelatedPersons() != null) {
                event.setRelatedPersons(objectMapper.valueToTree(request.getRelatedPersons()));
            }
            if (request.getLocationCoordinates() != null) {
                event.setLocationCoordinates(objectMapper.valueToTree(request.getLocationCoordinates()));
            }
            if (request.getReminder() != null) {
                event.setReminder(objectMapper.valueToTree(request.getReminder()));
                eventReminderService.updateRemindersForEvent(event);
            }
        } catch (IllegalArgumentException e) {
            log.error("Error parsing JSON data", e);
            throw new AppException(ErrorCode.INVALID_INPUT_DATA);
        }

        Event updatedEvent = eventRepository.save(event);
        return mapToResponse(updatedEvent);
    }

    @Transactional
    public void delete(UUID eventId, UUID userId) {
        Event event = eventRepository.findById(eventId)
                .orElseThrow(() -> new AppException(ErrorCode.EVENT_NOT_FOUND));

        if (!event.getCreatedBy().getId().equals(userId)) {
            throw new AppException(ErrorCode.UNAUTHORIZED);
        }

        eventRepository.delete(event);
    }

    @Transactional(readOnly = true)
    public EventResponse getById(UUID eventId, UUID userId) {
        Event event = eventRepository.findById(eventId)
                .orElseThrow(() -> new AppException(ErrorCode.EVENT_NOT_FOUND));

        if (!event.getIsPublic() && !event.getCreatedBy().getId().equals(userId)) {
            throw new AppException(ErrorCode.UNAUTHORIZED);
        }

        return mapToResponse(event);
    }

    @Transactional(readOnly = true)
    public List<EventResponse> getByFamilyTree(UUID treeId, UUID userId) {
        FamilyTree tree = findTreeWithAccessOrThrow(treeId, userId);

        return eventRepository.findByFamilyTreeAndStartDateBetweenOrderByStartDateAsc(
                        tree, LocalDateTime.now(), LocalDateTime.now().plusMonths(1))
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public List<EventResponse> getPersonalEvents(UUID userId) {
        return eventRepository.findByCreatedByAndFamilyTreeIsNullOrderByStartDateAsc(
                        loadUserOrThrow(userId))
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public List<EventResponse> getUpcomingEvents(UUID userId, int limit) {
        User user = loadUserOrThrow(userId);
        List<FamilyTree> userTrees = familyTreeRepository.findAllByCreatedBy(user);

        List<UUID> treeIds = userTrees.stream()
                .map(FamilyTree::getId)
                .collect(Collectors.toList());

        return eventRepository.findUpcomingEvents(treeIds, LocalDateTime.now(),
                        PageRequest.of(0, limit))
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public List<EventResponse> getEventsInDateRange(UUID treeId, UUID userId,
                                                    LocalDateTime start, LocalDateTime end) {
        FamilyTree tree = findTreeWithAccessOrThrow(treeId, userId);

        return eventRepository.findEventsInDateRange(tree, start, end)
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    private EventResponse mapToResponse(Event event) {
        EventResponse response = new EventResponse();
        response.setId(event.getId());
        response.setFamilyTreeId(event.getFamilyTree() != null ? event.getFamilyTree().getId() : null);
        response.setCreatedBy(event.getCreatedBy().getId());
        response.setTitle(event.getTitle());
        response.setDescription(event.getDescription());
        response.setEventType(event.getEventType());
        response.setStartDate(event.getStartDate());
        response.setEndDate(event.getEndDate());
        response.setIsFullDay(event.getIsFullDay());
        response.setCalendarType(event.getCalendarType());
        response.setIsRecurring(event.getIsRecurring());
        response.setRecurrenceRule(event.getRecurrenceRule());
        response.setLocation(event.getLocation());
        response.setIsPublic(event.getIsPublic());
        response.setStatus(event.getStatus());
        response.setCreatedAt(event.getCreatedAt());
        response.setUpdatedAt(event.getUpdatedAt());

        try {
            if (event.getRelatedPersons() != null) {
                response.setRelatedPersons(objectMapper.convertValue(
                        event.getRelatedPersons(),
                        objectMapper.getTypeFactory().constructCollectionType(List.class, EventResponse.RelatedPerson.class)
                ));
            }
            if (event.getLocationCoordinates() != null) {
                response.setLocationCoordinates(objectMapper.convertValue(
                        event.getLocationCoordinates(),
                        objectMapper.getTypeFactory().constructMapType(Map.class, String.class, Object.class)
                ));
            }
            if (event.getReminder() != null) {
                response.setReminder(objectMapper.convertValue(
                        event.getReminder(),
                        EventResponse.ReminderConfig.class
                ));
            }
        } catch (IllegalArgumentException e) {
            log.error("Error parsing JSON from database for event {}", event.getId(), e);
        }

        return response;
    }
}