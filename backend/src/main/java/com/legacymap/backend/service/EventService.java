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
import com.fasterxml.jackson.databind.ObjectMapper;
import com.legacymap.backend.utils.LunarSolarConverter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
        FamilyTree tree = treeId != null ? findTreeWithAccessOrThrow(treeId, userId) : null;

        OffsetDateTime startUtc = request.getStartDate() != null
                ? request.getStartDate().withOffsetSameInstant(ZoneOffset.UTC)
                : null;
        OffsetDateTime endUtc = request.getEndDate() != null
                ? request.getEndDate().withOffsetSameInstant(ZoneOffset.UTC)
                : null;

        Event event = Event.builder()
                .familyTree(tree)
                .personalOwner(tree == null ? creator : null)
                .createdBy(creator)
                .title(request.getTitle())
                .description(request.getDescription())
                .eventType(request.getEventType())
                .startDate(startUtc)
                .endDate(endUtc)
                .isFullDay(request.getIsFullDay() != null ? request.getIsFullDay() : false)
                .calendarType(request.getCalendarType() != null ? request.getCalendarType() : Event.CalendarType.solar)
                .isRecurring(request.getIsRecurring() != null ? request.getIsRecurring() : false)
                .recurrenceRule(request.getRecurrenceRule() != null ? request.getRecurrenceRule() : Event.RecurrenceRule.NONE)
                .location(request.getLocation())
                .isPublic(request.getIsPublic() != null ? request.getIsPublic() : true)
                .status(Event.EventStatus.active)
                .createdAt(OffsetDateTime.now(ZoneOffset.UTC))
                .updatedAt(OffsetDateTime.now(ZoneOffset.UTC))
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
        if (request.getStartDate() != null)
            event.setStartDate(request.getStartDate().withOffsetSameInstant(ZoneOffset.UTC));
        if (request.getEndDate() != null)
            event.setEndDate(request.getEndDate().withOffsetSameInstant(ZoneOffset.UTC));
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

        return mapToResponse(eventRepository.save(event));
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
        OffsetDateTime nowUtc = OffsetDateTime.now(ZoneOffset.UTC);
        OffsetDateTime nextMonthUtc = nowUtc.plusMonths(1);

        return eventRepository.findByFamilyTreeAndStartDateBetweenOrderByStartDateAsc(tree, nowUtc, nextMonthUtc)
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public List<EventResponse> getPersonalEvents(UUID userId) {
        User user = loadUserOrThrow(userId);
        return eventRepository.findByCreatedByAndFamilyTreeIsNullOrderByStartDateAsc(user)
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public List<EventResponse> getUpcomingEvents(UUID userId, int limit) {
        User user = loadUserOrThrow(userId);

        ZoneId vietnamZone = ZoneId.of("Asia/Ho_Chi_Minh");
        OffsetDateTime nowVietnam = OffsetDateTime.now(vietnamZone);
        OffsetDateTime nowUtc = nowVietnam.withOffsetSameInstant(ZoneOffset.UTC);
        OffsetDateTime next30Utc = nowVietnam.plusDays(30).withOffsetSameInstant(ZoneOffset.UTC);

        // Event cá nhân
        List<Event> personalEvents = eventRepository.findByPersonalOwnerAndFamilyTreeIsNullAndStatusAndStartDateBetweenOrderByStartDateAsc(
                user, Event.EventStatus.active, nowUtc, next30Utc);

        // Event family tree
        List<FamilyTree> userTrees = familyTreeRepository.findAllByCreatedBy(user);
        List<UUID> treeIds = userTrees.stream().map(FamilyTree::getId).toList();
        List<Event> treeEvents = treeIds.isEmpty() ? List.of() :
                eventRepository.findByFamilyTreeIdInAndStatusAndStartDateBetweenOrderByStartDateAsc(
                        treeIds, Event.EventStatus.active, nowUtc, next30Utc);

        return Stream.concat(personalEvents.stream(), treeEvents.stream())
                .sorted(Comparator.comparing(Event::getStartDate))
                .limit(limit)
                .map(this::mapToResponse)
                .toList();
    }

    @Transactional(readOnly = true)
    public List<EventResponse> getEventsInDateRange(UUID treeId, UUID userId, OffsetDateTime start, OffsetDateTime end) {
        FamilyTree tree = findTreeWithAccessOrThrow(treeId, userId);
        OffsetDateTime startUtc = start.withOffsetSameInstant(ZoneOffset.UTC);
        OffsetDateTime endUtc = end.withOffsetSameInstant(ZoneOffset.UTC);

        return eventRepository.findEventsInDateRange(tree, startUtc, endUtc)
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    private EventResponse mapToResponseWithLunarCheck(Event event) {
        EventResponse response = mapToResponse(event);

        if (event.getCalendarType() == Event.CalendarType.lunar) {
            if (event.getStartDate() != null)
                response.setStartDate(LunarSolarConverter.toSolar(event.getStartDate()));

            if (event.getEndDate() != null)
                response.setEndDate(LunarSolarConverter.toSolar(event.getEndDate()));
        }

        return response;
    }

    private EventResponse mapToResponse(Event event) {
        EventResponse response = new EventResponse();
        ZoneId vietnamZone = ZoneId.of("Asia/Ho_Chi_Minh");

        response.setId(event.getId());
        response.setFamilyTreeId(event.getFamilyTree() != null ? event.getFamilyTree().getId() : null);
        response.setCreatedBy(event.getCreatedBy().getId());
        response.setTitle(event.getTitle());
        response.setDescription(event.getDescription());
        response.setEventType(event.getEventType());
        response.setStartDate(event.getStartDate() != null ? event.getStartDate().atZoneSameInstant(vietnamZone).toOffsetDateTime() : null);
        response.setEndDate(event.getEndDate() != null ? event.getEndDate().atZoneSameInstant(vietnamZone).toOffsetDateTime() : null);
        response.setIsFullDay(event.getIsFullDay());
        response.setCalendarType(event.getCalendarType());
        response.setIsRecurring(event.getIsRecurring());
        response.setRecurrenceRule(event.getRecurrenceRule());
        response.setLocation(event.getLocation());
        response.setIsPublic(event.getIsPublic());
        response.setStatus(event.getStatus());
        response.setCreatedAt(event.getCreatedAt() != null ? event.getCreatedAt().atZoneSameInstant(vietnamZone).toOffsetDateTime() : null);
        response.setUpdatedAt(event.getUpdatedAt() != null ? event.getUpdatedAt().atZoneSameInstant(vietnamZone).toOffsetDateTime() : null);

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
                response.setReminder(objectMapper.convertValue(event.getReminder(), EventResponse.ReminderConfig.class));
            }
        } catch (IllegalArgumentException e) {
            log.error("Error parsing JSON from database for event {}", event.getId(), e);
        }

        return response;
    }

}