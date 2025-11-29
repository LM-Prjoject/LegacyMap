package com.legacymap.backend.service;

import java.time.*;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.legacymap.backend.entity.LunarDate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.databind.ObjectMapper;
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

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@RequiredArgsConstructor
public class EventService {

    private final EventRepository eventRepository;
    private final FamilyTreeRepository familyTreeRepository;
    private final UserRepository userRepository;
    private final EventReminderService eventReminderService;
    private final LunarCalendarService lunarCalendarService;
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
                .status(Event.EventStatus.active)
                .createdAt(OffsetDateTime.now(ZoneOffset.UTC))
                .updatedAt(OffsetDateTime.now(ZoneOffset.UTC))
                .build();

        EventCreateRequest.ReminderConfig reminderConfig = request.getReminder();
        if (reminderConfig == null) {
            reminderConfig = new EventCreateRequest.ReminderConfig();
        }
        event.setReminder(objectMapper.valueToTree(reminderConfig));

        if (request.getRelatedPersons() != null) {
            event.setRelatedPersons(objectMapper.valueToTree(request.getRelatedPersons()));
        }
        if (request.getLocationCoordinates() != null) {
            event.setLocationCoordinates(objectMapper.valueToTree(request.getLocationCoordinates()));
        }

        Event savedEvent = eventRepository.save(event);

        eventReminderService.createRemindersForEvent(savedEvent);

        return mapToResponse(savedEvent);
    }

    @Transactional
    public EventResponse update(UUID eventId, UUID userId, EventUpdateRequest request) {
        Event event = eventRepository.findById(eventId)
                .orElseThrow(() -> new AppException(ErrorCode.EVENT_NOT_FOUND));

        if (event.getCreatedBy() == null || !event.getCreatedBy().getId().equals(userId)) {
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
                eventRepository.save(event);
                eventReminderService.updateRemindersForEvent(event);
            } else if (event.getReminder() == null) {
                event.setReminder(objectMapper.valueToTree(new EventCreateRequest.ReminderConfig()));
            }
        } catch (IllegalArgumentException e) {
            log.error("Error parsing JSON data", e);
            throw new AppException(ErrorCode.INVALID_INPUT_DATA);
        }

        event.setUpdatedAt(OffsetDateTime.now(ZoneOffset.UTC));
        return mapToResponse(eventRepository.save(event));
    }

    @Transactional
    public void delete(UUID eventId, UUID userId) {
        Event event = eventRepository.findById(eventId)
                .orElseThrow(() -> new AppException(ErrorCode.EVENT_NOT_FOUND));

        if (event.getCreatedBy() == null || !event.getCreatedBy().getId().equals(userId)) {
            throw new AppException(ErrorCode.UNAUTHORIZED);
        }

        eventRepository.delete(event);
    }

    @Transactional(readOnly = true)
    public EventResponse getById(UUID eventId, UUID userId) {
        Event event = eventRepository.findById(eventId)
                .orElseThrow(() -> new AppException(ErrorCode.EVENT_NOT_FOUND));

        boolean isCreator = event.getCreatedBy() != null && event.getCreatedBy().getId().equals(userId);
        boolean hasTreeAccess = false;
        if (event.getFamilyTree() != null) {
            hasTreeAccess = familyTreeRepository.findByIdAndCreatedBy(event.getFamilyTree().getId(), loadUserOrThrow(userId))
                    .isPresent();
        }

        if (!isCreator && !hasTreeAccess) {
            throw new AppException(ErrorCode.UNAUTHORIZED);
        }

        return mapToResponse(event);
    }

    @Transactional(readOnly = true)
    public List<EventResponse> getByFamilyTree(UUID treeId, UUID userId) {
        FamilyTree tree = findTreeWithAccessOrThrow(treeId, userId);
        OffsetDateTime nowUtc = OffsetDateTime.now(ZoneOffset.UTC);
        OffsetDateTime nextMonthUtc = nowUtc.plusMonths(1);
        OffsetDateTime searchStart = nowUtc.minusYears(5);
        OffsetDateTime searchEnd = nowUtc.plusMonths(1);

        List<Event> treeEvents = eventRepository.findByFamilyTreeAndStatusAndStartDateBetweenOrderByStartDateAsc(
                tree, Event.EventStatus.active, searchStart, searchEnd);

        return treeEvents.stream()
                .flatMap(e -> expandRecurrence(e, nowUtc, nextMonthUtc).stream())
                .sorted(Comparator.comparing(EventResponse::getStartDate))
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
        ZonedDateTime nowVietnamZdt = ZonedDateTime.now(vietnamZone);
        OffsetDateTime nowUtc = nowVietnamZdt.toOffsetDateTime().withOffsetSameInstant(ZoneOffset.UTC);
        OffsetDateTime next30Utc = nowVietnamZdt.plusDays(30).toOffsetDateTime().withOffsetSameInstant(ZoneOffset.UTC);

        OffsetDateTime searchStart = nowUtc.minusYears(2);
        OffsetDateTime searchEnd = next30Utc.plusYears(5);

        // Personal events
        List<Event> personalEvents = eventRepository.findByPersonalOwnerAndFamilyTreeIsNullAndStatusAndStartDateBetweenOrderByStartDateAsc(
                user, Event.EventStatus.active, searchStart, searchEnd);

        // Family tree events
        List<FamilyTree> userTrees = familyTreeRepository.findAllByCreatedBy(user);
        List<UUID> treeIds = userTrees.stream().map(FamilyTree::getId).collect(Collectors.toList());
        List<Event> treeEvents = treeIds.isEmpty() ? List.of() :
                eventRepository.findByFamilyTreeIdInAndStatusAndStartDateBetweenOrderByStartDateAsc(
                        treeIds, Event.EventStatus.active, searchStart, searchEnd);

        List<EventResponse> allOccurrences = Stream.concat(personalEvents.stream(), treeEvents.stream())
                .flatMap(e -> expandRecurrence(e, nowUtc, next30Utc).stream())
                .collect(Collectors.toList());

        return allOccurrences.stream()
                .sorted(Comparator.comparing(EventResponse::getStartDate))
                .limit(limit)
                .collect(Collectors.toList());
    }

    private List<EventResponse> expandRecurrence(Event event, OffsetDateTime rangeStart, OffsetDateTime rangeEnd) {
        if (event == null) return List.of();

        if (!event.getIsRecurring() || event.getRecurrenceRule() == Event.RecurrenceRule.NONE) {
            if (isEventVisibleInRange(event, rangeStart, rangeEnd)) {
                return List.of(mapToResponse(event));
            }
            return List.of();
        }

        if (event.getStartDate() == null) {
            return List.of();
        }

        if (event.getCalendarType() == Event.CalendarType.lunar) {
            return expandLunarRecurrence(event, rangeStart, rangeEnd);
        }

        return expandSolarRecurrence(event, rangeStart, rangeEnd);
    }

    private List<EventResponse> expandLunarRecurrence(Event event, OffsetDateTime rangeStart, OffsetDateTime rangeEnd) {
        List<EventResponse> occurrences = new ArrayList<>();

        try {
            LunarDate originalLunarDate = lunarCalendarService.solarToLunar(
                    event.getStartDate().atZoneSameInstant(ZoneId.of("Asia/Ho_Chi_Minh")).toLocalDate()
            );

            int startYear = rangeStart.getYear() - 1;
            int endYear = rangeEnd.getYear() + 1;

            for (int year = startYear; year <= endYear; year++) {
                try {
                    LocalDate solarDate = lunarCalendarService.lunarToSolar(
                            new LunarDate(originalLunarDate.getDay(), originalLunarDate.getMonth(), year, originalLunarDate.isLeapMonth())
                    );

                    OffsetDateTime occurrenceStart = solarDate.atStartOfDay()
                            .atZone(ZoneId.of("Asia/Ho_Chi_Minh"))
                            .toOffsetDateTime()
                            .withOffsetSameInstant(ZoneOffset.UTC);

                    OffsetDateTime occurrenceEnd = occurrenceStart.plusSeconds(
                            event.getEndDate() != null
                                    ? Duration.between(event.getStartDate(), event.getEndDate()).getSeconds()
                                    : 3600
                    );

                    if (!occurrenceStart.isAfter(rangeEnd) && !occurrenceEnd.isBefore(rangeStart)) {
                        EventResponse resp = mapToResponse(event);
                        resp.setStartDate(occurrenceStart.atZoneSameInstant(ZoneId.of("Asia/Ho_Chi_Minh")).toOffsetDateTime());
                        resp.setEndDate(occurrenceEnd.atZoneSameInstant(ZoneId.of("Asia/Ho_Chi_Minh")).toOffsetDateTime());
                        resp.setLunarDate(getLunarDateString(occurrenceStart));
                        occurrences.add(resp);
                    }
                } catch (Exception e) {
                    log.warn("Unable to convert lunar date for year {}: {}", year, e.getMessage());
                }
            }
        } catch (Exception e) {
            return expandSolarRecurrence(event, rangeStart, rangeEnd);
        }

        return occurrences;
    }

    private List<EventResponse> expandSolarRecurrence(Event event, OffsetDateTime rangeStart, OffsetDateTime rangeEnd) {
        List<EventResponse> occurrences = new ArrayList<>();

        OffsetDateTime current = event.getStartDate();
        ZoneId vietnamZone = ZoneId.of("Asia/Ho_Chi_Minh");

        long durationSeconds = event.getEndDate() != null && event.getStartDate() != null
                ? Duration.between(event.getStartDate(), event.getEndDate()).getSeconds()
                : 3600;

        while (current.isBefore(rangeStart)) {
            if (event.getRecurrenceRule() == Event.RecurrenceRule.YEARLY) {
                current = current.plusYears(1);
            } else if (event.getRecurrenceRule() == Event.RecurrenceRule.MONTHLY) {
                current = current.plusMonths(1);
            } else {
                break;
            }
            if (current.isAfter(rangeEnd.plusYears(100))) {
                break;
            }
        }

        while (!current.isAfter(rangeEnd)) {
            OffsetDateTime occurrenceStartUtc = current;
            OffsetDateTime occurrenceEndUtc = current.plusSeconds(durationSeconds);

            if (!occurrenceStartUtc.isAfter(rangeEnd) && !occurrenceEndUtc.isBefore(rangeStart)) {
                EventResponse resp = mapToResponse(event);
                resp.setStartDate(occurrenceStartUtc.atZoneSameInstant(vietnamZone).toOffsetDateTime());
                resp.setEndDate(occurrenceEndUtc.atZoneSameInstant(vietnamZone).toOffsetDateTime());

                if (event.getCalendarType() == Event.CalendarType.lunar) {
                    resp.setLunarDate(getLunarDateString(occurrenceStartUtc));
                }

                occurrences.add(resp);
            }

            if (event.getRecurrenceRule() == Event.RecurrenceRule.YEARLY) {
                current = current.plusYears(1);
            } else if (event.getRecurrenceRule() == Event.RecurrenceRule.MONTHLY) {
                current = current.plusMonths(1);
            } else {
                break;
            }

            if (current.isAfter(rangeEnd.plusYears(20))) {
                break;
            }
        }

        return occurrences;
    }

    private String getLunarDateString(OffsetDateTime dateTime) {
        try {
            LocalDate solarDate = dateTime.atZoneSameInstant(ZoneId.of("Asia/Ho_Chi_Minh")).toLocalDate();
            com.legacymap.backend.entity.LunarDate lunarDate = lunarCalendarService.solarToLunar(solarDate);

            return String.format("%d/%d/%d %s",
                    lunarDate.getDay(), lunarDate.getMonth(), lunarDate.getYear(),
                    lunarDate.isLeapMonth() ? "(tháng nhuận)" : "");
        } catch (Exception e) {
            log.warn("Không thể chuyển đổi sang âm lịch cho ngày {}: {}", dateTime, e.getMessage());
            return "";
        }
    }

    @Transactional(readOnly = true)
    public List<EventResponse> getEventsInDateRange(UUID treeId, UUID userId, OffsetDateTime start, OffsetDateTime end) {
        User user = loadUserOrThrow(userId);
        OffsetDateTime startUtc = start.withOffsetSameInstant(ZoneOffset.UTC);
        OffsetDateTime endUtc = end.withOffsetSameInstant(ZoneOffset.UTC);
        OffsetDateTime searchStart = startUtc.minusYears(5);

        List<Event> events = new ArrayList<>();

        if (treeId != null) {
            FamilyTree tree = findTreeWithAccessOrThrow(treeId, userId);
            events.addAll(eventRepository.findByFamilyTreeAndStatusAndStartDateBetweenOrderByStartDateAsc(
                    tree, Event.EventStatus.active, searchStart, endUtc));
        } else {
            events.addAll(eventRepository.findByPersonalOwnerAndFamilyTreeIsNullAndStatusAndStartDateBetweenOrderByStartDateAsc(
                    user, Event.EventStatus.active, searchStart, endUtc));

            List<FamilyTree> userTrees = familyTreeRepository.findAllByCreatedBy(user);
            if (!userTrees.isEmpty()) {
                List<UUID> treeIds = userTrees.stream().map(FamilyTree::getId).collect(Collectors.toList());
                events.addAll(eventRepository.findByFamilyTreeIdInAndStatusAndStartDateBetweenOrderByStartDateAsc(
                        treeIds, Event.EventStatus.active, searchStart, endUtc));
            }
        }

        return events.stream()
                .flatMap(e -> expandRecurrence(e, startUtc, endUtc).stream())
                .sorted(Comparator.comparing(EventResponse::getStartDate))
                .collect(Collectors.toList());
    }

    private boolean isEventVisibleInRange(Event event, OffsetDateTime rangeStart, OffsetDateTime rangeEnd) {
        if (event == null) return false;

        if (!event.getIsRecurring() || event.getRecurrenceRule() == Event.RecurrenceRule.NONE) {
            OffsetDateTime eventEnd = event.getEndDate() != null ? event.getEndDate() : (event.getStartDate() != null ? event.getStartDate().plusHours(1) : null);
            if (event.getStartDate() == null || eventEnd == null) return false;
            return !event.getStartDate().isAfter(rangeEnd) && !eventEnd.isBefore(rangeStart);
        }

        OffsetDateTime current = event.getStartDate();
        OffsetDateTime searchUntil = rangeEnd.plusYears(10);

        long durationSeconds = 3600;
        if (event.getEndDate() != null && event.getStartDate() != null) {
            durationSeconds = Duration.between(event.getStartDate(), event.getEndDate()).getSeconds();
        }

        while (current != null && current.isBefore(searchUntil)) {
            OffsetDateTime currentEnd = current.plusSeconds(durationSeconds);

            if (!current.isAfter(rangeEnd) && !currentEnd.isBefore(rangeStart)) {
                return true;
            }

            if (event.getRecurrenceRule() == Event.RecurrenceRule.YEARLY) {
                current = current.plusYears(1);
            } else if (event.getRecurrenceRule() == Event.RecurrenceRule.MONTHLY) {
                current = current.plusMonths(1);
            } else {
                break;
            }
        }
        return false;
    }

    private EventResponse mapToResponse(Event event) {
        EventResponse response = new EventResponse();
        ZoneId vietnamZone = ZoneId.of("Asia/Ho_Chi_Minh");

        response.setId(event.getId());
        response.setFamilyTreeId(event.getFamilyTree() != null ? event.getFamilyTree().getId() : null);
        response.setCreatedBy(event.getCreatedBy() != null ? event.getCreatedBy().getId() : null);
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
