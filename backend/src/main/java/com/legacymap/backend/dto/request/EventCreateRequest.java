package com.legacymap.backend.dto.request;

import com.legacymap.backend.entity.Event;
import lombok.Data;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
public class EventCreateRequest {
    private String title;
    private String description;
    private Event.EventType eventType;
    private OffsetDateTime startDate;
    private OffsetDateTime endDate;
    private Boolean isFullDay = false;
    private Event.CalendarType calendarType = Event.CalendarType.solar;
    private Boolean isRecurring = false;
    private Event.RecurrenceRule recurrenceRule = Event.RecurrenceRule.NONE;
    private List<RelatedPerson> relatedPersons;
    private String location;
    private Map<String, Object> locationCoordinates;
    private ReminderConfig reminder;
    private UUID personalOwnerId;

    @Data
    public static class RelatedPerson {
        private UUID id;
        private String name;
    }

    @Data
    public static class ReminderConfig {
        private Integer daysBefore = 3;
        private List<String> methods = List.of("notification");
    }
}
