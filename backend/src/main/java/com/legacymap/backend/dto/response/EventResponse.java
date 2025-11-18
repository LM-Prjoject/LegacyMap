package com.legacymap.backend.dto.response;

import com.legacymap.backend.entity.Event;
import lombok.Data;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
public class EventResponse {
    private UUID id;
    private UUID familyTreeId;
    private UUID createdBy;
    private UUID personalOwnerId;
    private String title;
    private String description;
    private Event.EventType eventType;
    private OffsetDateTime startDate;
    private OffsetDateTime endDate;
    private Boolean isFullDay;
    private Event.CalendarType calendarType;
    private Boolean isRecurring;
    private Event.RecurrenceRule recurrenceRule;
    private List<RelatedPerson> relatedPersons;
    private String location;
    private Map<String, Object> locationCoordinates;
    private ReminderConfig reminder;
    private Event.EventStatus status;
    private OffsetDateTime createdAt;
    private OffsetDateTime updatedAt;

    @Data
    public static class RelatedPerson {
        private UUID id;
        private String name;
    }

    @Data
    public static class ReminderConfig {
        private Integer daysBefore;
        private List<String> methods;
    }
}
