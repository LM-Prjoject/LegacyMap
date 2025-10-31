package com.legacymap.backend.dto.response;

import com.legacymap.backend.entity.Event;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
public class EventResponse {
    private UUID id;
    private UUID familyTreeId;
    private UUID createdBy;
    private String title;
    private String description;
    private Event.EventType eventType;
    private LocalDateTime startDate;
    private LocalDateTime endDate;
    private Boolean isFullDay;
    private Event.CalendarType calendarType;
    private Boolean isRecurring;
    private Event.RecurrenceRule recurrenceRule;
    private List<RelatedPerson> relatedPersons;
    private String location;
    private Map<String, Object> locationCoordinates;
    private ReminderConfig reminder;
    private Boolean isPublic;
    private Event.EventStatus status;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

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
