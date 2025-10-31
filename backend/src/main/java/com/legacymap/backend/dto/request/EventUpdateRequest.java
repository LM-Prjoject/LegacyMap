package com.legacymap.backend.dto.request;

import com.legacymap.backend.entity.Event;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

@Data
public class EventUpdateRequest {
    private String title;
    private String description;
    private Event.EventType eventType;
    private LocalDateTime startDate;
    private LocalDateTime endDate;
    private Boolean isFullDay;
    private Event.CalendarType calendarType;
    private Boolean isRecurring;
    private Event.RecurrenceRule recurrenceRule;
    private List<EventCreateRequest.RelatedPerson> relatedPersons;
    private String location;
    private Map<String, Object> locationCoordinates;
    private EventCreateRequest.ReminderConfig reminder;
    private Boolean isPublic;
    private Event.EventStatus status;
}
