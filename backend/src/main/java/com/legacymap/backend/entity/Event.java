package com.legacymap.backend.entity;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "events")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Event {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "family_tree_id", nullable = false)
    private FamilyTree familyTree;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "created_by", nullable = false)
    private User createdBy;

    @Column(nullable = false, length = 200)
    private String title;

    @Column(columnDefinition = "TEXT")
    private String description;

    @Enumerated(EnumType.STRING)
    @Column(name = "event_type", nullable = false, length = 50)
    private EventType eventType;

    @Column(name = "start_date", nullable = false)
    private LocalDateTime startDate;

    @Column(name = "end_date")
    private LocalDateTime endDate;

    @Column(name = "is_full_day")
    private Boolean isFullDay = false;

    @Enumerated(EnumType.STRING)
    @Column(name = "calendar_type", length = 10)
    private CalendarType calendarType = CalendarType.SOLAR;

    @Column(name = "is_recurring")
    private Boolean isRecurring = false;

    @Enumerated(EnumType.STRING)
    @Column(name = "recurrence_rule", length = 20)
    private RecurrenceRule recurrenceRule = RecurrenceRule.NONE;

    @Column(name = "related_persons", columnDefinition = "jsonb")
    private String relatedPersons;

    @Column(length = 300)
    private String location;

    @Column(name = "location_coordinates", columnDefinition = "jsonb")
    private String locationCoordinates;

    @Column(columnDefinition = "jsonb")
    private String reminder = "{\"days_before\": 3, \"methods\": [\"notification\"]}";

    @Column(name = "is_public")
    private Boolean isPublic = true;

    @Enumerated(EnumType.STRING)
    @Column(length = 20)
    private EventStatus status = EventStatus.ACTIVE;

    @CreationTimestamp
    @Column(name = "created_at")
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    public enum EventType {
        DEATH_ANNIVERSARY, WEDDING_ANNIVERSARY, BIRTHDAY,
        FUNERAL, WEDDING, FAMILY_REUNION, CEREMONY, OTHER
    }

    public enum CalendarType {
        SOLAR, LUNAR
    }

    public enum RecurrenceRule {
        YEARLY, MONTHLY, NONE
    }

    public enum EventStatus {
        ACTIVE, CANCELLED, COMPLETED
    }
}
