package com.legacymap.backend.entity;

import com.fasterxml.jackson.databind.JsonNode;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.ColumnTransformer;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

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
    @JoinColumn(name = "family_tree_id")
    private FamilyTree familyTree;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "created_by", nullable = false)
    private User createdBy;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "personal_owner_id")
    private User personalOwner;

    @Column(nullable = false, length = 200)
    private String title;

    @Column(columnDefinition = "TEXT")
    private String description;

    @Enumerated(EnumType.STRING)
    @Column(name = "event_type", nullable = false, length = 50)
    @ColumnTransformer(read = "lower(event_type)", write = "lower(?)")
    private EventType eventType;

    @Column(name = "start_date", nullable = false)
    private LocalDateTime startDate;

    @Column(name = "end_date")
    private LocalDateTime endDate;

    @Column(name = "is_full_day")
    private Boolean isFullDay = false;

    @Enumerated(EnumType.STRING)
    @Column(name = "calendar_type", length = 10)
    @ColumnTransformer(read = "lower(calendar_type)", write = "lower(?)")
    private CalendarType calendarType = CalendarType.SOLAR;

    @Column(name = "is_recurring")
    private Boolean isRecurring = false;

    @Enumerated(EnumType.STRING)
    @Column(name = "recurrence_rule", length = 20)
    private RecurrenceRule recurrenceRule = RecurrenceRule.NONE;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "related_persons", columnDefinition = "jsonb")
    private JsonNode relatedPersons;

    @Column(length = 300)
    private String location;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "location_coordinates", columnDefinition = "jsonb")
    private JsonNode locationCoordinates;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "reminder", columnDefinition = "jsonb")
    private JsonNode reminder;

    @Column(name = "is_public")
    private Boolean isPublic = true;

    @Enumerated(EnumType.STRING)
    @Column(length = 20)
    @ColumnTransformer(write = "lower(?)")
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
