package com.legacymap.backend.entity;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import java.time.OffsetDateTime;
import java.util.UUID;

@Entity
@Table(name = "event_reminders")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class EventReminder {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "event_id", nullable = false)
    private Event event;

    @Enumerated(EnumType.STRING)
    @Column(name = "recipient_type", nullable = false, length = 10)
    private RecipientType recipientType;

    @Column(name = "recipient_id", nullable = false)
    private UUID recipientId;

    @Enumerated(EnumType.STRING)
    @Column(name = "send_method", nullable = false, length = 20)
    private SendMethod sendMethod;

    @Column(name = "scheduled_at", nullable = false)
    private OffsetDateTime scheduledAt;

    @Column(name = "sent_at")
    private OffsetDateTime sentAt;

    @Enumerated(EnumType.STRING)
    @Column(length = 20)
    private ReminderStatus status = ReminderStatus.pending;

    @CreationTimestamp
    @Column(name = "created_at")
    private OffsetDateTime createdAt;

    public enum SendMethod {
        notification, email, both
    }

    public enum ReminderStatus {
        pending, sent, failed
    }

    public enum RecipientType {
        user, person
    }
}