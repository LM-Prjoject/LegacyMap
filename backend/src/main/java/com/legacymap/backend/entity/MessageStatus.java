package com.legacymap.backend.entity;

import jakarta.persistence.*;
import lombok.*;

import java.time.OffsetDateTime;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(name = "message_status")
public class MessageStatus {

    @EmbeddedId
    private MessageStatusId id;

    @ManyToOne(fetch = FetchType.LAZY)
    @MapsId("messageId")
    @JoinColumn(name = "message_id")
    private ChatMessage message;

    @ManyToOne(fetch = FetchType.LAZY)
    @MapsId("userId")
    @JoinColumn(name = "user_id")
    private User user;

    @Column(name = "is_read", nullable = false)
    private Boolean read = Boolean.FALSE;

    @Column(name = "read_at")
    private OffsetDateTime readAt;

    public void markAsRead() {
        if (!Boolean.TRUE.equals(read)) {
            read = Boolean.TRUE;
            readAt = OffsetDateTime.now();
        }
    }
}

