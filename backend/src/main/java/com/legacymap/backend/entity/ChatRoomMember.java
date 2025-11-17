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
@Table(name = "chat_room_members")
public class ChatRoomMember {

    @EmbeddedId
    private ChatRoomMemberId id;

    @ManyToOne(fetch = FetchType.LAZY)
    @MapsId("roomId")
    @JoinColumn(name = "room_id")
    private ChatRoom room;

    @ManyToOne(fetch = FetchType.LAZY)
    @MapsId("userId")
    @JoinColumn(name = "user_id")
    private User user;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "person_id")
    private Person person;

    @Enumerated(EnumType.STRING)
    @Column(length = 20, nullable = false)
    private ChatMemberRole role = ChatMemberRole.member;

    @Column(name = "joined_at", nullable = false)
    private OffsetDateTime joinedAt;

    @Column(name = "last_read_at")
    private OffsetDateTime lastReadAt;

    @PrePersist
    void prePersist() {
        if (joinedAt == null) {
            joinedAt = OffsetDateTime.now();
        }
    }

    public enum ChatMemberRole {
        member, admin, moderator
    }
}

