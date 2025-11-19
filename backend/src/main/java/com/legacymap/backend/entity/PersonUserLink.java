package com.legacymap.backend.entity;

import jakarta.persistence.*;
import lombok.*;
import java.time.OffsetDateTime;
import java.util.UUID;

@Entity
@Table(name = "person_user_links")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PersonUserLink {

    @EmbeddedId
    private PersonUserLinkId id;

    @ManyToOne(fetch = FetchType.LAZY)
    @MapsId("personId")
    @JoinColumn(name = "person_id", nullable = false)
    private Person person;

    @ManyToOne(fetch = FetchType.LAZY)
    @MapsId("userId")
    @JoinColumn(name = "user_id", nullable = false)
    private User user;

    @Enumerated(EnumType.STRING)
    @Column(name = "link_type", length = 20, nullable = false)
    private LinkType linkType = LinkType.self;

    @Column(name = "verified", nullable = false)
    private boolean verified = false;

    @Column(name = "linked_at")
    private OffsetDateTime linkedAt = OffsetDateTime.now();

    @Column(name = "verified_at")
    private OffsetDateTime verifiedAt;

    public enum LinkType {
        self, relative, manager
    }
}
