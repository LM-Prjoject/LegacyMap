package com.legacymap.backend.dto.response;

import com.legacymap.backend.entity.PersonUserLink;
import lombok.Builder;
import lombok.Data;

import java.time.OffsetDateTime;
import java.util.UUID;

@Data
@Builder
public class PersonUserLinkResponse {
    private UUID personId;
    private UUID userId;
    private PersonUserLink.LinkType linkType;
    private PersonUserLink.Status status;
    private OffsetDateTime linkedAt;
    private OffsetDateTime verifiedAt;
}
