package com.legacymap.backend.dto.request;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.*;

import java.util.UUID;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BranchRoomCreateRequest {
    @NotNull
    private UUID branchPersonId;
    
    @NotBlank
    @Size(max = 200)
    private String name;
    
    @Size(max = 1000)
    private String description;
}