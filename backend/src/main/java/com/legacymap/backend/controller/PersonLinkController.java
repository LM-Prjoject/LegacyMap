package com.legacymap.backend.controller;

import com.legacymap.backend.dto.request.PersonLinkInviteRequest;
import com.legacymap.backend.dto.response.ApiResponse;
import com.legacymap.backend.dto.response.PersonClaimResponse;
import com.legacymap.backend.service.PersonLinkService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/persons")
public class PersonLinkController {

    @Autowired
    private PersonLinkService personLinkService;

    private UUID parseUUID(String id) {
        return UUID.fromString(id);
    }

    @PostMapping("/{personId}/invite")
    public ResponseEntity<ApiResponse<com.legacymap.backend.dto.response.PersonLinkInviteResponse>> invite(
            @PathVariable("personId") UUID personId,
            @RequestParam("userId") String inviterId,
            @RequestBody @Valid PersonLinkInviteRequest req
    ) {
        com.legacymap.backend.dto.response.PersonLinkInviteResponse res = personLinkService.inviteByEmail(parseUUID(inviterId), personId, req);
        return ResponseEntity.ok(ApiResponse.success(res));
    }

    @PostMapping("/{personId}/claims/accept")
    public ResponseEntity<ApiResponse<Void>> accept(
            @PathVariable("personId") UUID personId,
            @RequestParam("userId") String currentUserId
    ) {
        personLinkService.acceptClaim(parseUUID(currentUserId), personId);
        return ResponseEntity.ok(ApiResponse.success());
    }

    @PostMapping("/{personId}/claims/reject")
    public ResponseEntity<ApiResponse<Void>> reject(
            @PathVariable("personId") UUID personId,
            @RequestParam("userId") String currentUserId
    ) {
        personLinkService.rejectClaim(parseUUID(currentUserId), personId);
        return ResponseEntity.ok(ApiResponse.success());
    }

    @DeleteMapping("/{personId}/links/{userId}")
    public ResponseEntity<ApiResponse<Void>> unlink(
            @PathVariable("personId") UUID personId,
            @PathVariable("userId") UUID userId,
            @RequestParam("userId") String requesterId
    ) {
        personLinkService.unlink(parseUUID(requesterId), personId, userId);
        return ResponseEntity.ok(ApiResponse.success());
    }

    @DeleteMapping("/{personId}/links/self")
    public ResponseEntity<ApiResponse<Void>> unlinkSelf(
            @PathVariable("personId") UUID personId,
            @RequestParam("userId") String requesterId
    ) {
        personLinkService.unlinkSelf(parseUUID(requesterId), personId);
        return ResponseEntity.ok(ApiResponse.success());
    }

    @GetMapping("/me/claims")
    public ResponseEntity<ApiResponse<List<PersonClaimResponse>>> myPendingClaims(
            @RequestParam("userId") String userId
    ) {
        List<PersonClaimResponse> claims = personLinkService.listPendingClaims(parseUUID(userId));
        return ResponseEntity.ok(ApiResponse.success(claims));
    }

    @GetMapping("/{personId}/links/self/verified")
    public ResponseEntity<ApiResponse<Boolean>> isSelfVerified(
            @PathVariable("personId") UUID personId
    ) {
        boolean verified = personLinkService.isSelfVerified(personId);
        return ResponseEntity.ok(ApiResponse.success(verified));
    }
}
