package com.legacymap.backend.controller;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.legacymap.backend.dto.request.EventCreateRequest;
import com.legacymap.backend.dto.request.EventUpdateRequest;
import com.legacymap.backend.dto.response.EventResponse;
import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.service.EventService;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@RestController
@RequestMapping("/api/events")
@RequiredArgsConstructor
@Slf4j
public class EventController {

    private final EventService eventService;
    private final UserRepository userRepository;

    private UUID getCurrentUserId() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null || !authentication.isAuthenticated()) {
            throw new RuntimeException("User is not authenticated");
        }

        Object principal = authentication.getPrincipal();

        if (principal instanceof String str) {
            try {
                return UUID.fromString(str);
            } catch (IllegalArgumentException ignored) {
            }
        }

        String email = authentication.getName();
        return userRepository.findByEmail(email)
                .map(u -> u.getId())
                .orElseThrow(() -> new RuntimeException("User not found for email: " + email));
    }

    @PostMapping("/family-tree/{familyTreeId}")
    public ResponseEntity<EventResponse> createEvent(
            @PathVariable UUID familyTreeId,
            @Valid @RequestBody EventCreateRequest request) {
        return ResponseEntity.ok(eventService.create(familyTreeId, getCurrentUserId(), request));
    }

    @PostMapping("/personal")
    public ResponseEntity<EventResponse> createPersonalEvent(
            @Valid @RequestBody EventCreateRequest request) {
        return ResponseEntity.ok(eventService.create(null, getCurrentUserId(), request));
    }

    @PutMapping("/{eventId}")
    public ResponseEntity<EventResponse> updateEvent(
            @PathVariable UUID eventId,
            @Valid @RequestBody EventUpdateRequest request) {
        return ResponseEntity.ok(eventService.update(eventId, getCurrentUserId(), request));
    }

    @DeleteMapping("/{eventId}")
    public ResponseEntity<Void> deleteEvent(@PathVariable UUID eventId) {
        eventService.delete(eventId, getCurrentUserId());
        return ResponseEntity.ok().build();
    }

    @GetMapping("/{eventId}")
    public ResponseEntity<EventResponse> getEvent(@PathVariable UUID eventId) {
        return ResponseEntity.ok(eventService.getById(eventId, getCurrentUserId()));
    }

    @GetMapping("/family-tree/{familyTreeId}")
    public ResponseEntity<List<EventResponse>> getFamilyTreeEvents(@PathVariable UUID familyTreeId) {
        return ResponseEntity.ok(eventService.getByFamilyTree(familyTreeId, getCurrentUserId()));
    }

    @GetMapping("/personal")
    public ResponseEntity<List<EventResponse>> getPersonalEvents() {
        return ResponseEntity.ok(eventService.getPersonalEvents(getCurrentUserId()));
    }

    @GetMapping("/upcoming")
    public ResponseEntity<List<EventResponse>> getUpcomingEvents(
            @RequestParam(defaultValue = "10") int limit) {
        return ResponseEntity.ok(eventService.getUpcomingEvents(getCurrentUserId(), limit));
    }

    @GetMapping("/range")
    public ResponseEntity<List<EventResponse>> getEventsInDateRange(
            @RequestParam(required = false) UUID familyTreeId,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) OffsetDateTime start,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) OffsetDateTime end) {
        return ResponseEntity.ok(eventService.getEventsInDateRange(familyTreeId, getCurrentUserId(), start, end));
    }
}
