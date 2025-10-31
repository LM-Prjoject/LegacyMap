package com.legacymap.backend.controller;

import com.legacymap.backend.dto.request.EventCreateRequest;
import com.legacymap.backend.dto.request.EventUpdateRequest;
import com.legacymap.backend.dto.response.EventResponse;
import com.legacymap.backend.service.EventService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/events")
@RequiredArgsConstructor
public class EventController {

    private final EventService eventService;

    @PostMapping("/family-tree/{familyTreeId}")
    public ResponseEntity<EventResponse> createEvent(
            @PathVariable UUID familyTreeId,
            @RequestHeader("X-User-Id") UUID userId,
            @Valid @RequestBody EventCreateRequest request) {

        EventResponse response = eventService.create(familyTreeId, userId, request);
        return ResponseEntity.ok(response);
    }

    @PutMapping("/{eventId}")
    public ResponseEntity<EventResponse> updateEvent(
            @PathVariable UUID eventId,
            @RequestHeader("X-User-Id") UUID userId,
            @Valid @RequestBody EventUpdateRequest request) {

        EventResponse response = eventService.update(eventId, userId, request);
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{eventId}")
    public ResponseEntity<Void> deleteEvent(
            @PathVariable UUID eventId,
            @RequestHeader("X-User-Id") UUID userId) {

        eventService.delete(eventId, userId);
        return ResponseEntity.ok().build();
    }

    @GetMapping("/{eventId}")
    public ResponseEntity<EventResponse> getEvent(
            @PathVariable UUID eventId,
            @RequestHeader("X-User-Id") UUID userId) {

        EventResponse response = eventService.getById(eventId, userId);
        return ResponseEntity.ok(response);
    }

    @GetMapping("/family-tree/{familyTreeId}")
    public ResponseEntity<List<EventResponse>> getFamilyTreeEvents(
            @PathVariable UUID familyTreeId,
            @RequestHeader("X-User-Id") UUID userId) {

        List<EventResponse> responses = eventService.getByFamilyTree(familyTreeId, userId);
        return ResponseEntity.ok(responses);
    }

    @GetMapping("/upcoming")
    public ResponseEntity<List<EventResponse>> getUpcomingEvents(
            @RequestHeader("X-User-Id") UUID userId,
            @RequestParam(defaultValue = "10") int limit) {

        List<EventResponse> responses = eventService.getUpcomingEvents(userId, limit);
        return ResponseEntity.ok(responses);
    }

    @GetMapping("/family-tree/{familyTreeId}/range")
    public ResponseEntity<List<EventResponse>> getEventsInDateRange(
            @PathVariable UUID familyTreeId,
            @RequestHeader("X-User-Id") UUID userId,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime start,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime end) {

        List<EventResponse> responses = eventService.getEventsInDateRange(familyTreeId, userId, start, end);
        return ResponseEntity.ok(responses);
    }
}