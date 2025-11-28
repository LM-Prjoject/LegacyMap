package com.legacymap.backend.repository;

import com.legacymap.backend.entity.Event;
import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

@Repository
public interface EventRepository extends JpaRepository<Event, UUID> {

    List<Event> findByFamilyTreeAndStartDateBetweenOrderByStartDateAsc(
            FamilyTree familyTree, OffsetDateTime start, OffsetDateTime end);

    @Query("SELECT e FROM Event e WHERE e.familyTree = :familyTree " +
            "AND (e.startDate BETWEEN :startDate AND :endDate " +
            "OR e.endDate BETWEEN :startDate AND :endDate " +
            "OR (e.startDate <= :startDate AND e.endDate >= :endDate)) " +
            "ORDER BY e.startDate ASC")
    List<Event> findEventsInDateRange(
            @Param("familyTree") FamilyTree familyTree,
            @Param("startDate") OffsetDateTime startDate,
            @Param("endDate") OffsetDateTime endDate);

    @Query("SELECT e FROM Event e WHERE e.isRecurring = true AND e.startDate BETWEEN :start AND :end")
    List<Event> findRecurringEventsInRange(@Param("start") OffsetDateTime start, @Param("end") OffsetDateTime end);

    List<Event> findByCreatedByAndFamilyTreeIsNullOrderByStartDateAsc(User createdBy);

    // Event cá nhân
    List<Event> findByPersonalOwnerAndFamilyTreeIsNullAndStatusAndStartDateBetweenOrderByStartDateAsc(
            User personalOwner,
            Event.EventStatus status,
            OffsetDateTime start,
            OffsetDateTime end
    );

    // Event theo family tree
    List<Event> findByFamilyTreeIdInAndStatusAndStartDateBetweenOrderByStartDateAsc(
            List<UUID> familyTreeIds,
            Event.EventStatus status,
            OffsetDateTime start,
            OffsetDateTime end
    );

    List<Event> findByFamilyTreeAndStatusAndStartDateBetweenOrderByStartDateAsc(
         FamilyTree familyTree,
         Event.EventStatus status,
         OffsetDateTime start,
         OffsetDateTime end
    );
}
