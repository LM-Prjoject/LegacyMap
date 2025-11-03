package com.legacymap.backend.repository;

import com.legacymap.backend.entity.Event;
import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Repository
public interface EventRepository extends JpaRepository<Event, UUID> {

    List<Event> findByPersonalOwnerOrderByStartDateAsc(User personalOwner);

    List<Event> findByFamilyTreeAndStartDateBetweenOrderByStartDateAsc(FamilyTree familyTree, LocalDateTime start, LocalDateTime end);

    @Query("SELECT e FROM Event e WHERE e.familyTree = :familyTree " +
            "AND (e.startDate BETWEEN :startDate AND :endDate " +
            "OR e.endDate BETWEEN :startDate AND :endDate " +
            "OR (e.startDate <= :startDate AND e.endDate >= :endDate)) " +
            "ORDER BY e.startDate ASC")
    List<Event> findEventsInDateRange(
            @Param("familyTree") FamilyTree familyTree,
            @Param("startDate") LocalDateTime startDate,
            @Param("endDate") LocalDateTime endDate);

    @Query("SELECT e FROM Event e WHERE e.familyTree.id IN :familyTreeIds " +
            "AND e.status = 'ACTIVE' " +
            "AND e.startDate >= :startDate " +
            "ORDER BY e.startDate ASC")
    List<Event> findUpcomingEvents(@Param("familyTreeIds") List<UUID> familyTreeIds,
                                   @Param("startDate") LocalDateTime startDate,
                                   org.springframework.data.domain.Pageable pageable);

    @Query("SELECT e FROM Event e WHERE e.isRecurring = true AND e.startDate BETWEEN :start AND :end")
    List<Event> findRecurringEventsInRange(@Param("start") LocalDateTime start, @Param("end") LocalDateTime end);

    List<Event> findByCreatedByAndFamilyTreeIsNullOrderByStartDateAsc(User createdBy);
}
