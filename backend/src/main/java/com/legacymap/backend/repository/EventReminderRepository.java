package com.legacymap.backend.repository;

import com.legacymap.backend.entity.EventReminder;
import com.legacymap.backend.entity.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Repository
public interface EventReminderRepository extends JpaRepository<EventReminder, UUID> {

    @Query("SELECT er FROM EventReminder er WHERE er.status = 'PENDING' " +
            "AND er.scheduledAt <= :now " +
            "ORDER BY er.scheduledAt ASC")
    List<EventReminder> findPendingReminders(@Param("now") LocalDateTime now);

    List<EventReminder> findByEventId(UUID eventId);

    @Query("SELECT er FROM EventReminder er WHERE er.user = :user AND er.status = :status ORDER BY er.scheduledAt ASC")
    List<EventReminder> findByUserAndStatusOrderByScheduledAtAsc(
            @Param("user") User user,
            @Param("status") EventReminder.ReminderStatus status);

    List<EventReminder> findByUserId(UUID userId);

    @Query("SELECT er FROM EventReminder er WHERE er.event.id = :eventId AND er.user.id = :userId")
    List<EventReminder> findByEventIdAndUserId(@Param("eventId") UUID eventId, @Param("userId") UUID userId);
}