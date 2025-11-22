package com.legacymap.backend.repository;

import com.legacymap.backend.entity.EventReminder;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

@Repository
public interface EventReminderRepository extends JpaRepository<EventReminder, UUID> {

    List<EventReminder> findByEventId(UUID eventId);

    @Query("SELECT r FROM EventReminder r " +
            "WHERE r.status = 'pending' " +
            "AND r.scheduledAt <= :nowPlusBuffer " +
            "AND r.scheduledAt >= :ninetySecondsAgo")
    List<EventReminder> findPendingReminders(
            @Param("nowPlusBuffer") OffsetDateTime nowPlusBuffer,
            @Param("ninetySecondsAgo") OffsetDateTime ninetySecondsAgo);

    List<EventReminder> findByRecipientTypeAndRecipientIdOrderByScheduledAtAsc(
            EventReminder.RecipientType recipientType,
            UUID recipientId);
}
