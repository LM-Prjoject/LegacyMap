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

    @Query("""
    SELECT er FROM EventReminder er 
    WHERE er.recipientType = :recipientType 
      AND er.recipientId = :recipientId 
      AND er.status = :status 
    ORDER BY er.scheduledAt ASC
    """)
    List<EventReminder> findByRecipientAndStatusOrderByScheduledAtAsc(
            @Param("recipientType") EventReminder.RecipientType recipientType,
            @Param("recipientId") UUID recipientId,
            @Param("status") EventReminder.ReminderStatus status);

    @Query("""
    SELECT er FROM EventReminder er 
    WHERE er.status = 'pending' 
      AND er.scheduledAt <= :now
    ORDER BY er.scheduledAt ASC
    """)
    List<EventReminder> findPendingReminders(@Param("now") OffsetDateTime now);

    List<EventReminder> findByRecipientTypeAndRecipientIdOrderByScheduledAtAsc(
            EventReminder.RecipientType recipientType,
            UUID recipientId);
}
