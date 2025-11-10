package com.legacymap.backend.repository;

import com.legacymap.backend.entity.Notification;
import com.legacymap.backend.entity.User;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

@Repository
public interface NotificationRepository extends JpaRepository<Notification, UUID> {

    Page<Notification> findByUserOrderByCreatedAtDesc(User user, Pageable pageable);

    List<Notification> findByUserAndIsReadOrderByCreatedAtDesc(User user, Boolean isRead);

    Long countByUserAndIsRead(User user, Boolean isRead);

    @Query("SELECT COUNT(n) FROM Notification n WHERE n.user = :user AND n.isRead = false")
    Long countUnreadByUser(@Param("user") User user);

    @Modifying
    @Query("UPDATE Notification n SET n.isRead = true WHERE n.user = :user AND n.isRead = false")
    int markAllAsReadByUser(@Param("user") User user);

    @Modifying
    @Query("DELETE FROM Notification n WHERE n.user = :user AND n.createdAt < :beforeDate")
    int deleteOldNotifications(@Param("user") User user, @Param("beforeDate") OffsetDateTime beforeDate);

}
