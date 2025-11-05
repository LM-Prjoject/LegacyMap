export type NotificationType = 'system' | 'update' | 'invite' | 'alert' | 'event_reminder';

export interface NotificationResponse {
    id: string;
    userId: string;
    title: string;
    message: string;
    type: NotificationType;
    relatedEntity?: Record<string, any>;
    isRead: boolean;
    createdAt: string;
}

export interface NotificationStatsResponse {
    totalCount: number;
    unreadCount: number;
    lastNotificationTime: string | null;
}