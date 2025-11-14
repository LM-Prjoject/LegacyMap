import {http} from '@/api/http';
import type {NotificationResponse, NotificationPageResponse } from '@/types/notification';

export const notificationApi = {
    // Lấy danh sách thông báo (phân trang)
    getNotifications: async (page = 0, size = 10): Promise<NotificationPageResponse> => {
        const response = await http.get('/notifications', {
            params: { page, size, sort: 'createdAt,desc' }
        });
        return response.data;
    },

    // Lấy thông báo chưa đọc
    getUnreadNotifications: async (): Promise<NotificationResponse[]> => {
        const response = await http.get('/notifications/unread');
        return response.data;
    },

    // Đánh dấu đã đọc
    markAsRead: async (notificationId: string) => {
        await http.put(`/notifications/${notificationId}/read`);
    },

    // Đánh dấu tất cả đã đọc
    markAllAsRead: async () => {
        await http.put('/notifications/read-all');
    },

    // Xóa thông báo
    deleteNotification: async (notificationId: string) => {
        await http.delete(`/notifications/${notificationId}`);
    },

    // Dọn dẹp cũ
    cleanupOld: async (daysToKeep: number = 30) => {
        await http.delete('/notifications/cleanup', { params: { daysToKeep } });
    }
}
