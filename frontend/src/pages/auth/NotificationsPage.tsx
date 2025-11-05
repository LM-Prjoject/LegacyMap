import { useState, useEffect, useCallback } from 'react';
import { Bell, Check, CheckCheck, Trash2, Calendar, Users, TreePine, Gift, Filter, Loader2 } from 'lucide-react';
import {notificationApi} from "@/api/notificationApi.ts";
import type { NotificationResponse } from '@/types/notification';

const NotificationsPage = () => {
    const [notifications, setNotifications] = useState<NotificationResponse[]>([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);
    const [filter, setFilter] = useState<'all' | 'unread' | 'read'>('all');
    const [selectedType, setSelectedType] = useState<string>('all');
    const [page, setPage] = useState(0);
    const [hasMore, setHasMore] = useState(true);
    const [stats, setStats] = useState({ unreadCount: 0, totalCount: 0 });

    // Load notifications
    const loadNotifications = useCallback(async (pageNum = 0, append = false) => {
        try {
            setLoading(pageNum === 0);
            const data = await notificationApi.getNotifications(pageNum, 15);

            const newNotifs = data.content || [];
            setNotifications(prev => append ? [...prev, ...newNotifs] : newNotifs);
            setHasMore(!data.last);
            setPage(pageNum);
        } catch (err: any) {
            setError(err.message || 'Không thể tải thông báo');
        } finally {
            setLoading(false);
        }
    }, []);

    // Load stats
    const loadStats = useCallback(async () => {
        try {
            const data = await notificationApi.getStats();
            setStats({
                unreadCount: data.unreadCount || 0,
                totalCount: data.totalCount || 0
            });
        } catch (err) {
            console.error('Failed to load stats', err);
        }
    }, []);

    // Initial load
    useEffect(() => {
        loadNotifications(0, false);
        loadStats();
    }, [loadNotifications, loadStats]);

    // Icons & Colors
    const getIcon = (type: string) => {
        switch (type) {
            case 'event_reminder': return <Calendar className="w-5 h-5" />;
            case 'invite': return <Users className="w-5 h-5" />;
            case 'update': return <TreePine className="w-5 h-5" />;
            case 'alert': return <Gift className="w-5 h-5" />;
            default: return <Bell className="w-5 h-5" />;
        }
    };

    const getTypeColor = (type: string) => {
        switch (type) {
            case 'event_reminder': return 'from-blue-500/20 to-blue-600/20 border-blue-500/30';
            case 'invite': return 'from-green-500/20 to-green-600/20 border-green-500/30';
            case 'update': return 'from-amber-500/20 to-amber-600/20 border-amber-500/30';
            case 'alert': return 'from-pink-500/20 to-pink-600/20 border-pink-500/30';
            default: return 'from-gray-500/20 to-gray-600/20 border-gray-500/30';
        }
    };

    const formatTimestamp = (isoString: string) => {
        const now = new Date();
        const time = new Date(isoString);
        const diff = now.getTime() - time.getTime();
        const minutes = Math.floor(diff / (1000 * 60));
        const hours = Math.floor(diff / (1000 * 60 * 60));
        const days = Math.floor(diff / (1000 * 60 * 60 * 24));

        if (minutes < 60) return `${minutes} phút trước`;
        if (hours < 24) return `${hours} giờ trước`;
        if (days < 7) return `${days} ngày trước`;
        return time.toLocaleDateString('vi-VN');
    };

    // Actions
    const markAsRead = async (id: string) => {
        try {
            await notificationApi.markAsRead(id);
            setNotifications(prev => prev.map(n => n.id === id ? { ...n, isRead: true } : n));
            loadStats();
        } catch (err) {
            alert('Không thể đánh dấu đã đọc');
        }
    };

    const markAllAsRead = async () => {
        try {
            await notificationApi.markAllAsRead();
            setNotifications(prev => prev.map(n => ({ ...n, isRead: true })));
            loadStats();
        } catch (err) {
            alert('Không thể đánh dấu tất cả');
        }
    };

    const deleteNotification = async (id: string) => {
        if (!confirm('Xóa thông báo này?')) return;
        try {
            await notificationApi.deleteNotification(id);
            setNotifications(prev => prev.filter(n => n.id !== id));
            loadStats();
        } catch (err) {
            alert('Không thể xóa');
        }
    };

    const deleteAllRead = async () => {
        if (!confirm('Xóa tất cả thông báo đã đọc?')) return;
        const readIds = notifications.filter(n => n.isRead).map(n => n.id);
        for (const id of readIds) {
            try {
                await notificationApi.deleteNotification(id);
            } catch (err) { /* ignore */ }
        }
        setNotifications(prev => prev.filter(n => !n.isRead));
        loadStats();
    };

    // Filter
    const filteredNotifications = notifications.filter(n => {
        if (filter === 'unread' && n.isRead) return false;
        if (filter === 'read' && !n.isRead) return false;
        if (selectedType !== 'all' && n.type !== selectedType) return false;
        return true;
    });

    const unreadCount = stats.unreadCount;

    return (
        <div className="min-h-screen pt-20 pb-12 px-4" style={{ backgroundColor: '#2a3548' }}>
            <div className="max-w-4xl mx-auto">
                {/* Header */}
                <div className="mb-8">
                    <div className="flex items-center justify-between mb-4">
                        <div className="flex items-center gap-3">
                            <div className="w-12 h-12 rounded-xl bg-gradient-to-br from-[#b49e7b] to-[#d1b98a] flex items-center justify-center shadow-lg">
                                <Bell className="w-6 h-6 text-[#2a3548]" />
                            </div>
                            <div>
                                <h1 className="text-3xl font-bold text-white">Thông báo</h1>
                                <p className="text-sm" style={{ color: 'rgb(255, 216, 155)' }}>
                                    {unreadCount} thông báo chưa đọc
                                </p>
                            </div>
                        </div>

                        {unreadCount > 0 && (
                            <button
                                onClick={markAllAsRead}
                                className="flex items-center gap-2 px-4 py-2 rounded-lg bg-gradient-to-r from-[#b49e7b] to-[#d1b98a] text-[#2a3548] font-medium hover:brightness-110 transition-all shadow-lg"
                            >
                                <CheckCheck className="w-4 h-4" />
                                Đánh dấu tất cả
                            </button>
                        )}
                    </div>

                    {/* Filters */}
                    <div className="flex flex-wrap gap-3 mb-4">
                        <div className="flex gap-2">
                            {(['all', 'unread', 'read'] as const).map(f => (
                                <button
                                    key={f}
                                    onClick={() => setFilter(f)}
                                    className={`px-4 py-2 rounded-lg text-sm font-medium transition-all ${
                                        filter === f
                                            ? 'bg-gradient-to-r from-[#b49e7b] to-[#d1b98a] text-[#2a3548] shadow-lg'
                                            : 'bg-white/5 text-white hover:bg-white/10'
                                    }`}
                                >
                                    {f === 'all' ? 'Tất cả' : f === 'unread' ? 'Chưa đọc' : 'Đã đọc'}
                                </button>
                            ))}
                        </div>

                        <div className="flex items-center gap-2">
                            <Filter className="w-4 h-4" style={{ color: 'rgb(255, 216, 155)' }} />
                            <select
                                value={selectedType}
                                onChange={(e) => setSelectedType(e.target.value)}
                                className="px-4 py-2 rounded-lg bg-white/5 text-white text-sm font-medium border border-white/10 hover:bg-white/10 transition-all cursor-pointer outline-none"
                                style={{ backgroundColor: '#20283d' }}
                            >
                                <option value="all">Tất cả loại</option>
                                <option value="event_reminder">Nhắc nhở</option>
                                <option value="invite">Mời tham gia</option>
                                <option value="update">Cập nhật</option>
                                <option value="alert">Cảnh báo</option>
                                <option value="system">Hệ thống</option>
                            </select>
                        </div>
                    </div>

                    {notifications.some(n => n.isRead) && (
                        <button
                            onClick={deleteAllRead}
                            className="flex items-center gap-2 px-3 py-1.5 rounded-lg bg-red-500/10 text-red-300 text-sm hover:bg-red-500/20 transition-all"
                        >
                            <Trash2 className="w-4 h-4" />
                            Xóa thông báo đã đọc
                        </button>
                    )}
                </div>

                {/* Loading & Error */}
                {loading && (
                    <div className="flex justify-center py-8">
                        <Loader2 className="w-8 h-8 animate-spin" style={{ color: 'rgb(255, 216, 155)' }} />
                    </div>
                )}

                {error && (
                    <div className="text-center py-8 text-red-300">
                        {error}
                        <button onClick={() => loadNotifications(0, false)} className="ml-2 underline">
                            Thử lại
                        </button>
                    </div>
                )}

                {/* List */}
                <div className="space-y-3">
                    {filteredNotifications.length === 0 && !loading ? (
                        <div className="text-center py-16">
                            <Bell className="w-16 h-16 mx-auto mb-4 opacity-20" style={{ color: 'rgb(255, 216, 155)' }} />
                            <p className="text-white/50 text-lg">Không có thông báo nào</p>
                        </div>
                    ) : (
                        filteredNotifications.map((n) => (
                            <div
                                key={n.id}
                                className={`rounded-xl border backdrop-blur-sm transition-all hover:scale-[1.02] ${
                                    n.isRead ? 'bg-white/5' : `bg-gradient-to-r ${getTypeColor(n.type)}`
                                }`}
                                style={{ borderColor: n.isRead ? 'rgba(255, 216, 155, 0.1)' : undefined }}
                            >
                                <div className="p-4 flex items-start gap-4">
                                    <div className={`w-10 h-10 rounded-lg flex items-center justify-center flex-shrink-0 ${
                                        n.isRead ? 'bg-white/10' : 'bg-white/20'
                                    }`}>
                                        <div style={{ color: n.isRead ? 'rgb(255, 216, 155)' : '#fff' }}>
                                            {getIcon(n.type)}
                                        </div>
                                    </div>

                                    <div className="flex-1 min-w-0">
                                        <div className="flex items-start justify-between gap-2 mb-1">
                                            <h3 className="font-semibold text-white text-sm">{n.title}</h3>
                                            {!n.isRead && (
                                                <div className="w-2 h-2 rounded-full bg-gradient-to-r from-[#b49e7b] to-[#d1b98a] flex-shrink-0 mt-1.5" />
                                            )}
                                        </div>
                                        <p className="text-sm text-white/70 mb-2">{n.message}</p>
                                        <p className="text-xs" style={{ color: 'rgb(255, 216, 155)', opacity: 0.7 }}>
                                            {formatTimestamp(n.createdAt)}
                                        </p>
                                    </div>

                                    <div className="flex items-center gap-2 flex-shrink-0">
                                        {!n.isRead && (
                                            <button
                                                onClick={() => markAsRead(n.id)}
                                                className="p-2 rounded-lg bg-white/10 text-white hover:bg-white/20 transition-all"
                                                title="Đánh dấu đã đọc"
                                            >
                                                <Check className="w-4 h-4" />
                                            </button>
                                        )}
                                        <button
                                            onClick={() => deleteNotification(n.id)}
                                            className="p-2 rounded-lg bg-red-500/10 text-red-300 hover:bg-red-500/20 transition-all"
                                            title="Xóa"
                                        >
                                            <Trash2 className="w-4 h-4" />
                                        </button>
                                    </div>
                                </div>
                            </div>
                        ))
                    )}
                </div>

                {/* Load More */}
                {hasMore && !loading && (
                    <div className="mt-6 text-center">
                        <button
                            onClick={() => loadNotifications(page + 1, true)}
                            className="px-6 py-2 rounded-lg bg-white/5 text-white hover:bg-white/10 transition-all"
                        >
                            Xem thêm
                        </button>
                    </div>
                )}

                {/* Stats */}
                {notifications.length > 0 && (
                    <div className="mt-8 grid grid-cols-1 sm:grid-cols-3 gap-4">
                        <div className="p-4 rounded-xl bg-white/5 border border-white/10">
                            <div className="text-2xl font-bold text-white mb-1">{stats.totalCount}</div>
                            <div className="text-sm" style={{ color: 'rgb(255, 216, 155)' }}>Tổng</div>
                        </div>
                        <div className="p-4 rounded-xl bg-white/5 border border-white/10">
                            <div className="text-2xl font-bold text-white mb-1">{unreadCount}</div>
                            <div className="text-sm" style={{ color: 'rgb(255, 216, 155)' }}>Chưa đọc</div>
                        </div>
                        <div className="p-4 rounded-xl bg-white/5 border border-white/10">
                            <div className="text-2xl font-bold text-white mb-1">
                                {notifications.filter(n => n.type === 'event_reminder').length}
                            </div>
                            <div className="text-sm" style={{ color: 'rgb(255, 216, 155)' }}>Nhắc nhở</div>
                        </div>
                    </div>
                )}
            </div>
        </div>
    );
};

export default NotificationsPage;