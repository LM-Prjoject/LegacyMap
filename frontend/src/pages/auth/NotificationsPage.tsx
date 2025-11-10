import { useState, useEffect, useCallback} from 'react';
import { Bell, Check, CheckCheck, Trash2, Calendar, Users, TreePine, Gift, Filter, Loader2} from 'lucide-react';
import { notificationApi } from "@/api/notificationApi";
import { sseService } from "@/api/sseService";
import type { NotificationResponse, NotificationStatsResponse } from '@/types/notification';
import {useCurrentUser} from "@/hooks/useCurrentUser";
import Navbar from "@/components/layout/Navbar.tsx";

const NotificationsPage = () => {
    const { userId, loading: userLoading } = useCurrentUser();
    const [notifications, setNotifications] = useState<NotificationResponse[]>([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);
    const [filter, setFilter] = useState<'all' | 'unread' | 'read'>('all');
    const [selectedType, setSelectedType] = useState<string>('all');
    const [page, setPage] = useState(0);
    const [hasMore, setHasMore] = useState(true);
    const [stats, setStats] = useState({ unreadCount: 0, totalCount: 0 });
    const [newNotificationIds, setNewNotificationIds] = useState<Set<string>>(new Set());

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

    const loadStats = useCallback(async () => {
        try {
            const data: NotificationStatsResponse = await notificationApi.getStats();
            setStats({
                unreadCount: data.unreadCount || 0,
                totalCount: data.totalCount || 0
            });
        } catch (err) {
            console.error('Failed to load stats', err);
        }
    }, []);

    useEffect(() => {
        if (!userId || userLoading) return;

        let eventSource: EventSource | null = null;

        const connectSSE = async () => {
            eventSource = sseService.connect(userId, (notif) => {
                setNotifications(prev => [notif, ...prev]);
                const id = notif.id;
                setNewNotificationIds(prev => new Set(prev).add(id));
                setTimeout(() => {
                    setNewNotificationIds(prev => {
                        const next = new Set(prev);
                        next.delete(id);
                        return next;
                    });
                }, 3000);
                loadStats();
            });
        };

        connectSSE();

        return () => {
            if (eventSource) {
                sseService.disconnect(eventSource);
            }
        };
    }, [userId, userLoading, loadStats]);

    useEffect(() => {
        if (userId && !userLoading) {
            loadNotifications(0, false);
            loadStats();
        }
    }, [userId, userLoading, loadNotifications, loadStats]);

    // Nếu đang load user
    if (userLoading) {
        return (
            <div className="flex justify-center items-center min-h-screen">
                <Loader2 className="w-8 h-8 animate-spin" style={{ color: 'rgb(255, 216, 155)' }} />
            </div>
        );
    }

    // Nếu không có user
    if (!userId) {
        return (
            <div className="flex flex-col items-center justify-center min-h-screen text-white">
                <Bell className="w-16 h-16 mb-4 opacity-30" />
                <p>Vui lòng đăng nhập để xem thông báo</p>
                <button
                    onClick={() => window.location.href = '/login'}
                    className="mt-4 px-4 py-2 bg-gradient-to-r from-[#b49e7b] to-[#d1b98a] text-[#2a3548] rounded-lg"
                >
                    Đăng nhập
                </button>
            </div>
        );
    }

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

        if (minutes < 1) return 'Vừa xong';
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

    const filteredNotifications = notifications.filter(n => {
        if (filter === 'unread' && n.isRead) return false;
        if (filter === 'read' && !n.isRead) return false;
        if (selectedType !== 'all' && n.type !== selectedType) return false;
        return true;
    });

    const unreadCount = stats.unreadCount;

    return (
        <div className="min-h-screen pt-20 pb-12 px-4" style={{ backgroundColor: '#2a3548' }}>
            <Navbar />
            <div className="max-w-6xl mx-auto">
                {/* Header */}
                <div className="mb-8">
                    <div className="flex items-center justify-between mb-4">
                        <div className="flex items-center gap-3">
                            <div className="relative w-12 h-12 rounded-xl bg-gradient-to-br from-[#b49e7b] to-[#d1b98a] flex items-center justify-center shadow-lg">
                                <Bell className="w-6 h-6 text-[#2a3548]" />
                                {unreadCount > 0 && (
                                    <span className="absolute -top-1 -right-1 min-w-5 h-5 px-1.5 rounded-full bg-red-500 text-white text-xs font-bold flex items-center justify-center animate-pulse">
                                        {unreadCount}
                                    </span>
                                )}
                            </div>
                            <div>
                                <h1 className="text-3xl font-bold text-white">Thông báo</h1>
                                <p className="text-sm" style={{ color: 'rgb(255, 216, 155)' }}>
                                    {unreadCount > 0 ? `${unreadCount} chưa đọc` : 'Không có thông báo mới'}
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
                                } ${newNotificationIds.has(n.id) ? 'animate-pulse ring-2 ring-yellow-400' : ''}`}
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
                                            <h3 className="font-semibold text-white text-sm">
                                                {n.title}
                                                {newNotificationIds.has(n.id) && (
                                                    <span className="ml-2 inline-block w-2 h-2 rounded-full bg-yellow-400 animate-ping" />
                                                )}
                                            </h3>
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
            </div>
        </div>
    );
};

export default NotificationsPage;