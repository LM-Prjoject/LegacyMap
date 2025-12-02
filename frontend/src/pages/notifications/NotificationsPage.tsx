import { useState, useEffect, useCallback } from 'react';
import { Bell, Check, CheckCheck, Trash2, Calendar, Users, TreePine, Gift, Filter, AlertCircle, X} from 'lucide-react';
import { notificationApi } from "@/api/notificationApi.ts";
import { sseService } from "@/api/sseService.ts";
import type { NotificationResponse } from '@/types/notification.ts';
import {useCurrentUser} from "@/hooks/useCurrentUser.ts";
import Navbar from "@/components/layout/Navbar.tsx";
import { personLinkApi } from "@/api/personLink.ts";
import { adminApi } from "@/api/ts_admin";
import NotificationDetailModal from "@/pages/notifications/NotificationDetailModal";

interface ConfirmModalProps {
    isOpen: boolean;
    title: string;
    message: string;
    onConfirm: () => void;
    onCancel: () => void;
}

const ConfirmModal = ({ isOpen, title, message, onConfirm, onCancel }: ConfirmModalProps) => {
    if (!isOpen) return null;

    return (
        <div className="fixed inset-0 z-50 flex items-center justify-center" style={{background: 'rgba(42, 53, 72, 0.5)', backdropFilter: 'blur(8px)'}}>
            <div className="relative w-full max-w-md mx-4">
                <div className="rounded-3xl shadow-2xl p-6" style={{
                    background: 'linear-gradient(135deg, rgba(255, 245, 220, 0.95) 0%, rgba(255, 235, 200, 0.9) 50%, rgba(255, 245, 220, 0.95) 100%)',
                    border: '3px solid rgba(255, 216, 155, 0.6)',
                    boxShadow: '0 20px 60px rgba(42, 53, 72, 0.3)'
                }}>
                    <div className="flex items-start gap-3 mb-4">
                        <div className="w-10 h-10 rounded-full flex items-center justify-center flex-shrink-0" style={{
                            background: 'linear-gradient(135deg, rgba(239, 68, 68, 0.2) 0%, rgba(239, 68, 68, 0.1) 100%)',
                            border: '2px solid rgba(239, 68, 68, 0.3)'
                        }}>
                            <AlertCircle className="w-5 h-5" style={{color: '#dc2626'}} />
                        </div>
                        <div className="flex-1">
                            <h3 className="text-xl font-black mb-2" style={{color: '#2a3548'}}>{title}</h3>
                            <p className="text-sm" style={{color: '#2a3548'}}>{message}</p>
                        </div>
                        <button
                            onClick={onCancel}
                            className="p-1 rounded-lg transition-all hover:bg-black/5"
                        >
                            <X className="w-5 h-5" style={{color: '#2a3548'}} />
                        </button>
                    </div>
                    <div className="flex gap-3 mt-6">
                        <button
                            onClick={onCancel}
                            className="flex-1 rounded-xl py-2.5 font-semibold transition-all border-2"
                            style={{
                                background: 'linear-gradient(135deg, rgba(255, 255, 255, 0.8) 0%, rgba(255, 255, 255, 0.6) 100%)',
                                borderColor: 'rgba(42, 53, 72, 0.3)',
                                color: '#2a3548'
                            }}
                        >
                            Hủy
                        </button>
                        <button
                            onClick={() => {
                                onConfirm();
                                onCancel();
                            }}
                            className="flex-1 rounded-xl py-2.5 font-semibold transition-all shadow-lg hover:brightness-110"
                            style={{
                                background: 'linear-gradient(135deg, #dc2626 0%, #ef4444 100%)',
                                color: 'white',
                                border: '2px solid rgba(220, 38, 38, 0.5)'
                            }}
                        >
                            Xác nhận
                        </button>
                    </div>
                </div>
            </div>
        </div>
    );
};

interface ToastProps {
    isOpen: boolean;
    message: string;
    type: 'error' | 'success';
    onClose: () => void;
}

const Toast = ({ isOpen, message, type, onClose }: ToastProps) => {
    useEffect(() => {
        if (isOpen) {
            const timer = setTimeout(onClose, 3000);
            return () => clearTimeout(timer);
        }
    }, [isOpen, onClose]);

    if (!isOpen) return null;

    return (
        <div className="fixed top-24 right-4 z-50 animate-slide-in">
            <div className="rounded-xl shadow-2xl p-4 flex items-center gap-3 min-w-[300px]" style={{
                background: type === 'error'
                    ? 'linear-gradient(135deg, rgba(239, 68, 68, 0.95) 0%, rgba(220, 38, 38, 0.95) 100%)'
                    : 'linear-gradient(135deg, rgba(34, 197, 94, 0.95) 0%, rgba(22, 163, 74, 0.95) 100%)',
                border: type === 'error' ? '2px solid rgba(239, 68, 68, 0.5)' : '2px solid rgba(34, 197, 94, 0.5)'
            }}>
                <AlertCircle className="w-5 h-5 text-white flex-shrink-0" />
                <p className="text-white font-medium flex-1">{message}</p>
                <button onClick={onClose} className="p-1 hover:bg-white/20 rounded transition-all">
                    <X className="w-4 h-4 text-white" />
                </button>
            </div>
        </div>
    );
};

const isUnbanRequestNotification = (
    n: NotificationResponse | null | undefined
): boolean => {
    if (!n) return false;
    const title = (n.title || '').toLowerCase();
    const msg = (n.message || '').toLowerCase();

    return (
        title.includes('yêu cầu mở khóa tài khoản') ||
        title.includes('yêu cầu mở khóa') ||
        msg.includes('yêu cầu mở khóa tài khoản') ||
        msg.includes('yêu cầu mở khóa')
    );
};

const NotificationsPage = () => {
    const { userId, loading: userLoading } = useCurrentUser();
    const [notifications, setNotifications] = useState<NotificationResponse[]>([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);
    const [filter, setFilter] = useState<'all' | 'unread' | 'read'>('all');
    const [selectedType, setSelectedType] = useState<string>('all');
    const [page, setPage] = useState(0);
    const [hasMore, setHasMore] = useState(true);
    const [unreadCount, setUnreadCount] = useState(0);
    const [newNotificationIds, setNewNotificationIds] = useState<Set<string>>(new Set());
    const [inviteActionById, setInviteActionById] = useState<Record<string, 'accepted' | 'rejected'>>({});
    const [actionLoading, setActionLoading] = useState(false);

    // Pending person link claims for current user (used to resolve target from notification text)
    const [claims, setClaims] = useState<Array<{ personId: string; personFullName: string; familyTreeId: string; linkType: string; invitedAt?: string }>>([]);
    const [claimsLoaded, setClaimsLoaded] = useState(false);

    const [detailModal, setDetailModal] = useState<{
        isOpen: boolean;
        notification: NotificationResponse | null;
    }>({
        isOpen: false,
        notification: null
    });

    const [confirmModal, setConfirmModal] = useState<{ isOpen: boolean; title: string; message: string; onConfirm: () => void; }>({
        isOpen: false,
        title: '',
        message: '',
        onConfirm: () => {}
    });

    const [toast, setToast] = useState<{ isOpen: boolean; message: string; type: 'error' | 'success'; }>({
        isOpen: false,
        message: '',
        type: 'success'
    });

    const showToast = (message: string, type: 'error' | 'success') => {
        setToast({ isOpen: true, message, type });
    };

    const showConfirm = (title: string, message: string, onConfirm: () => void) => {
        setConfirmModal({ isOpen: true, title, message, onConfirm });
    };

    // Inline actions for invite notifications
    const acceptInvite = async (n: NotificationResponse) => {
        const run = async () => {
            try {
                const target = resolveInviteTarget(n);
                if (!target || !userId) {
                    showToast('Không xác định được hồ sơ để xác nhận', 'error');
                    return;
                }
                await personLinkApi.acceptClaim(target.personId, userId);
                try { await notificationApi.markAsRead(n.id); } catch {}
                showToast('Đã xác nhận liên kết', 'success');
                setNotifications(prev => prev.map(x => x.id === n.id ? { ...x, isRead: true } : x));
                setInviteActionById(prev => ({ ...prev, [n.id]: 'accepted' }));
                try { localStorage.setItem(`notif_action_${n.id}`, 'accepted'); } catch {}
                const newCount = Math.max(0, unreadCount - (n.isRead ? 0 : 1));
                setUnreadCount(newCount);
                window.dispatchEvent(new CustomEvent('unreadCountChanged', { detail: newCount }));
                window.dispatchEvent(new CustomEvent('person-verified', { detail: { personId: target.personId } }));
                loadNotifications(0, false);
                loadClaims();
            } catch (e) {
                showToast('Xác nhận thất bại', 'error');
            }
        };
        showConfirm(
            'Xác nhận liên kết',
            'Bạn có chắc chắn muốn xác nhận liên kết hồ sơ này?',
            run
        );
    };

    const rejectInvite = async (n: NotificationResponse) => {
        const run = async () => {
            try {
                const target = resolveInviteTarget(n);
                if (!target || !userId) {
                    showToast('Không xác định được hồ sơ để từ chối', 'error');
                    return;
                }
                await personLinkApi.rejectClaim(target.personId, userId);
                try { await notificationApi.markAsRead(n.id); } catch {}
                showToast('Đã từ chối liên kết', 'success');
                setNotifications(prev => prev.map(x => x.id === n.id ? { ...x, isRead: true } : x));
                setInviteActionById(prev => ({ ...prev, [n.id]: 'rejected' }));
                try { localStorage.setItem(`notif_action_${n.id}`, 'rejected'); } catch {}
                const newCount = Math.max(0, unreadCount - (n.isRead ? 0 : 1));
                setUnreadCount(newCount);
                window.dispatchEvent(new CustomEvent('unreadCountChanged', { detail: newCount }));
                loadNotifications(0, false);
                loadClaims();
            } catch (e) {
                showToast('Từ chối thất bại', 'error');
            }
        };
        showConfirm(
            'Từ chối liên kết',
            'Bạn có chắc chắn muốn từ chối liên kết hồ sơ này?',
            run
        );
    };

    // Add body class management
    useEffect(() => {
        document.body.classList.add('page-notifications');
        return () => {
            document.body.classList.remove('page-notifications');
        };
    }, []);

    const loadNotifications = useCallback(async (pageNum = 0, append = false) => {
        if (!userId) return;

        try {
            if (pageNum === 0) {
                setLoading(true);
            }
            setError(null);

            const data = await notificationApi.getNotifications(pageNum, 15);

            const nextList = append ? [...notifications, ...data.content] : data.content;
            setNotifications(nextList);
            try {
                const map: Record<string, 'accepted' | 'rejected'> = {};
                nextList.forEach(n => {
                    const val = localStorage.getItem(`notif_action_${n.id}`);
                    if (val === 'accepted' || val === 'rejected') map[n.id] = val;
                });
                setInviteActionById(map);
            } catch {}
            setHasMore(!data.last);
            setPage(pageNum);
            setUnreadCount(data.unreadCount);
        } catch (err: any) {
            setError(err.message || 'Không thể tải thông báo');
        } finally {
            setLoading(false);
        }
    }, [userId, notifications]);

    const loadClaims = useCallback(async () => {
        if (!userId) return;
        try {
            setClaimsLoaded(false);
            const res = await personLinkApi.getMyClaims(userId);
            const items = res?.result ?? res?.content ?? res ?? [];
            setClaims(items.map((c: any) => ({
                personId: c.personId,
                personFullName: c.personFullName,
                familyTreeId: c.familyTreeId,
                linkType: c.linkType,
                invitedAt: c.invitedAt
            })));
        } catch (e: any) {
            console.warn('Failed to load claims', e);
        } finally { setClaimsLoaded(true); }
    }, [userId]);

    useEffect(() => {
        if (!userId || userLoading) return;

        let eventSource: EventSource | null = null;

        const connectSSE = async () => {
            eventSource = sseService.connect(userId, (notif: NotificationResponse) => {
                setNotifications(prev => {
                    if (prev.some(n => n.id === notif.id)) return prev;
                    return [notif, ...prev];
                });
                try {
                    const val = localStorage.getItem(`notif_action_${notif.id}`);
                    if (val === 'accepted' || val === 'rejected') {
                        setInviteActionById(prev => ({ ...prev, [notif.id]: val as 'accepted' | 'rejected' }));
                    }
                } catch {}

                const id = notif.id;
                setNewNotificationIds(prev => new Set(prev).add(id));
                setTimeout(() => {
                    setNewNotificationIds(prev => {
                        const next = new Set(prev);
                        next.delete(id);
                        return next;
                    });
                }, 3000);

                if (!notif.isRead) {
                    setUnreadCount(prev => prev + 1);
                }

                try {
                    if (isAcceptedResult(notif)) {
                        const target = resolveInviteTarget(notif);
                        if (target?.personId) {
                            window.dispatchEvent(new CustomEvent('person-verified', { detail: { personId: target.personId } }));
                            loadClaims();
                        }
                    }
                } catch {}
            });
        };

        connectSSE();

        return () => {
            if (eventSource) {
                sseService.disconnect(eventSource);
            }
        };
    }, [userId, userLoading, loadClaims]);

    useEffect(() => {
        if (userId && !userLoading) {
            loadNotifications(0, false);
            loadClaims();
        } else if (userId === null && !userLoading) {
            setLoading(false);
        }
    }, [userId, userLoading]);

    if (userLoading || (loading && notifications.length === 0)) {
        return (
            <>
                <Navbar />
                <div className="min-h-screen flex items-center justify-center" style={{backgroundColor: '#2a3548'}}>
                    <div className="text-center">
                        <div className="animate-spin rounded-full h-12 w-12 border-b-2 mx-auto mb-4" style={{borderColor: 'rgb(255, 216, 155)'}}></div>
                        <p style={{color: 'rgb(255, 216, 155)'}}>Đang tải thông báo...</p>
                    </div>
                </div>
            </>
        );
    }

    if (!userId) {
        return (
            <>
                <Navbar />
                <div className="flex flex-col items-center justify-center min-h-screen text-white" style={{backgroundColor: '#2a3548'}}>
                    <Bell className="w-16 h-16 mb-4 opacity-30" />
                    <p>Vui lòng đăng nhập để xem thông báo</p>
                    <button
                        onClick={() => window.location.href = '/login'}
                        className="mt-4 px-4 py-2 bg-gradient-to-r from-[#b49e7b] to-[#d1b98a] text-[#2a3548] rounded-lg"
                    >
                        Đăng nhập
                    </button>
                </div>
            </>
        );
    }

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

    // Helpers to render inline actions for invite notifications
    const isPersonInvite = (n: NotificationResponse) => {
        const t = (n.title || '').toLowerCase();
        const m = (n.message || '').toLowerCase();
        return (
            t.includes('lời mời liên kết hồ sơ') ||
            t.includes('liên kết hồ sơ') ||
            m.includes('mời xác nhận liên kết') ||
            m.includes('liên kết hồ sơ') ||
            m.includes('xác nhận liên kết')
        );
    };

    const resolveInviteTarget = (n: NotificationResponse): { personId: string; personFullName?: string } | null => {
        const re: any = (n as any).relatedEntity;
        const byRelated = re?.personId || re?.id;
        if (byRelated) {
            return { personId: String(byRelated), personFullName: re?.name };
        }
        const msg = n.message || '';
        const found = claims.find(c => !!c.personFullName && msg.includes(c.personFullName));
        return found ? { personId: found.personId, personFullName: found.personFullName } : null;
    };

    const isAcceptedResult = (n: NotificationResponse) => {
        const t = (n.title || '').toLowerCase();
        const m = (n.message || '').toLowerCase();
        return t.includes('đã xác nhận') || t.includes('đã chấp nhận') || m.includes('đã xác nhận') || m.includes('đã chấp nhận') || m.includes('accepted');
    };

    // Only the invited user (who has a pending claim) should see actions
    const canActOnInvite = (n: NotificationResponse) => {
        if (!isPersonInvite(n)) return false;
        const target = resolveInviteTarget(n);
        if (!target) return false;
        return claims.some(c => c.personId === target.personId);
    };

    const markAsRead = async (id: string) => {
        try {
            await notificationApi.markAsRead(id);
            setNotifications(prev => prev.map(n => n.id === id ? { ...n, isRead: true } : n));

            const newCount = notifications.filter(n => !n.isRead && n.id !== id).length;
            setUnreadCount(newCount);

            window.dispatchEvent(new CustomEvent('unreadCountChanged', { detail: newCount }));
        } catch (err) {
            showToast('Không thể đánh dấu đã đọc', 'error');
        }
    };

    const markAllAsRead = async () => {
        try {
            await notificationApi.markAllAsRead();
            setNotifications(prev => prev.map(n => ({ ...n, isRead: true })));
            setUnreadCount(0);

            window.dispatchEvent(new CustomEvent('unreadCountChanged', { detail: 0 }));
        } catch (err) {
            showToast('Không thể đánh dấu tất cả', 'error');
        }
    };

    const deleteNotification = async (id: string) => {
        showConfirm(
            'Xóa thông báo',
            'Bạn có chắc chắn muốn xóa thông báo này không?',
            async () => {
                try {
                    await notificationApi.deleteNotification(id);
                    const wasUnread = notifications.find(n => n.id === id)?.isRead === false;
                    setNotifications(prev => prev.filter(n => n.id !== id));
                    try { localStorage.removeItem(`notif_action_${id}`); } catch {}

                    if (wasUnread) {
                        const newCount = Math.max(0, unreadCount - 1);
                        setUnreadCount(newCount);
                        window.dispatchEvent(new CustomEvent('unreadCountChanged', { detail: newCount }));
                    }
                } catch (err) {
                    showToast('Không thể xóa thông báo', 'error');
                }
            }
        );
    };

    const deleteAllRead = async () => {
        showConfirm(
            'Xóa tất cả thông báo đã đọc',
            'Bạn có chắc chắn muốn xóa tất cả thông báo đã đọc không? Hành động này không thể hoàn tác.',
            async () => {
                const readIds = notifications.filter(n => n.isRead).map(n => n.id);
                for (const id of readIds) {
                    try {
                        await notificationApi.deleteNotification(id);
                    } catch (err) { /* ignore */ }
                }
                setNotifications(prev => prev.filter(n => !n.isRead));
            }
        );
    };

    const filteredNotifications = notifications.filter(n => {
        if (filter === 'unread' && n.isRead) return false;
        if (filter === 'read' && !n.isRead) return false;
        if (selectedType !== 'all' && n.type !== selectedType) return false;
        const t = (n.title || '').toLowerCase();
        const m = (n.message || '').toLowerCase();
        if (canActOnInvite(n) && (
            t.includes('đã chấp nhận') || t.includes('đã từ chối') || t.includes('đã xác nhận') ||
            m.includes('đã chấp nhận') || m.includes('đã từ chối') || m.includes('đã xác nhận') ||
            m.includes('accepted') || m.includes('rejected')
        )) {
            return false;
        }
        return true;
    });

    const handleApproveFromDetail = async (n: NotificationResponse) => {
        if (actionLoading) return;
        setActionLoading(true);

        const requestId = n.relatedEntity?.id;
        if (!requestId) {
            showToast("Không tìm thấy ID yêu cầu mở khóa!","error");
            setActionLoading(false);
            return;
        }

        try {
            await adminApi.approveUnbanRequest(requestId);
            await markAsRead(n.id);

            showToast("Đã chấp nhận yêu cầu mở khóa tài khoản.", "success");

            setDetailModal({ isOpen: false, notification: null });
            loadNotifications(0, false);
        } catch (e) {
            showToast("Xử lý thất bại", "error");
        } finally {
            setActionLoading(false);
        }
    };

    const handleRejectFromDetail = async (n: NotificationResponse) => {
        if (actionLoading) return;
        setActionLoading(true);

        const requestId = n.relatedEntity?.id;
        if (!requestId) {
            showToast("Không tìm thấy ID yêu cầu mở khóa!", "error");
            setActionLoading(false);
            return;
        }

        try {
            await adminApi.denyUnbanRequest(requestId);
            await markAsRead(n.id);

            showToast("Đã từ chối yêu cầu mở khóa tài khoản.", "success");

            setDetailModal({ isOpen: false, notification: null });
            loadNotifications(0, false);
        } catch (e) {
            showToast("Từ chối thất bại", "error");
        } finally {
            setActionLoading(false);
        }
    };

    return (
        <div className="min-h-screen pt-8 pb-12 px-4" style={{ backgroundColor: '#2a3548' }}>
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

                {/* Error */}
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
                                onClick={() => setDetailModal({ isOpen: true, notification: n })}
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
                                        {claimsLoaded && canActOnInvite(n) && !n.isRead && (
                                            <div className="mt-3 flex gap-2">
                                                <button
                                                    className="px-3 py-1.5 rounded-md bg-emerald-600 text-white text-sm hover:bg-emerald-700"
                                                    onClick={(e) => { e.stopPropagation(); acceptInvite(n); }}
                                                >Chấp nhận</button>
                                                <button
                                                    className="px-3 py-1.5 rounded-md bg-red-600 text-white text-sm hover:bg-red-700"
                                                    onClick={(e) => { e.stopPropagation(); rejectInvite(n); }}
                                                >Từ chối</button>
                                            </div>
                                        )}
                                        {isPersonInvite(n) && inviteActionById[n.id] && (
                                            <div className="mt-3">
                                                <span className={`px-2 py-1 rounded text-xs font-semibold inline-block ${inviteActionById[n.id] === 'accepted' ? 'bg-emerald-600/20 text-emerald-300 border border-emerald-500/30' : inviteActionById[n.id] === 'rejected' ? 'bg-red-600/20 text-red-300 border border-red-500/30' : 'bg-white/10 text-white/70 border border-white/10'}`}>
                                                    {inviteActionById[n.id] === 'accepted' ? 'Đã xác nhận' : inviteActionById[n.id] === 'rejected' ? 'Đã từ chối' : 'Đã xử lý'}
                                                </span>
                                            </div>
                                        )}
                                    </div>

                                    <div className="flex items-center gap-2 flex-shrink-0">
                                        {!n.isRead && (
                                            <button
                                                onClick={(e) => { e.stopPropagation(); markAsRead(n.id); }}
                                                className="p-2 rounded-lg bg-white/10 text-white hover:bg-white/20 transition-all"
                                                title="Đánh dấu đã đọc"
                                            >
                                                <Check className="w-4 h-4" />
                                            </button>
                                        )}
                                        <button
                                            onClick={(e) => { e.stopPropagation(); deleteNotification(n.id); }}
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

                {/* Loading more indicator */}
                {loading && notifications.length > 0 && (
                    <div className="flex justify-center py-4">
                        <div className="animate-spin rounded-full h-8 w-8 border-b-2" style={{borderColor: 'rgb(255, 216, 155)'}}></div>
                    </div>
                )}
            </div>

            {/* Modals */}
            <ConfirmModal
                isOpen={confirmModal.isOpen}
                title={confirmModal.title}
                message={confirmModal.message}
                onConfirm={confirmModal.onConfirm}
                onCancel={() => setConfirmModal(prev => ({ ...prev, isOpen: false }))}
            />

            <Toast
                isOpen={toast.isOpen}
                message={toast.message}
                type={toast.type}
                onClose={() => setToast(prev => ({ ...prev, isOpen: false }))}
            />

            <NotificationDetailModal
                isOpen={detailModal.isOpen}
                notification={detailModal.notification}
                onClose={() => setDetailModal({ isOpen: false, notification: null })}
                onApprove={handleApproveFromDetail}
                onReject={handleRejectFromDetail}
                showModerationActions={isUnbanRequestNotification(detailModal.notification)}
                actionLoading={actionLoading}
            />

            <style>{`
                @keyframes slide-in {
                    from {
                        transform: translateX(100%);
                        opacity: 0;
                    }
                    to {
                        transform: translateX(0);
                        opacity: 1;
                    }
                }
                .animate-slide-in {
                    animation: slide-in 0.3s ease-out;
                }
            `}</style>
        </div>
    );
};

export default NotificationsPage;
