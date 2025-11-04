import React, { useState, useEffect } from 'react';
import { Bell, Check, CheckCheck, Trash2, Calendar, Users, TreePine, Gift, AlertCircle, X, Filter } from 'lucide-react';

// Mock data - trong thực tế sẽ fetch từ API
const mockNotifications = [
    {
        id: 1,
        type: 'reminder',
        title: 'Nhắc nhở sự kiện',
        message: 'Sự kiện "Giỗ Tổ" sẽ diễn ra vào ngày mai',
        timestamp: new Date(Date.now() - 1000 * 60 * 30).toISOString(),
        read: false,
        icon: 'calendar'
    },
    {
        id: 2,
        type: 'family',
        title: 'Thành viên mới',
        message: 'Nguyễn Văn A đã được thêm vào cây gia phả',
        timestamp: new Date(Date.now() - 1000 * 60 * 60 * 2).toISOString(),
        read: false,
        icon: 'users'
    },
    {
        id: 3,
        type: 'tree',
        title: 'Cập nhật cây gia phả',
        message: 'Cây gia phả "Dòng họ Nguyễn" đã được cập nhật',
        timestamp: new Date(Date.now() - 1000 * 60 * 60 * 5).toISOString(),
        read: true,
        icon: 'tree'
    },
    {
        id: 4,
        type: 'birthday',
        title: 'Sinh nhật',
        message: 'Hôm nay là sinh nhật của Nguyễn Thị B',
        timestamp: new Date(Date.now() - 1000 * 60 * 60 * 24).toISOString(),
        read: false,
        icon: 'gift'
    },
    {
        id: 5,
        type: 'reminder',
        title: 'Nhắc nhở quan trọng',
        message: 'Đừng quên chuẩn bị cho lễ giỗ vào tuần tới',
        timestamp: new Date(Date.now() - 1000 * 60 * 60 * 24 * 2).toISOString(),
        read: true,
        icon: 'alert'
    },
    {
        id: 6,
        type: 'family',
        title: 'Yêu cầu kết nối',
        message: 'Trần Văn C muốn kết nối với cây gia phả của bạn',
        timestamp: new Date(Date.now() - 1000 * 60 * 60 * 24 * 3).toISOString(),
        read: false,
        icon: 'users'
    }
];

const NotificationsPage = () => {
    const [notifications, setNotifications] = useState(mockNotifications);
    const [filter, setFilter] = useState('all'); // all, unread, read
    const [selectedType, setSelectedType] = useState('all'); // all, reminder, family, tree, birthday

    const getIcon = (iconType) => {
        switch (iconType) {
            case 'calendar':
                return <Calendar className="w-5 h-5" />;
            case 'users':
                return <Users className="w-5 h-5" />;
            case 'tree':
                return <TreePine className="w-5 h-5" />;
            case 'gift':
                return <Gift className="w-5 h-5" />;
            case 'alert':
                return <AlertCircle className="w-5 h-5" />;
            default:
                return <Bell className="w-5 h-5" />;
        }
    };

    const getTypeColor = (type) => {
        switch (type) {
            case 'reminder':
                return 'from-blue-500/20 to-blue-600/20 border-blue-500/30';
            case 'family':
                return 'from-green-500/20 to-green-600/20 border-green-500/30';
            case 'tree':
                return 'from-amber-500/20 to-amber-600/20 border-amber-500/30';
            case 'birthday':
                return 'from-pink-500/20 to-pink-600/20 border-pink-500/30';
            default:
                return 'from-gray-500/20 to-gray-600/20 border-gray-500/30';
        }
    };

    const formatTimestamp = (timestamp) => {
        const now = new Date();
        const time = new Date(timestamp);
        const diff = now.getTime() - time.getTime();
        const minutes = Math.floor(diff / (1000 * 60));
        const hours = Math.floor(diff / (1000 * 60 * 60));
        const days = Math.floor(diff / (1000 * 60 * 60 * 24));

        if (minutes < 60) return `${minutes} phút trước`;
        if (hours < 24) return `${hours} giờ trước`;
        if (days < 7) return `${days} ngày trước`;
        return time.toLocaleDateString('vi-VN');
    };

    const markAsRead = (id) => {
        setNotifications(notifications.map(n =>
            n.id === id ? { ...n, read: true } : n
        ));
    };

    const markAllAsRead = () => {
        setNotifications(notifications.map(n => ({ ...n, read: true })));
    };

    const deleteNotification = (id) => {
        setNotifications(notifications.filter(n => n.id !== id));
    };

    const deleteAllRead = () => {
        setNotifications(notifications.filter(n => !n.read));
    };

    const filteredNotifications = notifications.filter(n => {
        if (filter === 'unread' && n.read) return false;
        if (filter === 'read' && !n.read) return false;
        if (selectedType !== 'all' && n.type !== selectedType) return false;
        return true;
    });

    const unreadCount = notifications.filter(n => !n.read).length;

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
                            <button
                                onClick={() => setFilter('all')}
                                className={`px-4 py-2 rounded-lg text-sm font-medium transition-all ${
                                    filter === 'all'
                                        ? 'bg-gradient-to-r from-[#b49e7b] to-[#d1b98a] text-[#2a3548] shadow-lg'
                                        : 'bg-white/5 text-white hover:bg-white/10'
                                }`}
                            >
                                Tất cả
                            </button>
                            <button
                                onClick={() => setFilter('unread')}
                                className={`px-4 py-2 rounded-lg text-sm font-medium transition-all ${
                                    filter === 'unread'
                                        ? 'bg-gradient-to-r from-[#b49e7b] to-[#d1b98a] text-[#2a3548] shadow-lg'
                                        : 'bg-white/5 text-white hover:bg-white/10'
                                }`}
                            >
                                Chưa đọc
                            </button>
                            <button
                                onClick={() => setFilter('read')}
                                className={`px-4 py-2 rounded-lg text-sm font-medium transition-all ${
                                    filter === 'read'
                                        ? 'bg-gradient-to-r from-[#b49e7b] to-[#d1b98a] text-[#2a3548] shadow-lg'
                                        : 'bg-white/5 text-white hover:bg-white/10'
                                }`}
                            >
                                Đã đọc
                            </button>
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
                                <option value="reminder">Nhắc nhở</option>
                                <option value="family">Gia đình</option>
                                <option value="tree">Cây gia phả</option>
                                <option value="birthday">Sinh nhật</option>
                            </select>
                        </div>
                    </div>

                    {notifications.some(n => n.read) && (
                        <button
                            onClick={deleteAllRead}
                            className="flex items-center gap-2 px-3 py-1.5 rounded-lg bg-red-500/10 text-red-300 text-sm hover:bg-red-500/20 transition-all"
                        >
                            <Trash2 className="w-4 h-4" />
                            Xóa thông báo đã đọc
                        </button>
                    )}
                </div>

                {/* Notifications List */}
                <div className="space-y-3">
                    {filteredNotifications.length === 0 ? (
                        <div className="text-center py-16">
                            <Bell className="w-16 h-16 mx-auto mb-4 opacity-20" style={{ color: 'rgb(255, 216, 155)' }} />
                            <p className="text-white/50 text-lg">Không có thông báo nào</p>
                        </div>
                    ) : (
                        filteredNotifications.map((notification) => (
                            <div
                                key={notification.id}
                                className={`rounded-xl border backdrop-blur-sm transition-all hover:scale-[1.02] ${
                                    notification.read ? 'bg-white/5' : `bg-gradient-to-r ${getTypeColor(notification.type)}`
                                }`}
                                style={{
                                    borderColor: notification.read ? 'rgba(255, 216, 155, 0.1)' : undefined
                                }}
                            >
                                <div className="p-4 flex items-start gap-4">
                                    {/* Icon */}
                                    <div className={`w-10 h-10 rounded-lg flex items-center justify-center flex-shrink-0 ${
                                        notification.read ? 'bg-white/10' : 'bg-white/20'
                                    }`}>
                                        <div style={{ color: notification.read ? 'rgb(255, 216, 155)' : '#fff' }}>
                                            {getIcon(notification.icon)}
                                        </div>
                                    </div>

                                    {/* Content */}
                                    <div className="flex-1 min-w-0">
                                        <div className="flex items-start justify-between gap-2 mb-1">
                                            <h3 className="font-semibold text-white text-sm">
                                                {notification.title}
                                            </h3>
                                            {!notification.read && (
                                                <div className="w-2 h-2 rounded-full bg-gradient-to-r from-[#b49e7b] to-[#d1b98a] flex-shrink-0 mt-1.5" />
                                            )}
                                        </div>
                                        <p className="text-sm text-white/70 mb-2">
                                            {notification.message}
                                        </p>
                                        <p className="text-xs" style={{ color: 'rgb(255, 216, 155)', opacity: 0.7 }}>
                                            {formatTimestamp(notification.timestamp)}
                                        </p>
                                    </div>

                                    {/* Actions */}
                                    <div className="flex items-center gap-2 flex-shrink-0">
                                        {!notification.read && (
                                            <button
                                                onClick={() => markAsRead(notification.id)}
                                                className="p-2 rounded-lg bg-white/10 text-white hover:bg-white/20 transition-all"
                                                title="Đánh dấu đã đọc"
                                            >
                                                <Check className="w-4 h-4" />
                                            </button>
                                        )}
                                        <button
                                            onClick={() => deleteNotification(notification.id)}
                                            className="p-2 rounded-lg bg-red-500/10 text-red-300 hover:bg-red-500/20 transition-all"
                                            title="Xóa thông báo"
                                        >
                                            <X className="w-4 h-4" />
                                        </button>
                                    </div>
                                </div>
                            </div>
                        ))
                    )}
                </div>

                {/* Stats */}
                {notifications.length > 0 && (
                    <div className="mt-8 grid grid-cols-1 sm:grid-cols-3 gap-4">
                        <div className="p-4 rounded-xl bg-white/5 border border-white/10">
                            <div className="text-2xl font-bold text-white mb-1">
                                {notifications.length}
                            </div>
                            <div className="text-sm" style={{ color: 'rgb(255, 216, 155)' }}>
                                Tổng thông báo
                            </div>
                        </div>
                        <div className="p-4 rounded-xl bg-white/5 border border-white/10">
                            <div className="text-2xl font-bold text-white mb-1">
                                {unreadCount}
                            </div>
                            <div className="text-sm" style={{ color: 'rgb(255, 216, 155)' }}>
                                Chưa đọc
                            </div>
                        </div>
                        <div className="p-4 rounded-xl bg-white/5 border border-white/10">
                            <div className="text-2xl font-bold text-white mb-1">
                                {notifications.filter(n => n.type === 'reminder').length}
                            </div>
                            <div className="text-sm" style={{ color: 'rgb(255, 216, 155)' }}>
                                Nhắc nhở
                            </div>
                        </div>
                    </div>
                )}
            </div>
        </div>
    );
};

export default NotificationsPage;