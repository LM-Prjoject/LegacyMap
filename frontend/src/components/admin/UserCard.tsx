// src/components/admin/UserCard.tsx
import React from 'react';
import { User } from '../../types/ts_user';
import BanUnbanButton from './BanUnbanButton';

interface UserCardProps {
    user: User;
    onBan: (userId: string) => Promise<void>;
    onUnban: (userId: string) => Promise<void>;
    onViewDetail?: (userId: string) => void;
}

const UserCard: React.FC<UserCardProps> = ({ user, onBan, onUnban, onViewDetail }) => {
    const formatDate = (dateString: string | undefined) => {
        if (!dateString) return 'N/A';
        return new Date(dateString).toLocaleDateString('vi-VN', {
            year: 'numeric',
            month: 'long',
            day: 'numeric'
        });
    };

    const getRoleBadgeColor = (role: string | undefined) => {
        const roleValue = role || 'USER';
        switch (roleValue) {
            case 'ADMIN':
                return 'bg-[#D1B066] text-[#084289]';
            case 'MODERATOR':
                return 'bg-purple-500/20 text-purple-300';
            default:
                return 'bg-blue-500/20 text-blue-300';
        }
    };

    const getInitials = (): string => {
        const first = user.firstName?.charAt(0) || '';
        const last = user.lastName?.charAt(0) || '';

        if (first || last) {
            return (first + last).toUpperCase();
        }

        return user.email?.charAt(0).toUpperCase() || 'U';
    };

    const getDisplayName = (): string => {
        if (user.firstName && user.lastName) {
            return `${user.firstName} ${user.lastName}`;
        }
        if (user.firstName) {
            return user.firstName;
        }
        if (user.lastName) {
            return user.lastName;
        }
        return user.username || user.email || 'Unknown User';
    };

    return (
        <div className="bg-[#084289] p-6 rounded-xl border border-[#0a4a9e] hover:border-[#D1B066]/50 transition-all shadow-lg">
            <div className="flex justify-between items-start">
                <div className="flex-1">
                    {/* User Info */}
                    <div className="flex items-center mb-4">
                        <div className="relative">
                            <div className="w-16 h-16 bg-[#D1B066] rounded-xl flex items-center justify-center text-white font-bold text-xl mr-4">
                                {getInitials()}
                            </div>
                            {!user.isBanned && (
                                <div className="absolute -bottom-1 -right-1 w-4 h-4 bg-green-500 rounded-full border-2 border-[#084289]"></div>
                            )}
                        </div>
                        <div>
                            <h3 className="font-bold text-lg text-white">
                                {getDisplayName()}
                            </h3>
                            <p className="text-white/60 text-sm">{user.email}</p>
                        </div>
                    </div>

                    {/* User Details */}
                    <div className="space-y-2 mb-4">
                        <div className="flex items-center text-sm">
                            <span className="text-white/60 mr-2 w-24">Vai trò:</span>
                            <span className={`px-3 py-1 rounded-lg text-xs font-semibold ${getRoleBadgeColor(user.role || user.roleName)}`}>
                                {user.role || user.roleName || 'USER'}
                            </span>
                        </div>

                        <div className="flex items-center text-sm">
                            <span className="text-white/60 mr-2 w-24">Trạng thái:</span>
                            <span className={`px-3 py-1 rounded-lg text-xs font-semibold ${
                                user.isBanned
                                    ? 'bg-red-500/20 text-red-300'
                                    : 'bg-green-500/20 text-green-300'
                            }`}>
                                {user.isBanned ? '🚫 Đã khóa' : '✅ Hoạt động'}
                            </span>
                        </div>

                        <div className="text-sm text-white/60">
                            <span className="mr-2 w-24 inline-block">Tham gia:</span>
                            <span className="text-white/80">{formatDate(user.createdAt)}</span>
                        </div>

                        {user.isBanned && user.bannedAt && (
                            <div className="text-sm bg-red-500/10 px-3 py-2 rounded-lg border border-red-400/30 mt-2">
                                <span className="text-red-300">⚠️ Bị khóa: {formatDate(user.bannedAt)}</span>
                            </div>
                        )}
                    </div>
                </div>

                {/* Actions */}
                <div className="flex flex-col space-y-2 ml-4">
                    <BanUnbanButton
                        userId={user.id}
                        isBanned={user.isBanned}
                        onBan={onBan}
                        onUnban={onUnban}
                    />

                    {onViewDetail && (
                        <button
                            onClick={() => onViewDetail(user.id)}
                            className="px-4 py-2 bg-[#D1B066] text-[#084289] rounded-lg hover:bg-[#f4d88a] font-medium transition-all text-sm"
                        >
                            Chi tiết
                        </button>
                    )}
                </div>
            </div>
        </div>
    );
};

export default UserCard;