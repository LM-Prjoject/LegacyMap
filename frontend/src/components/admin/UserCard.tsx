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
            day: 'numeric',
        });
    };

    const getRoleBadgeColor = (role: string | undefined) => {
        const roleValue = role || 'USER';
        switch (roleValue) {
            case 'ADMIN':
                return 'bg-gradient-to-br from-[#d1b98a] to-[#f4e9c8] text-[#20283d] font-semibold';
            case 'MODERATOR':
                return 'bg-purple-700/20 text-purple-300 border border-purple-400/30';
            default:
                return 'bg-blue-700/20 text-blue-300 border border-blue-400/30';
        }
    };

    const getInitials = (): string => {
        const first = user.firstName?.charAt(0) || '';
        const last = user.lastName?.charAt(0) || '';
        if (first || last) return (first + last).toUpperCase();
        return user.email?.charAt(0).toUpperCase() || 'U';
    };

    const getDisplayName = (): string => {
        if (user.firstName && user.lastName) return `${user.firstName} ${user.lastName}`;
        if (user.firstName) return user.firstName;
        if (user.lastName) return user.lastName;
        return user.username || user.email || 'Unknown User';
    };

    return (
        <div
            className="
        bg-gradient-to-br from-[#1b2233] to-[#2e3a57]
        p-6 rounded-2xl border border-[#2e3a57]
        hover:border-[#d1b98a]/60 hover:shadow-lg hover:shadow-[#d1b98a]/20
        transition-all duration-300
      "
        >
            <div className="flex justify-between items-start">
                <div className="flex-1">
                    {/* User Info */}
                    <div className="flex items-center mb-4">
                        <div className="relative">
                            <div
                                className="
                  w-16 h-16 rounded-xl flex items-center justify-center
                  bg-gradient-to-br from-[#d1b98a] to-[#f4e9c8]
                  text-[#20283d] font-extrabold text-xl mr-4
                "
                            >
                                {getInitials()}
                            </div>
                            {!user.isBanned && (
                                <div className="absolute -bottom-1 -right-1 w-4 h-4 bg-green-400 rounded-full border-2 border-[#1b2233]" />
                            )}
                        </div>

                        <div>
                            <h3 className="font-bold text-lg text-[#f4e9c8]">{getDisplayName()}</h3>
                            <p className="text-[#f4e9c8]/70 text-sm">{user.email}</p>
                        </div>
                    </div>

                    {/* User Details */}
                    <div className="space-y-2 mb-4">
                        <div className="flex items-center text-sm">
                            <span className="text-[#f4e9c8]/60 mr-2 w-24">Vai trò:</span>
                            <span
                                className={`px-3 py-1 rounded-lg text-xs font-semibold ${getRoleBadgeColor(
                                    user.role || user.roleName
                                )}`}
                            >
                {user.role || user.roleName || 'USER'}
              </span>
                        </div>

                        <div className="flex items-center text-sm">
                            <span className="text-[#f4e9c8]/60 mr-2 w-24">Trạng thái:</span>
                            <span
                                className={`px-3 py-1 rounded-lg text-xs font-semibold ${
                                    user.isBanned
                                        ? 'bg-red-800/30 text-red-300 border border-red-500/30'
                                        : 'bg-green-700/30 text-green-300 border border-green-400/30'
                                }`}
                            >
                {user.isBanned ? '🚫 Đã khóa' : '✅ Hoạt động'}
              </span>
                        </div>

                        <div className="text-sm text-[#f4e9c8]/80">
                            <span className="mr-2 w-24 inline-block">Tham gia:</span>
                            <span className="text-[#f4e9c8]">{formatDate(user.createdAt)}</span>
                        </div>

                        {user.isBanned && user.bannedAt && (
                            <div className="text-sm bg-red-900/20 px-3 py-2 rounded-lg border border-red-700/30 mt-2">
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
                        role={user.role || user.roleName} // 🔥 TRUYỀN ROLE CHO BUTTON
                        onBan={onBan}
                        onUnban={onUnban}
                    />

                    {onViewDetail && (
                        <button
                            onClick={() => onViewDetail(user.id)}
                            className="
                px-4 py-2 rounded-lg text-sm font-medium
                bg-gradient-to-br from-[#d1b98a] to-[#f4e9c8]
                text-[#20283d] hover:from-[#f4e9c8] hover:to-[#ffffff]
                hover:scale-105 transition-all duration-300
                shadow-md shadow-black/20
              "
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