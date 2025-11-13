import React from 'react';
import {
    User,
    getUserInitials,
    getUserDisplayName,
    isUserOnline,
    formatUserDate,
    getLastLoginText
} from '../../types/ts_user';
import BanUnbanButton from './BanUnbanButton';

interface UserCardProps {
    user: User;
    onBan: (userId: string) => Promise<void>;
    onUnban: (userId: string) => Promise<void>;
    onViewDetail?: (userId: string) => void;
}

const UserCard: React.FC<UserCardProps> = ({ user, onBan, onUnban, onViewDetail }) => {

    const getRoleBadgeColor = (role: string | undefined) => {
        const roleValue = role || 'USER';
        switch (roleValue.toUpperCase()) {
            case 'ADMIN':
                return 'bg-gradient-to-br from-[#d1b98a] to-[#f4e9c8] text-[#20283d] font-semibold';
            case 'MODERATOR':
                return 'bg-purple-700/20 text-purple-300 border border-purple-400/30';
            default:
                return 'bg-blue-700/20 text-blue-300 border border-blue-400/30';
        }
    };

    const isOnline = isUserOnline(user);

    return (
        <div className="bg-gradient-to-br from-[#1b2233] to-[#2e3a57] p-6 rounded-2xl border border-[#2e3a57] hover:border-[#d1b98a]/60 hover:shadow-lg hover:shadow-[#d1b98a]/20 transition-all duration-300">
            {/* ✅ HEADER: Avatar + Name + Actions trên cùng 1 hàng */}
            <div className="flex items-start justify-between gap-4 mb-4">
                {/* LEFT: Avatar + Basic Info */}
                <div className="flex items-center gap-3 min-w-0 flex-1">
                    <div className="relative flex-shrink-0">
                        <div className="w-16 h-16 rounded-xl flex items-center justify-center bg-gradient-to-br from-[#d1b98a] to-[#f4e9c8] text-[#20283d] font-extrabold text-xl">
                            {getUserInitials(user)}
                        </div>
                        {!user.isBanned && (
                            <div
                                className={`absolute -bottom-1 -right-1 w-4 h-4 rounded-full border-2 border-[#1b2233] ${
                                    isOnline ? 'bg-green-400' : 'bg-gray-500'
                                }`}
                                title={isOnline ? 'Đang online' : 'Offline'}
                            />
                        )}
                    </div>

                    <div className="min-w-0 flex-1">
                        <h3 className="font-bold text-lg text-[#f4e9c8] truncate">
                            {getUserDisplayName(user)}
                        </h3>
                        <p className="text-[#f4e9c8]/70 text-sm truncate">{user.email}</p>
                        <p className="text-xs text-[#f4e9c8]/50 mt-1 truncate">
                            {getLastLoginText(user)}
                        </p>
                    </div>
                </div>

                {/* RIGHT: Action Buttons - Đặt riêng bên phải */}
                <div className="flex flex-col gap-2 flex-shrink-0 w-[140px]">
                    <BanUnbanButton
                        userId={user.id}
                        isBanned={user.isBanned}
                        role={user.role || user.roleName}
                        onBan={onBan}
                        onUnban={onUnban}
                    />

                    {onViewDetail && (
                        <button
                            onClick={() => onViewDetail(user.id)}
                            className="w-full px-4 py-2 rounded-lg text-sm font-medium bg-gradient-to-br from-[#d1b98a] to-[#f4e9c8] text-[#20283d] hover:from-[#f4e9c8] hover:to-[#ffffff] hover:scale-105 transition-all duration-300 shadow-md shadow-black/20 whitespace-nowrap"
                        >
                            Chi tiết
                        </button>
                    )}
                </div>
            </div>

            {/* ✅ DETAILS: Thông tin chi tiết bên dưới */}
            <div className="space-y-2 pt-4 border-t border-[#2e3a57]">
                <div className="flex items-center text-sm">
                    <span className="text-[#f4e9c8]/60 mr-2 w-24 flex-shrink-0">Vai trò:</span>
                    <span className={`px-3 py-1 rounded-lg text-xs font-semibold whitespace-nowrap ${getRoleBadgeColor(user.role || user.roleName)}`}>
                        {user.role || user.roleName || 'USER'}
                    </span>
                </div>

                <div className="flex items-center text-sm">
                    <span className="text-[#f4e9c8]/60 mr-2 w-24 flex-shrink-0">Trạng thái:</span>
                    <span className={`px-3 py-1 rounded-lg text-xs font-semibold whitespace-nowrap ${
                        user.isBanned
                            ? 'bg-red-800/30 text-red-300 border border-red-500/30'
                            : 'bg-green-700/30 text-green-300 border border-green-400/30'
                    }`}>
                        {user.isBanned ? '🚫 Đã khóa' : '✅ Hoạt động'}
                    </span>
                </div>

                <div className="flex items-start text-sm">
                    <span className="text-[#f4e9c8]/60 mr-2 w-24 flex-shrink-0">Tham gia:</span>
                    <span className="text-[#f4e9c8]">{formatUserDate(user.createdAt)}</span>
                </div>

                {user.isBanned && user.bannedAt && (
                    <div className="text-sm bg-red-900/20 px-3 py-2 rounded-lg border border-red-700/30 mt-2">
                        <span className="text-red-300">⚠️ Bị khóa: {formatUserDate(user.bannedAt)}</span>
                    </div>
                )}
            </div>
        </div>
    );
};

export default UserCard;