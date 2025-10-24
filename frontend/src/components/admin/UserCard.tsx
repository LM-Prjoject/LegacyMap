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
                return 'bg-purple-100 text-purple-800 border-purple-300';
            case 'MODERATOR':
                return 'bg-blue-100 text-blue-800 border-blue-300';
            default:
                return 'bg-gray-100 text-gray-800 border-gray-300';
        }
    };

    const getInitials = (): string => {
        const first = user.firstName?.charAt(0) || '';
        const last = user.lastName?.charAt(0) || '';

        if (first || last) {
            return (first + last).toUpperCase();
        }

        // Fallback to email first character
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
        <div className="bg-white p-6 rounded-lg shadow-md border hover:shadow-lg transition-shadow">
            <div className="flex justify-between items-start">
                <div className="flex-1">
                    {/* User Info */}
                    <div className="flex items-center mb-3">
                        <div className="w-12 h-12 bg-gradient-to-br from-blue-400 to-blue-600 rounded-full flex items-center justify-center text-white font-bold text-lg mr-4">
                            {getInitials()}
                        </div>
                        <div>
                            <h3 className="font-semibold text-lg text-gray-900">
                                {getDisplayName()}
                            </h3>
                            <p className="text-gray-600 text-sm">{user.email}</p>
                        </div>
                    </div>

                    {/* User Details */}
                    <div className="space-y-2 mb-4">
                        <div className="flex items-center text-sm">
                            <span className="text-gray-500 mr-2">Role:</span>
                            <span className={`px-2 py-1 rounded border text-xs font-medium ${getRoleBadgeColor(user.role || user.roleName)}`}>
                                {user.role || user.roleName || 'USER'}
                            </span>
                        </div>

                        <div className="flex items-center text-sm">
                            <span className="text-gray-500 mr-2">Status:</span>
                            <span className={`px-2 py-1 rounded text-xs font-medium ${
                                user.isBanned
                                    ? 'bg-red-100 text-red-800'
                                    : 'bg-green-100 text-green-800'
                            }`}>
                                {user.isBanned ? '🚫 Banned' : '✅ Active'}
                            </span>
                        </div>

                        <div className="text-sm text-gray-500">
                            <span className="mr-2">Joined:</span>
                            <span className="font-medium">{formatDate(user.createdAt)}</span>
                        </div>

                        {user.isBanned && user.bannedAt && (
                            <div className="text-sm text-red-600">
                                <span className="mr-2">Banned on:</span>
                                <span className="font-medium">{formatDate(user.bannedAt)}</span>
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
                            className="px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 transition-colors text-sm"
                        >
                            View Details
                        </button>
                    )}
                </div>
            </div>
        </div>
    );
};

export default UserCard;