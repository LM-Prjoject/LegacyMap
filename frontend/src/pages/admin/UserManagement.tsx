// src/pages/admin/UserManagement.tsx
import React from 'react';
import AdminLayout from '../../components/admin/AdminLayout';
import UserList from '../../components/admin/UserList';
import { User } from '../../types/ts_user';
import { useUsers } from '../../hooks/useUsers';

const UserManagement: React.FC = () => {
    const { users, loading, error, banUser, unbanUser, refreshUsers } = useUsers();

    if (loading) {
        return (
            <AdminLayout>
                <div className="flex flex-col justify-center items-center h-64">
                    <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mb-4"></div>
                    <p className="text-gray-600">Loading users...</p>
                </div>
            </AdminLayout>
        );
    }

    if (error) {
        return (
            <AdminLayout>
                <div className="bg-red-50 border border-red-200 rounded-lg p-6">
                    <h3 className="text-red-800 font-semibold mb-2">Error Loading Users</h3>
                    <p className="text-red-600">{error}</p>
                    <button
                        onClick={refreshUsers}
                        className="mt-4 px-4 py-2 bg-red-600 text-white rounded hover:bg-red-700 transition-colors"
                    >
                        Try Again
                    </button>
                </div>
            </AdminLayout>
        );
    }

    return (
        <AdminLayout>
            <div>
                {/* Header */}
                <div className="flex justify-between items-center mb-8">
                    <div>
                        <h2 className="text-3xl font-bold text-gray-900">
                            User Management
                        </h2>
                        <p className="text-gray-600 mt-1">
                            Manage and monitor all users in the system
                        </p>
                    </div>
                    <button
                        onClick={refreshUsers}
                        className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors flex items-center"
                    >
                        <svg
                            className="w-4 h-4 mr-2"
                            fill="none"
                            stroke="currentColor"
                            viewBox="0 0 24 24"
                        >
                            <path
                                strokeLinecap="round"
                                strokeLinejoin="round"
                                strokeWidth={2}
                                d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"
                            />
                        </svg>
                        Refresh
                    </button>
                </div>

                {/* Stats Summary */}
                <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-6">
                    <StatCard
                        label="Total Users"
                        value={users.length}
                        icon="👥"
                        color="blue"
                    />
                    <StatCard
                        label="Active Users"
                        value={users.filter((u: User) => !u.isBanned).length}
                        icon="✅"
                        color="green"
                    />
                    <StatCard
                        label="Banned Users"
                        value={users.filter((u: User) => u.isBanned).length}
                        icon="🚫"
                        color="red"
                    />
                    <StatCard
                        label="Admins"
                        value={users.filter((u: User) => u.role === 'ADMIN').length}
                        icon="👑"
                        color="purple"
                    />
                </div>

                {/* User List */}
                <UserList
                    users={users}
                    onBan={banUser}
                    onUnban={unbanUser}
                />
            </div>
        </AdminLayout>
    );
};

interface StatCardProps {
    label: string;
    value: number;
    icon: string;
    color: 'blue' | 'green' | 'red' | 'purple';
}

const StatCard: React.FC<StatCardProps> = ({ label, value, icon, color }) => {
    const colorClasses = {
        blue: 'bg-blue-50 border-blue-200 text-blue-600',
        green: 'bg-green-50 border-green-200 text-green-600',
        red: 'bg-red-50 border-red-200 text-red-600',
        purple: 'bg-purple-50 border-purple-200 text-purple-600',
    };

    return (
        <div className={`${colorClasses[color]} border rounded-lg p-4`}>
            <div className="flex items-center justify-between">
                <div>
                    <p className="text-sm font-medium opacity-80">{label}</p>
                    <p className="text-2xl font-bold mt-1">{value}</p>
                </div>
                <div className="text-3xl">{icon}</div>
            </div>
        </div>
    );
};

export default UserManagement;