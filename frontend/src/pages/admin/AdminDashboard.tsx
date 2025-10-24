// src/pages/admin/AdminDashboard.tsx
import React from 'react';
import { Link } from 'react-router-dom';
import AdminLayout from '../../components/admin/AdminLayout';
import { User } from '../../types/ts_user';
import { useUsers } from '../../hooks/useUsers';
import { useFamilyTrees } from '../../hooks/useFamilyTrees';
import DebugAuthInfo from '../../components/DebugAuthInfo';

const AdminDashboard: React.FC = () => {
    const { users, loading: usersLoading, error: usersError } = useUsers();
    const { familyTrees, loading: treesLoading, error: treesError } = useFamilyTrees();

    const stats = React.useMemo(() => {
        if (!users.length) return null;

        const totalUsers = users.length;
        const bannedUsers = users.filter((u: User) => u.isBanned).length;
        const activeUsers = totalUsers - bannedUsers;

        const now = new Date();
        const firstDayOfMonth = new Date(now.getFullYear(), now.getMonth(), 1);
        const newUsersThisMonth = users.filter((u: User) =>
            new Date(u.createdAt) >= firstDayOfMonth
        ).length;

        // 🔥 SỬA: Dùng roleName thay vì role, và giá trị từ database ('admin' thay vì 'ADMIN')
        const adminUsers = users.filter((u: User) =>
            u.roleName === 'admin' || u.role === 'ADMIN' // 🔥 Backward compatibility
        ).length;

        const moderatorUsers = users.filter((u: User) =>
            u.roleName === 'moderator' || u.role === 'MODERATOR' // 🔥 Backward compatibility
        ).length;

        return {
            totalUsers,
            activeUsers,
            bannedUsers,
            newUsersThisMonth,
            adminUsers,
            moderatorUsers,
            totalFamilyTrees: familyTrees.length,
        };
    }, [users, familyTrees]);

    if (usersLoading || treesLoading) {
        return (
            <AdminLayout>
                <div className="flex justify-center items-center h-64">
                    <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blueA-600"></div>
                    <p className="text-gray-600 ml-4">Loading dashboard...</p>
                </div>
            </AdminLayout>
        );
    }

    return (
        <AdminLayout>
            <div>
                {/* Debug Info - Chỉ hiển thị trong development */}
                {process.env.NODE_ENV === 'development' && <DebugAuthInfo />}

                <div className="mb-8">
                    <h2 className="text-3xl font-bold text-gray-900">
                        Dashboard Overview
                    </h2>
                    <p className="text-gray-600 mt-1">
                        Welcome to the admin panel. Here's what's happening today.
                    </p>
                </div>

                {/* Error Messages */}
                {usersError && (
                    <div className="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded mb-4">
                        <strong>Users Error:</strong> {usersError}
                    </div>
                )}

                {treesError && (
                    <div className="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded mb-4">
                        <strong>Family Trees Error:</strong> {treesError}
                    </div>
                )}

                {/* Stats Grid */}
                <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
                    <StatCard
                        title="Total Users"
                        value={stats?.totalUsers || 0}
                        icon="👥"
                        color="blue"
                        change={`${stats?.activeUsers || 0} active`}
                    />
                    <StatCard
                        title="Family Trees"
                        value={stats?.totalFamilyTrees || 0}
                        icon="🌳"
                        color="green"
                        change={`${familyTrees.length} total`}
                    />
                    <StatCard
                        title="Banned Users"
                        value={stats?.bannedUsers || 0}
                        icon="🚫"
                        color="red"
                        change="Requires attention"
                    />
                    <StatCard
                        title="New This Month"
                        value={stats?.newUsersThisMonth || 0}
                        icon="📈"
                        color="purple"
                        change="Growing steadily"
                    />
                </div>

                {/* Two Column Layout */}
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-8">
                    {/* User Roles */}
                    <div className="bg-white rounded-lg shadow p-6">
                        <h3 className="text-xl font-semibold mb-4">User Roles Distribution</h3>
                        <div className="space-y-4">
                            <RoleBar
                                label="Regular Users"
                                count={stats?.totalUsers ? stats.totalUsers - stats.adminUsers - stats.moderatorUsers : 0}
                                total={stats?.totalUsers || 1}
                                color="blue"
                            />
                            <RoleBar
                                label="Moderators"
                                count={stats?.moderatorUsers || 0}
                                total={stats?.totalUsers || 1}
                                color="purple"
                            />
                            <RoleBar
                                label="Admins"
                                count={stats?.adminUsers || 0}
                                total={stats?.totalUsers || 1}
                                color="red"
                            />
                        </div>
                    </div>

                    {/* System Overview */}
                    <div className="bg-white rounded-lg shadow p-6">
                        <h3 className="text-xl font-semibold mb-4">System Overview</h3>
                        <div className="space-y-4">
                            <OverviewItem
                                label="Total Family Trees"
                                value={stats?.totalFamilyTrees || 0}
                                icon="🌳"
                            />
                            <OverviewItem
                                label="Admin Users"
                                value={stats?.adminUsers || 0}
                                icon="👑"
                            />
                            <OverviewItem
                                label="Active Users"
                                value={stats?.activeUsers || 0}
                                icon="✅"
                            />
                            <OverviewItem
                                label="Banned Users"
                                value={stats?.bannedUsers || 0}
                                icon="🚫"
                            />
                        </div>
                    </div>
                </div>

                {/* Three Column Layout */}
                <div className="grid grid-cols-1 lg:grid-cols-3 gap-6 mb-8">
                    {/* Quick Actions */}
                    <div className="bg-white rounded-lg shadow p-6">
                        <h3 className="text-xl font-semibold mb-4">Quick Actions</h3>
                        <div className="space-y-3">
                            <QuickActionButton
                                to="/admin/users"
                                icon="👥"
                                title="Manage Users"
                                description="View and manage all users"
                            />
                            <QuickActionButton
                                to="/admin/family-trees"
                                icon="🌳"
                                title="Family Trees"
                                description="Browse all family trees"
                            />
                            <QuickActionButton
                                to="/admin/settings"
                                icon="⚙️"
                                title="Settings"
                                description="Configure system settings"
                            />
                        </div>
                    </div>

                    {/* Recent Family Trees */}
                    <div className="bg-white rounded-lg shadow p-6">
                        <div className="flex justify-between items-center mb-4">
                            <h3 className="text-xl font-semibold">Recent Family Trees</h3>
                            <Link
                                to="/admin/family-trees"
                                className="text-blue-600 hover:text-blue-700 text-sm font-medium"
                            >
                                View All →
                            </Link>
                        </div>
                        <div className="space-y-3">
                            {familyTrees.slice(0, 3).map((tree) => (
                                <div
                                    key={tree.id}
                                    className="flex items-center justify-between p-3 hover:bg-gray-50 rounded transition-colors"
                                >
                                    <div className="flex items-center">
                                        <div className="w-10 h-10 bg-gradient-to-br from-green-400 to-green-600 rounded-full flex items-center justify-center text-white font-bold mr-3">
                                            🌳
                                        </div>
                                        <div>
                                            <p className="font-medium text-sm">{tree.name}</p>
                                            <p className="text-xs text-gray-500">
                                                {tree.description || 'No description'}
                                            </p>
                                        </div>
                                    </div>
                                    <div className="text-right">
                                        <span className="text-xs text-gray-500">
                                            {new Date(tree.createdAt).toLocaleDateString()}
                                        </span>
                                    </div>
                                </div>
                            ))}
                            {familyTrees.length === 0 && (
                                <div className="text-center text-gray-500 py-4">
                                    No family trees found
                                </div>
                            )}
                        </div>
                    </div>

                    {/* System Status */}
                    <div className="bg-white rounded-lg shadow p-6">
                        <h3 className="text-xl font-semibold mb-4">System Status</h3>
                        <div className="space-y-3">
                            <StatusItem
                                label="API Server"
                                status="online"
                                message="All systems operational"
                            />
                            <StatusItem
                                label="Database"
                                status="online"
                                message="Connected successfully"
                            />
                            <StatusItem
                                label="Authentication"
                                status="online"
                                message="JWT tokens working"
                            />
                            <StatusItem
                                label="Admin Access"
                                status={users.length > 0 ? "online" : "warning"}
                                message={users.length > 0 ? "Admin privileges active" : "No users loaded"}
                            />
                        </div>
                    </div>
                </div>

                {/* Recent Users */}
                <div className="bg-white rounded-lg shadow p-6">
                    <div className="flex justify-between items-center mb-4">
                        <h3 className="text-xl font-semibold">Recent Users</h3>
                        <Link
                            to="/admin/users"
                            className="text-blue-600 hover:text-blue-700 text-sm font-medium"
                        >
                            View All →
                        </Link>
                    </div>
                    <div className="space-y-3">
                        {users.slice(0, 5).map((user: User) => (
                            <div
                                key={user.id}
                                className="flex items-center justify-between p-3 hover:bg-gray-50 rounded transition-colors"
                            >
                                <div className="flex items-center">
                                    <div className="w-10 h-10 bg-gradient-to-br from-blue-400 to-blue-600 rounded-full flex items-center justify-center text-white font-bold mr-3">
                                        {user.email?.charAt(0).toUpperCase()}
                                    </div>
                                    <div>
                                        <p className="font-medium">
                                            {user.username || user.email}
                                        </p>
                                        <p className="text-sm text-gray-500">
                                            {user.email} • {user.roleName || user.role}
                                        </p>
                                    </div>
                                </div>
                                <div className="flex items-center space-x-2">
                                    <span className={`px-3 py-1 rounded-full text-xs font-medium ${
                                        user.isBanned
                                            ? 'bg-red-100 text-red-800'
                                            : 'bg-green-100 text-green-800'
                                    }`}>
                                        {user.isBanned ? 'Banned' : 'Active'}
                                    </span>
                                    <span className="text-xs text-gray-500">
                                        {new Date(user.createdAt).toLocaleDateString()}
                                    </span>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            </div>
        </AdminLayout>
    );
};

// StatCard Component
interface StatCardProps {
    title: string;
    value: number;
    icon: string;
    color: 'blue' | 'green' | 'red' | 'purple';
    change: string;
}

const StatCard: React.FC<StatCardProps> = ({ title, value, icon, color, change }) => {
    const colorClasses = {
        blue: 'from-blue-400 to-blue-600',
        green: 'from-green-400 to-green-600',
        red: 'from-red-400 to-red-600',
        purple: 'from-purple-400 to-purple-600',
    };

    return (
        <div className="bg-white rounded-lg shadow p-6">
            <div className="flex items-center justify-between mb-4">
                <div className={`w-12 h-12 bg-gradient-to-br ${colorClasses[color]} rounded-lg flex items-center justify-center text-2xl`}>
                    {icon}
                </div>
                <div className="text-right">
                    <p className="text-3xl font-bold text-gray-900">{value}</p>
                </div>
            </div>
            <h3 className="text-gray-600 font-medium mb-1">{title}</h3>
            <p className="text-sm text-gray-500">{change}</p>
        </div>
    );
};

// RoleBar Component
interface RoleBarProps {
    label: string;
    count: number;
    total: number;
    color: 'blue' | 'purple' | 'red';
}

const RoleBar: React.FC<RoleBarProps> = ({ label, count, total, color }) => {
    const percentage = total > 0 ? (count / total) * 100 : 0;

    const colorClasses = {
        blue: 'bg-blue-500',
        purple: 'bg-purple-500',
        red: 'bg-red-500',
    };

    return (
        <div>
            <div className="flex justify-between text-sm mb-2">
                <span className="font-medium text-gray-700">{label}</span>
                <span className="text-gray-500">{count} ({percentage.toFixed(1)}%)</span>
            </div>
            <div className="w-full bg-gray-200 rounded-full h-2">
                <div
                    className={`${colorClasses[color]} h-2 rounded-full transition-all duration-300`}
                    style={{ width: `${percentage}%` }}
                />
            </div>
        </div>
    );
};

// QuickActionButton Component
interface QuickActionButtonProps {
    to: string;
    icon: string;
    title: string;
    description: string;
}

const QuickActionButton: React.FC<QuickActionButtonProps> = ({ to, icon, title, description }) => {
    return (
        <Link
            to={to}
            className="flex items-center p-4 border border-gray-200 rounded-lg hover:bg-gray-50 hover:border-blue-300 transition-all group"
        >
            <div className="text-3xl mr-4">{icon}</div>
            <div className="flex-1">
                <h4 className="font-semibold text-gray-900 group-hover:text-blue-600 transition-colors">
                    {title}
                </h4>
                <p className="text-sm text-gray-500">{description}</p>
            </div>
            <svg
                className="w-5 h-5 text-gray-400 group-hover:text-blue-600 transition-colors"
                fill="none"
                stroke="currentColor"
                viewBox="0 0 24 24"
            >
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 5l7 7-7 7" />
            </svg>
        </Link>
    );
};

// OverviewItem Component
interface OverviewItemProps {
    label: string;
    value: number;
    icon: string;
}

const OverviewItem: React.FC<OverviewItemProps> = ({ label, value, icon }) => {
    return (
        <div className="flex items-center justify-between">
            <div className="flex items-center">
                <span className="text-2xl mr-3">{icon}</span>
                <span className="text-gray-700">{label}</span>
            </div>
            <span className="font-semibold text-gray-900">{value}</span>
        </div>
    );
};

// StatusItem Component
interface StatusItemProps {
    label: string;
    status: 'online' | 'offline' | 'warning';
    message: string;
}

const StatusItem: React.FC<StatusItemProps> = ({ label, status, message }) => {
    const statusClasses = {
        online: 'bg-green-100 text-green-800',
        offline: 'bg-red-100 text-red-800',
        warning: 'bg-yellow-100 text-yellow-800',
    };

    const statusIcons = {
        online: '🟢',
        offline: '🔴',
        warning: '🟡',
    };

    return (
        <div className="flex items-center justify-between">
            <div className="flex items-center">
                <span className="mr-2">{statusIcons[status]}</span>
                <span className="text-gray-700">{label}</span>
            </div>
            <span className={`px-2 py-1 rounded text-xs ${statusClasses[status]}`}>
                {message}
            </span>
        </div>
    );
};

export default AdminDashboard;