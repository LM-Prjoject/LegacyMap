// src/pages/admin/UserDetail.tsx
import React, { useState, useEffect } from 'react';
import { useParams, useNavigate } from 'react-router-dom';
import AdminLayout from '../../components/admin/AdminLayout';
import { User } from '../../types/ts_user';
import { useUsers } from '../../hooks/useUsers';

const UserDetail: React.FC = () => {
    const { userId } = useParams<{ userId: string }>();
    const navigate = useNavigate();
    const { users, banUser, unbanUser, loading } = useUsers();
    const [user, setUser] = useState<User | null>(null);
    const [showBanModal, setShowBanModal] = useState(false);
    const [actionLoading, setActionLoading] = useState(false);

    useEffect(() => {
        if (users.length > 0 && userId) {
            const foundUser = users.find((u: User) => u.id === userId);
            if (foundUser) {
                setUser(foundUser);
            } else {
                navigate('/admin/users');
            }
        }
    }, [users, userId, navigate]);

    const handleBanToggle = async () => {
        if (!user) return;

        setActionLoading(true);
        try {
            if (user.isBanned) {
                await unbanUser(user.id);
            } else {
                await banUser(user.id);
            }
            setShowBanModal(false);
        } catch (error) {
            console.error('Error toggling ban status:', error);
        } finally {
            setActionLoading(false);
        }
    };

    const getInitials = (user: User): string => {
        const first = user.firstName?.charAt(0) || '';
        const last = user.lastName?.charAt(0) || '';

        if (first || last) {
            return (first + last).toUpperCase();
        }

        return user.email?.charAt(0).toUpperCase() || 'U';
    };

    const getDisplayName = (user: User): string => {
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

    const getRoleDisplay = (user: User): string => {
        return user.role || user.roleName || 'USER';
    };

    if (loading || !user) {
        return (
            <AdminLayout>
                <div className="flex justify-center items-center h-64">
                    <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600"></div>
                </div>
            </AdminLayout>
        );
    }

    return (
        <AdminLayout>
            <div>
                {/* Header */}
                <div className="mb-6">
                    <button
                        onClick={() => navigate('/admin/users')}
                        className="flex items-center text-gray-600 hover:text-gray-900 mb-4"
                    >
                        <svg className="w-5 h-5 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M15 19l-7-7 7-7" />
                        </svg>
                        Back to Users
                    </button>
                    <div className="flex items-center justify-between">
                        <div className="flex items-center">
                            <div className="w-16 h-16 bg-gradient-to-br from-blue-400 to-blue-600 rounded-full flex items-center justify-center text-white text-2xl font-bold mr-4">
                                {getInitials(user)}
                            </div>
                            <div>
                                <h2 className="text-3xl font-bold text-gray-900">
                                    {getDisplayName(user)}
                                </h2>
                                <p className="text-gray-600">{user.email}</p>
                            </div>
                        </div>
                        <div className="flex items-center space-x-3">
                            <span className={`px-4 py-2 rounded-lg text-sm font-medium ${
                                user.isBanned
                                    ? 'bg-red-100 text-red-800'
                                    : 'bg-green-100 text-green-800'
                            }`}>
                                {user.isBanned ? 'Banned' : 'Active'}
                            </span>
                            <button
                                onClick={() => setShowBanModal(true)}
                                className={`px-4 py-2 rounded-lg text-white transition-colors ${
                                    user.isBanned
                                        ? 'bg-green-600 hover:bg-green-700'
                                        : 'bg-red-600 hover:bg-red-700'
                                }`}
                            >
                                {user.isBanned ? 'Unban User' : 'Ban User'}
                            </button>
                        </div>
                    </div>
                </div>

                {/* Main Content Grid */}
                <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
                    {/* User Information */}
                    <div className="lg:col-span-2 space-y-6">
                        {/* Basic Info */}
                        <div className="bg-white rounded-lg shadow p-6">
                            <h3 className="text-xl font-semibold mb-4">Basic Information</h3>
                            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                                <InfoField label="First Name" value={user.firstName || 'Not provided'} />
                                <InfoField label="Last Name" value={user.lastName || 'Not provided'} />
                                <InfoField label="Email" value={user.email} />
                                <InfoField label="Username" value={user.username || 'Not provided'} />
                                <InfoField label="Role" value={getRoleDisplay(user)} />
                                <InfoField
                                    label="Account Created"
                                    value={user.createdAt ? new Date(user.createdAt).toLocaleDateString('en-US', {
                                        year: 'numeric',
                                        month: 'long',
                                        day: 'numeric'
                                    }) : 'Unknown'}
                                />
                                <InfoField
                                    label="Last Updated"
                                    value={user.updatedAt ? new Date(user.updatedAt).toLocaleDateString('en-US', {
                                        year: 'numeric',
                                        month: 'long',
                                        day: 'numeric'
                                    }) : 'Unknown'}
                                />
                                <InfoField
                                    label="Last Login"
                                    value={user.lastLogin ? new Date(user.lastLogin).toLocaleDateString('en-US', {
                                        year: 'numeric',
                                        month: 'long',
                                        day: 'numeric',
                                        hour: '2-digit',
                                        minute: '2-digit'
                                    }) : 'Never'}
                                />
                            </div>
                        </div>

                        {/* Account Status */}
                        <div className="bg-white rounded-lg shadow p-6">
                            <h3 className="text-xl font-semibold mb-4">Account Status</h3>
                            <div className="space-y-4">
                                <StatusItem
                                    label="Account Status"
                                    value={user.isBanned ? 'Banned' : 'Active'}
                                    status={user.isBanned ? 'danger' : 'success'}
                                />
                                <StatusItem
                                    label="User Role"
                                    value={getRoleDisplay(user)}
                                    status={getRoleDisplay(user) === 'ADMIN' ? 'warning' : 'info'}
                                />
                                <StatusItem
                                    label="Email Verified"
                                    value={user.isVerified ? "Yes" : "No"}
                                    status={user.isVerified ? "success" : "danger"}
                                />
                                <StatusItem
                                    label="Account Active"
                                    value={user.isActive ? "Yes" : "No"}
                                    status={user.isActive ? "success" : "danger"}
                                />
                            </div>
                        </div>

                        {/* Activity Log */}
                        <div className="bg-white rounded-lg shadow p-6">
                            <h3 className="text-xl font-semibold mb-4">Recent Activity</h3>
                            <div className="space-y-3">
                                <ActivityItem
                                    action="Account created"
                                    date={user.createdAt}
                                    icon="🎉"
                                />
                                {user.updatedAt && (
                                    <ActivityItem
                                        action="Profile updated"
                                        date={user.updatedAt}
                                        icon="✏️"
                                    />
                                )}
                                {user.isBanned && user.bannedAt && (
                                    <ActivityItem
                                        action="Account banned"
                                        date={user.bannedAt}
                                        icon="🚫"
                                    />
                                )}
                                {user.lastLogin && (
                                    <ActivityItem
                                        action="Last login"
                                        date={user.lastLogin}
                                        icon="🔐"
                                    />
                                )}
                            </div>
                        </div>
                    </div>

                    {/* Sidebar */}
                    <div className="space-y-6">
                        {/* Quick Actions */}
                        <div className="bg-white rounded-lg shadow p-6">
                            <h3 className="text-lg font-semibold mb-4">Quick Actions</h3>
                            <div className="space-y-2">
                                <ActionButton
                                    icon="📧"
                                    label="Send Email"
                                    onClick={() => window.location.href = `mailto:${user.email}`}
                                />
                                <ActionButton
                                    icon="🔄"
                                    label="Reset Password"
                                    onClick={() => alert('Reset password functionality')}
                                />
                                <ActionButton
                                    icon="📝"
                                    label="View Logs"
                                    onClick={() => alert('View logs functionality')}
                                />
                                <ActionButton
                                    icon={user.isBanned ? "✅" : "🚫"}
                                    label={user.isBanned ? "Unban User" : "Ban User"}
                                    onClick={() => setShowBanModal(true)}
                                    danger={!user.isBanned}
                                />
                            </div>
                        </div>

                        {/* User Stats */}
                        <div className="bg-white rounded-lg shadow p-6">
                            <h3 className="text-lg font-semibold mb-4">Statistics</h3>
                            <div className="space-y-3">
                                <StatItem label="Family Trees" value="3" />
                                <StatItem label="Total Members" value="47" />
                                <StatItem
                                    label="Last Login"
                                    value={user.lastLogin ? "2 hours ago" : "Never"}
                                />
                                <StatItem
                                    label="Account Age"
                                    value={
                                        user.createdAt
                                            ? Math.floor((Date.now() - new Date(user.createdAt).getTime()) / (1000 * 60 * 60 * 24)) + ' days'
                                            : 'Unknown'
                                    }
                                />
                            </div>
                        </div>
                    </div>
                </div>
            </div>

            {/* Ban Confirmation Modal */}
            {showBanModal && (
                <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
                    <div className="bg-white rounded-lg p-6 max-w-md w-full mx-4">
                        <h3 className="text-xl font-semibold mb-4">
                            {user.isBanned ? 'Unban User' : 'Ban User'}
                        </h3>
                        <p className="text-gray-600 mb-6">
                            {user.isBanned
                                ? `Are you sure you want to unban ${getDisplayName(user)}? They will regain access to the platform.`
                                : `Are you sure you want to ban ${getDisplayName(user)}? This will prevent them from accessing the platform.`
                            }
                        </p>
                        <div className="flex justify-end space-x-3">
                            <button
                                onClick={() => setShowBanModal(false)}
                                className="px-4 py-2 border border-gray-300 rounded-lg hover:bg-gray-50 transition-colors"
                                disabled={actionLoading}
                            >
                                Cancel
                            </button>
                            <button
                                onClick={handleBanToggle}
                                className={`px-4 py-2 rounded-lg text-white transition-colors ${
                                    user.isBanned
                                        ? 'bg-green-600 hover:bg-green-700'
                                        : 'bg-red-600 hover:bg-red-700'
                                } ${actionLoading ? 'opacity-50 cursor-not-allowed' : ''}`}
                                disabled={actionLoading}
                            >
                                {actionLoading ? 'Processing...' : user.isBanned ? 'Unban' : 'Ban User'}
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </AdminLayout>
    );
};

// Helper Components
const InfoField: React.FC<{ label: string; value: string }> = ({ label, value }) => (
    <div>
        <label className="text-sm font-medium text-gray-500">{label}</label>
        <p className="text-gray-900 mt-1">{value}</p>
    </div>
);

const StatusItem: React.FC<{ label: string; value: string; status: 'success' | 'danger' | 'warning' | 'info' }> = ({ label, value, status }) => {
    const statusColors = {
        success: 'bg-green-100 text-green-800',
        danger: 'bg-red-100 text-red-800',
        warning: 'bg-yellow-100 text-yellow-800',
        info: 'bg-blue-100 text-blue-800',
    };

    return (
        <div className="flex items-center justify-between p-3 bg-gray-50 rounded-lg">
            <span className="text-gray-700 font-medium">{label}</span>
            <span className={`px-3 py-1 rounded-full text-sm font-medium ${statusColors[status]}`}>
                {value}
            </span>
        </div>
    );
};

const ActivityItem: React.FC<{ action: string; date: string | undefined; icon: string }> = ({ action, date, icon }) => (
    <div className="flex items-start p-3 hover:bg-gray-50 rounded-lg transition-colors">
        <div className="text-2xl mr-3">{icon}</div>
        <div className="flex-1">
            <p className="font-medium text-gray-900">{action}</p>
            <p className="text-sm text-gray-500">
                {date ? new Date(date).toLocaleDateString('en-US', {
                    year: 'numeric',
                    month: 'long',
                    day: 'numeric',
                    hour: '2-digit',
                    minute: '2-digit'
                }) : 'Unknown date'}
            </p>
        </div>
    </div>
);

const ActionButton: React.FC<{ icon: string; label: string; onClick: () => void; danger?: boolean }> = ({ icon, label, onClick, danger }) => (
    <button
        onClick={onClick}
        className={`w-full flex items-center p-3 rounded-lg transition-colors ${
            danger
                ? 'hover:bg-red-50 text-red-700'
                : 'hover:bg-gray-100 text-gray-700'
        }`}
    >
        <span className="text-xl mr-3">{icon}</span>
        <span className="font-medium">{label}</span>
    </button>
);

const StatItem: React.FC<{ label: string; value: string }> = ({ label, value }) => (
    <div className="flex justify-between items-center">
        <span className="text-gray-600">{label}</span>
        <span className="font-semibold text-gray-900">{value}</span>
    </div>
);

export default UserDetail;