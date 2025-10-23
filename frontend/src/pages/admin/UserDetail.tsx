// src/pages/admin/UserDetail.tsx
import React, { useState, useEffect } from 'react';
import { useParams, useNavigate } from 'react-router-dom';
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
            <div className="flex justify-center items-center h-64">
                <div className="w-16 h-16 border-4 border-[#D1B066] border-t-transparent rounded-full animate-spin"></div>
            </div>
        );
    }

    return (
        <div>
            {/* Header */}
            <div className="mb-6">
                <button
                    onClick={() => navigate('/admin/users')}
                    className="flex items-center text-white/80 hover:text-white mb-4 transition-colors"
                >
                    <svg className="w-5 h-5 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M15 19l-7-7 7-7" />
                    </svg>
                    Quay lại danh sách
                </button>
                <div className="flex items-center justify-between">
                    <div className="flex items-center">
                        <div className="w-16 h-16 bg-gradient-to-br from-[#D1B066] to-amber-700 rounded-full flex items-center justify-center text-white text-2xl font-bold mr-4">
                            {getInitials(user)}
                        </div>
                        <div>
                            <h2 className="text-3xl font-bold text-white">
                                {getDisplayName(user)}
                            </h2>
                            <p className="text-white/60">{user.email}</p>
                        </div>
                    </div>
                    <div className="flex items-center space-x-3">
                        <span className={`px-4 py-2 rounded-lg text-sm font-medium ${
                            user.isBanned
                                ? 'bg-red-500/20 text-red-300'
                                : 'bg-green-500/20 text-green-300'
                        }`}>
                            {user.isBanned ? 'Đã khóa' : 'Hoạt động'}
                        </span>
                        <button
                            onClick={() => setShowBanModal(true)}
                            className={`px-4 py-2 rounded-lg text-white font-medium transition-colors ${
                                user.isBanned
                                    ? 'bg-green-600 hover:bg-green-700'
                                    : 'bg-red-600 hover:bg-red-700'
                            }`}
                        >
                            {user.isBanned ? 'Mở khóa' : 'Khóa tài khoản'}
                        </button>
                    </div>
                </div>
            </div>

            {/* Main Content Grid */}
            <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
                {/* User Information */}
                <div className="lg:col-span-2 space-y-6">
                    {/* Basic Info */}
                    <div className="bg-[#084289] border border-[#0a4a9e] rounded-xl shadow-lg p-6">
                        <h3 className="text-xl font-semibold text-white mb-4">Thông tin cơ bản</h3>
                        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                            <InfoField label="Họ" value={user.firstName || 'Chưa cung cấp'} />
                            <InfoField label="Tên" value={user.lastName || 'Chưa cung cấp'} />
                            <InfoField label="Email" value={user.email} />
                            <InfoField label="Tên đăng nhập" value={user.username || 'Chưa cung cấp'} />
                            <InfoField label="Vai trò" value={getRoleDisplay(user)} />
                            <InfoField
                                label="Ngày tạo tài khoản"
                                value={user.createdAt ? new Date(user.createdAt).toLocaleDateString('vi-VN', {
                                    year: 'numeric',
                                    month: 'long',
                                    day: 'numeric'
                                }) : 'Không rõ'}
                            />
                            <InfoField
                                label="Cập nhật lần cuối"
                                value={user.updatedAt ? new Date(user.updatedAt).toLocaleDateString('vi-VN', {
                                    year: 'numeric',
                                    month: 'long',
                                    day: 'numeric'
                                }) : 'Không rõ'}
                            />
                            <InfoField
                                label="Đăng nhập lần cuối"
                                value={user.lastLogin ? new Date(user.lastLogin).toLocaleDateString('vi-VN', {
                                    year: 'numeric',
                                    month: 'long',
                                    day: 'numeric',
                                    hour: '2-digit',
                                    minute: '2-digit'
                                }) : 'Chưa đăng nhập'}
                            />
                        </div>
                    </div>

                    {/* Account Status */}
                    <div className="bg-[#084289] border border-[#0a4a9e] rounded-xl shadow-lg p-6">
                        <h3 className="text-xl font-semibold text-white mb-4">Trạng thái tài khoản</h3>
                        <div className="space-y-4">
                            <StatusItem
                                label="Trạng thái"
                                value={user.isBanned ? 'Đã khóa' : 'Hoạt động'}
                                status={user.isBanned ? 'danger' : 'success'}
                            />
                            <StatusItem
                                label="Vai trò"
                                value={getRoleDisplay(user)}
                                status={getRoleDisplay(user) === 'ADMIN' ? 'warning' : 'info'}
                            />
                            <StatusItem
                                label="Email xác thực"
                                value={user.isVerified ? "Đã xác thực" : "Chưa xác thực"}
                                status={user.isVerified ? "success" : "danger"}
                            />
                            <StatusItem
                                label="Tài khoản kích hoạt"
                                value={user.isActive ? "Có" : "Không"}
                                status={user.isActive ? "success" : "danger"}
                            />
                        </div>
                    </div>

                    {/* Activity Log */}
                    <div className="bg-[#084289] border border-[#0a4a9e] rounded-xl shadow-lg p-6">
                        <h3 className="text-xl font-semibold text-white mb-4">Hoạt động gần đây</h3>
                        <div className="space-y-3">
                            <ActivityItem
                                action="Tạo tài khoản"
                                date={user.createdAt}
                                icon="🎉"
                            />
                            {user.updatedAt && (
                                <ActivityItem
                                    action="Cập nhật hồ sơ"
                                    date={user.updatedAt}
                                    icon="✏️"
                                />
                            )}
                            {user.isBanned && user.bannedAt && (
                                <ActivityItem
                                    action="Tài khoản bị khóa"
                                    date={user.bannedAt}
                                    icon="🚫"
                                />
                            )}
                            {user.lastLogin && (
                                <ActivityItem
                                    action="Đăng nhập lần cuối"
                                    date={user.lastLogin}
                                    icon="🔓"
                                />
                            )}
                        </div>
                    </div>
                </div>

                {/* Sidebar */}
                <div className="space-y-6">
                    {/* Quick Actions */}
                    <div className="bg-[#084289] border border-[#0a4a9e] rounded-xl shadow-lg p-6">
                        <h3 className="text-lg font-semibold text-white mb-4">Hành động nhanh</h3>
                        <div className="space-y-2">
                            <ActionButton
                                icon="📧"
                                label="Gửi Email"
                                onClick={() => window.location.href = `mailto:${user.email}`}
                            />
                            <ActionButton
                                icon="🔄"
                                label="Đặt lại mật khẩu"
                                onClick={() => alert('Chức năng đặt lại mật khẩu')}
                            />
                            <ActionButton
                                icon="📜"
                                label="Xem logs"
                                onClick={() => alert('Chức năng xem logs')}
                            />
                            <ActionButton
                                icon={user.isBanned ? "✅" : "🚫"}
                                label={user.isBanned ? "Mở khóa" : "Khóa tài khoản"}
                                onClick={() => setShowBanModal(true)}
                                danger={!user.isBanned}
                            />
                        </div>
                    </div>

                    {/* User Stats */}
                    <div className="bg-[#084289] border border-[#0a4a9e] rounded-xl shadow-lg p-6">
                        <h3 className="text-lg font-semibold text-white mb-4">Thống kê</h3>
                        <div className="space-y-3">
                            <StatItem label="Cây gia phả" value="3" />
                            <StatItem label="Tổng thành viên" value="47" />
                            <StatItem
                                label="Đăng nhập lần cuối"
                                value={user.lastLogin ? "2 giờ trước" : "Chưa bao giờ"}
                            />
                            <StatItem
                                label="Thời gian sử dụng"
                                value={
                                    user.createdAt
                                        ? Math.floor((Date.now() - new Date(user.createdAt).getTime()) / (1000 * 60 * 60 * 24)) + ' ngày'
                                        : 'Không rõ'
                                }
                            />
                        </div>
                    </div>
                </div>
            </div>

            {/* Ban Confirmation Modal */}
            {showBanModal && (
                <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
                    <div className="bg-white rounded-lg p-6 max-w-md w-full mx-4">
                        <h3 className="text-xl font-semibold mb-4">
                            {user.isBanned ? 'Mở khóa tài khoản' : 'Khóa tài khoản'}
                        </h3>
                        <p className="text-gray-600 mb-6">
                            {user.isBanned
                                ? `Bạn có chắc chắn muốn mở khóa tài khoản của ${getDisplayName(user)}? Họ sẽ có thể truy cập lại hệ thống.`
                                : `Bạn có chắc chắn muốn khóa tài khoản của ${getDisplayName(user)}? Họ sẽ không thể truy cập hệ thống.`
                            }
                        </p>
                        <div className="flex justify-end space-x-3">
                            <button
                                onClick={() => setShowBanModal(false)}
                                className="px-4 py-2 border border-gray-300 rounded-lg hover:bg-gray-50 transition-colors"
                                disabled={actionLoading}
                            >
                                Hủy
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
                                {actionLoading ? 'Đang xử lý...' : user.isBanned ? 'Mở khóa' : 'Khóa'}
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
};

// Helper Components
const InfoField: React.FC<{ label: string; value: string }> = ({ label, value }) => (
    <div>
        <label className="text-sm font-medium text-white/60">{label}</label>
        <p className="text-white mt-1">{value}</p>
    </div>
);

const StatusItem: React.FC<{ label: string; value: string; status: 'success' | 'danger' | 'warning' | 'info' }> = ({ label, value, status }) => {
    const statusColors = {
        success: 'bg-green-500/20 text-green-300',
        danger: 'bg-red-500/20 text-red-300',
        warning: 'bg-yellow-500/20 text-yellow-300',
        info: 'bg-blue-500/20 text-blue-300',
    };

    return (
        <div className="flex items-center justify-between p-3 bg-[#0a4a9e]/50 rounded-lg">
            <span className="text-white/80 font-medium">{label}</span>
            <span className={`px-3 py-1 rounded-full text-sm font-medium ${statusColors[status]}`}>
                {value}
            </span>
        </div>
    );
};

const ActivityItem: React.FC<{ action: string; date: string | undefined; icon: string }> = ({ action, date, icon }) => (
    <div className="flex items-start p-3 hover:bg-[#0a4a9e]/50 rounded-lg transition-colors">
        <div className="text-2xl mr-3">{icon}</div>
        <div className="flex-1">
            <p className="font-medium text-white">{action}</p>
            <p className="text-sm text-white/60">
                {date ? new Date(date).toLocaleDateString('vi-VN', {
                    year: 'numeric',
                    month: 'long',
                    day: 'numeric',
                    hour: '2-digit',
                    minute: '2-digit'
                }) : 'Không rõ ngày'}
            </p>
        </div>
    </div>
);

const ActionButton: React.FC<{ icon: string; label: string; onClick: () => void; danger?: boolean }> = ({ icon, label, onClick, danger }) => (
    <button
        onClick={onClick}
        className={`w-full flex items-center p-3 rounded-lg transition-colors ${
            danger
                ? 'hover:bg-red-500/20 text-red-300'
                : 'hover:bg-[#0a4a9e]/50 text-white'
        }`}
    >
        <span className="text-xl mr-3">{icon}</span>
        <span className="font-medium">{label}</span>
    </button>
);

const StatItem: React.FC<{ label: string; value: string }> = ({ label, value }) => (
    <div className="flex justify-between items-center">
        <span className="text-white/60">{label}</span>
        <span className="font-semibold text-white">{value}</span>
    </div>
);

export default UserDetail;