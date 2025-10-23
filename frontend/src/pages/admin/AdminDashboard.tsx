// src/pages/admin/AdminDashboard.tsx
import React from 'react';
import { Link } from 'react-router-dom';
import { User } from '../../types/ts_user';
import { useUsers } from '../../hooks/useUsers';
import { useFamilyTrees } from '../../hooks/useFamilyTrees';

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

        const adminUsers = users.filter((u: User) =>
            u.roleName === 'admin' || u.role === 'ADMIN'
        ).length;

        const moderatorUsers = users.filter((u: User) =>
            u.roleName === 'moderator' || u.role === 'MODERATOR'
        ).length;

        // ✅ Calculate REAL total members from family trees
        const totalMembers = familyTrees.reduce((sum, tree) => {
            return sum + (tree.memberCount || 0);
        }, 0);

        // Calculate percentages
        const userGrowth = totalUsers > 0 ? Math.floor((newUsersThisMonth / totalUsers) * 100) : 0;
        const treeGrowth = Math.floor(Math.random() * 15) + 5; // Mock data
        const memberGrowth = Math.floor(Math.random() * 30) + 10; // Mock data
        const activityRate = totalUsers > 0 ? Math.floor((activeUsers / totalUsers) * 100) : 0;

        return {
            totalUsers,
            activeUsers,
            bannedUsers,
            newUsersThisMonth,
            adminUsers,
            moderatorUsers,
            totalFamilyTrees: familyTrees.length,
            totalMembers, // ✅ Now using REAL data
            userGrowth,
            treeGrowth,
            memberGrowth,
            activityRate,
        };
    }, [users, familyTrees]);

    if (usersLoading || treesLoading) {
        return (
            <div className="flex flex-col justify-center items-center h-96">
                <div className="w-16 h-16 border-4 border-[#D1B066] border-t-transparent rounded-full animate-spin"></div>
                <p className="text-white/80 mt-4">Đang tải dữ liệu...</p>
            </div>
        );
    }

    return (
        <div>
            {/* Header */}
            <div className="mb-8">
                <h1 className="text-4xl font-bold text-white mb-2">Tổng Quan Hệ Thống</h1>
                <p className="text-white/60">Quản lý gia phả và người dùng</p>
            </div>

            {/* Error Messages */}
            {usersError && (
                <div className="bg-red-500/20 border border-red-400/50 text-red-200 px-6 py-4 rounded-xl mb-6">
                    <strong>⚠️ Lỗi:</strong> {usersError}
                </div>
            )}

            {treesError && (
                <div className="bg-red-500/20 border border-red-400/50 text-red-200 px-6 py-4 rounded-xl mb-6">
                    <strong>⚠️ Lỗi:</strong> {treesError}
                </div>
            )}

            {/* Stats Grid */}
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
                <StatCard
                    title="Tổng Người Dùng"
                    value={stats?.totalUsers.toLocaleString() || '0'}
                    icon="👥"
                    change={`+${stats?.userGrowth || 0}% từ tháng trước`}
                    bgColor="bg-[#2563eb]"
                />
                <StatCard
                    title="Gia Phả Hoạt Động"
                    value={stats?.totalFamilyTrees.toLocaleString() || '0'}
                    icon="🌲"
                    change={`+${stats?.treeGrowth || 0}% từ tháng trước`}
                    bgColor="bg-[#2563eb]"
                />
                <StatCard
                    title="Thành Viên Gia Phả"
                    value={stats?.totalMembers.toLocaleString() || '0'}  // Dynamic từ data thật
                    icon="📊"
                    change={`+${stats?.memberGrowth || 0}% từ tháng trước`}  // Dynamic
                    bgColor="bg-[#2563eb]"
                />
                <StatCard
                    title="Tỷ Lệ Hoạt Động"
                    value={`${stats?.activityRate || 0}%`}
                    icon="📈"
                    change={`+${Math.floor(Math.random() * 10)}% từ tháng trước`}
                    bgColor="bg-[#2563eb]"
                />
            </div>

            {/* Two Column Layout */}
            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-8">
                {/* User Activity Chart */}
                <div className="bg-[#084289] rounded-2xl border border-[#0a4a9e] p-6">
                    <h3 className="text-xl font-bold text-white mb-6">Hoạt Động Người Dùng</h3>
                    <div className="space-y-4">
                        <ActivityBar label="Đăng nhập hôm nay" value={85} color="bg-[#D1B066]" />
                        <ActivityBar label="Người dùng mới" value={45} color="bg-green-500" />
                        <ActivityBar label="Đã tạo gia phả" value={62} color="bg-blue-500" />
                        <ActivityBar label="Đã cập nhật hồ sơ" value={38} color="bg-purple-500" />
                    </div>
                </div>

                {/* User Roles Distribution */}
                <div className="bg-[#084289] rounded-2xl border border-[#0a4a9e] p-6">
                    <h3 className="text-xl font-bold text-white mb-6">Phân Loại Vai Trò</h3>
                    <div className="space-y-4">
                        <RoleBar
                            label="Người dùng thông thường"
                            count={stats?.totalUsers ? stats.totalUsers - stats.adminUsers - stats.moderatorUsers : 0}
                            total={stats?.totalUsers || 1}
                            color="bg-blue-500"
                        />
                        <RoleBar
                            label="Kiểm duyệt viên"
                            count={stats?.moderatorUsers || 0}
                            total={stats?.totalUsers || 1}
                            color="bg-purple-500"
                        />
                        <RoleBar
                            label="Quản trị viên"
                            count={stats?.adminUsers || 0}
                            total={stats?.totalUsers || 1}
                            color="bg-[#D1B066]"
                        />
                    </div>
                </div>
            </div>

            {/* Recent Users and Trees */}
            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                {/* Recent Users */}
                <div className="bg-[#084289] rounded-2xl border border-[#0a4a9e] p-6">
                    <div className="flex justify-between items-center mb-6">
                        <h3 className="text-xl font-bold text-white">Người Dùng Gần Đây</h3>
                        <Link to="/admin/users" className="text-[#D1B066] hover:text-white text-sm">
                            Xem tất cả →
                        </Link>
                    </div>
                    <div className="space-y-3">
                        {users.slice(0, 5).map((user: User) => (
                            <div
                                key={user.id}
                                className="flex items-center justify-between p-3 bg-[#0a4a9e]/50 rounded-lg hover:bg-[#0a4a9e] transition-colors"
                            >
                                <div className="flex items-center gap-3">
                                    <div className="w-10 h-10 bg-[#D1B066] rounded-lg flex items-center justify-center text-white font-bold">
                                        {user.email?.charAt(0).toUpperCase()}
                                    </div>
                                    <div>
                                        <p className="font-medium text-white text-sm">
                                            {user.username || user.email}
                                        </p>
                                        <p className="text-xs text-white/60">{user.email}</p>
                                    </div>
                                </div>
                                <span className={`px-3 py-1 rounded-full text-xs font-medium ${
                                    user.isBanned
                                        ? 'bg-red-500/20 text-red-300'
                                        : 'bg-green-500/20 text-green-300'
                                }`}>
                                    {user.isBanned ? 'Khóa' : 'Hoạt động'}
                                </span>
                            </div>
                        ))}
                    </div>
                </div>

                {/* Recent Family Trees */}
                <div className="bg-[#084289] rounded-2xl border border-[#0a4a9e] p-6">
                    <div className="flex justify-between items-center mb-6">
                        <h3 className="text-xl font-bold text-white">Cây Gia Phả Mới</h3>
                        <Link to="/admin/trees" className="text-[#D1B066] hover:text-white text-sm">
                            Xem tất cả →
                        </Link>
                    </div>
                    <div className="space-y-3">
                        {familyTrees.slice(0, 5).map((tree) => (
                            <div
                                key={tree.id}
                                className="flex items-center justify-between p-3 bg-[#0a4a9e]/50 rounded-lg hover:bg-[#0a4a9e] transition-colors"
                            >
                                <div className="flex items-center gap-3">
                                    <div className="w-10 h-10 bg-green-500 rounded-lg flex items-center justify-center">
                                        🌳
                                    </div>
                                    <div>
                                        <p className="font-medium text-white text-sm">{tree.name}</p>
                                        <p className="text-xs text-white/60 truncate max-w-xs">
                                            {tree.description || 'Không có mô tả'}
                                        </p>
                                    </div>
                                </div>
                                <span className="text-xs text-white/60">
                                    {new Date(tree.createdAt).toLocaleDateString('vi-VN')}
                                </span>
                            </div>
                        ))}
                        {familyTrees.length === 0 && (
                            <div className="text-center text-white/60 py-8">
                                <p>Chưa có cây gia phả</p>
                            </div>
                        )}
                    </div>
                </div>
            </div>
        </div>
    );
};

// StatCard Component
interface StatCardProps {
    title: string;
    value: string;
    icon: string;
    change: string;
    bgColor: string;
}

const StatCard: React.FC<StatCardProps> = ({ title, value, icon, change, bgColor }) => {
    return (
        <div className={`${bgColor} rounded-2xl p-6 shadow-lg`}>
            <div className="flex items-start justify-between mb-4">
                <div className="w-14 h-14 bg-[#D1B066] rounded-xl flex items-center justify-center text-2xl">
                    {icon}
                </div>
            </div>
            <div>
                <p className="text-white/80 text-sm mb-1">{title}</p>
                <p className="text-4xl font-bold text-white mb-2">{value}</p>
                <p className="text-white/60 text-xs">{change}</p>
            </div>
        </div>
    );
};

// ActivityBar Component
interface ActivityBarProps {
    label: string;
    value: number;
    color: string;
}

const ActivityBar: React.FC<ActivityBarProps> = ({ label, value, color }) => {
    return (
        <div>
            <div className="flex justify-between text-sm mb-2">
                <span className="text-white/80">{label}</span>
                <span className="text-white font-medium">{value}%</span>
            </div>
            <div className="w-full bg-[#0a4a9e] rounded-full h-2">
                <div
                    className={`${color} h-2 rounded-full transition-all duration-500`}
                    style={{ width: `${value}%` }}
                />
            </div>
        </div>
    );
};

// RoleBar Component
interface RoleBarProps {
    label: string;
    count: number;
    total: number;
    color: string;
}

const RoleBar: React.FC<RoleBarProps> = ({ label, count, total, color }) => {
    const percentage = total > 0 ? (count / total) * 100 : 0;

    return (
        <div>
            <div className="flex justify-between text-sm mb-2">
                <span className="text-white/80">{label}</span>
                <span className="text-white font-medium">{count} ({percentage.toFixed(1)}%)</span>
            </div>
            <div className="w-full bg-[#0a4a9e] rounded-full h-2">
                <div
                    className={`${color} h-2 rounded-full transition-all duration-500`}
                    style={{ width: `${percentage}%` }}
                />
            </div>
        </div>
    );
};

export default AdminDashboard;