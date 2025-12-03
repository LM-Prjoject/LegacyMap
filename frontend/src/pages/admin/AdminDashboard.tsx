import React, {ReactNode, useEffect, useState} from 'react';
import { Link } from 'react-router-dom';
import { User } from"@/types/ts_user.ts";
import { useUsers } from "@/hooks/useUsers.ts";
import { useFamilyTrees } from "@/hooks/useFamilyTrees.ts";
import { adminApi, AdminStats } from "@/api/ts_admin.ts";
import {Circle, UsersRound, Trees, TrendingUp, TreePine, ArrowRight} from "lucide-react"

const AdminDashboard: React.FC = () => {
    const { users, loading: usersLoading, error: usersError } = useUsers();
    const { familyTrees, loading: treesLoading, error: treesError } = useFamilyTrees();
    const [stats, setStats] = useState<AdminStats | null>(null);
    const [statsLoading, setStatsLoading] = useState(true);

    useEffect(() => {
        const loadStats = async () => {
            try {
                setStatsLoading(true);
                const data = await adminApi.getAdminStats();

                if (!data.activityStats) {
                    data.activityStats = {
                        loginsToday: 0,
                        loginsThisWeek: 0,
                        loginsThisMonth: 0,
                        newUsersThisMonth: 0,
                        loginsTodayPercent: 0,
                        newUsersPercent: 0
                    };
                }

                setStats(data);
            } catch (error) {

                setStats({
                    totalUsers: 0,
                    activeUsers: 0,
                    bannedUsers: 0,
                    onlineUsers: 0,
                    adminUsers: 0,
                    moderatorUsers: 0,
                    regularUsers: 0,
                    newUsersThisMonth: 0,
                    totalFamilyTrees: 0,
                    totalMembers: 0,
                    activityStats: {
                        loginsToday: 0,
                        loginsThisWeek: 0,
                        loginsThisMonth: 0,
                        newUsersThisMonth: 0,
                        loginsTodayPercent: 0,
                        newUsersPercent: 0
                    }
                });
            } finally {
                setStatsLoading(false);
            }
        };

        loadStats();
    }, []);

    if (usersLoading || treesLoading || statsLoading) {
        return (
            <div className="flex flex-col justify-center items-center h-96">
                <div className="w-16 h-16 border-4 border-[#d1b98a] border-t-transparent rounded-full animate-spin"></div>
                <p className="text-[#f4e9c8]/90 mt-4">Đang tải dữ liệu...</p>
            </div>
        );
    }

    return (
        <div>
            <div className="mb-8">
                <h1 className="text-4xl font-bold bg-gradient-to-r from-[#d1b98a] to-[#f4e9c8] bg-clip-text text-transparent mb-2">
                    Tổng Quan Hệ Thống
                </h1>
                <p className="text-[#f4e9c8]/70">Quản lý gia phả và người dùng</p>
            </div>

            {usersError && (
                <div className="bg-red-500/10 border border-red-500/40 text-red-300 px-6 py-4 rounded-xl mb-6">
                    <strong>Lỗi:</strong> {usersError}
                </div>
            )}
            {treesError && (
                <div className="bg-red-500/10 border border-red-500/40 text-red-300 px-6 py-4 rounded-xl mb-6">
                    <strong>Lỗi:</strong> {treesError}
                </div>
            )}

            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
                <StatCard
                    title="Tổng Người Dùng"
                    value={(stats?.totalUsers || 0).toLocaleString()}
                    change={`${stats?.newUsersThisMonth || 0} mới tháng này`}
                >
                    <UsersRound size={28} className="text-[#20283d]" />
                </StatCard>
                <StatCard
                    title="Đang Online"
                    value={(stats?.onlineUsers || 0).toLocaleString()}
                    change={`${stats?.activeUsers || 0} đang hoạt động`}
                >
                    <Circle size={28} className="text-green-600" />
                </StatCard>
                <StatCard
                    title="Gia Phả Hoạt Động"
                    value={(stats?.totalFamilyTrees || 0).toLocaleString()}
                    change={`${stats?.totalMembers || 0} thành viên`}
                >
                    <Trees size={28} className="text-green-600" />
                </StatCard>
                <StatCard
                    title="Tỷ Lệ Hoạt Động"
                    value={`${stats?.totalUsers && stats?.activeUsers ? Math.round((stats.activeUsers / stats.totalUsers) * 100) : 0}%`}
                    change={`${stats?.bannedUsers || 0} bị khóa`}
                >
                    <TrendingUp size={28} className="text-red-600" />
                </StatCard>
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-8">
                <div className="bg-[#1b2233]/90 border border-[#d1b98a]/20 rounded-2xl p-6 shadow-lg shadow-black/30">
                    <h3 className="text-xl font-bold text-[#f4e9c8] mb-6">Hoạt Động Người Dùng</h3>
                    <div className="space-y-4">
                        <ActivityBar
                            label="Active hôm nay (24h gần nhất)"
                            value={stats?.activityStats?.loginsTodayPercent || 0}
                            count={stats?.activityStats?.loginsToday || 0}
                            total={stats?.totalUsers || 0}
                            color="bg-[#d1b98a]"
                        />
                        <ActivityBar
                            label="Người dùng mới tháng này"
                            value={stats?.activityStats?.newUsersPercent || 0}
                            count={stats?.activityStats?.newUsersThisMonth || 0}
                            total={stats?.totalUsers || 0}
                            color="bg-green-500"
                        />
                        <ActivityBar
                            label="Active tuần này (7 ngày)"
                            value={stats?.totalUsers ? Math.round((stats.activityStats.loginsThisWeek / stats.totalUsers) * 100) : 0}
                            count={stats?.activityStats?.loginsThisWeek || 0}
                            total={stats?.totalUsers || 0}
                            color="bg-blue-500"
                        />
                        <ActivityBar
                            label="Active tháng này (30 ngày)"
                            value={stats?.totalUsers ? Math.round((stats.activityStats.loginsThisMonth / stats.totalUsers) * 100) : 0}
                            count={stats?.activityStats?.loginsThisMonth || 0}
                            total={stats?.totalUsers || 0}
                            color="bg-purple-500"
                        />
                    </div>
                </div>

                {/* Role Distribution */}
                <div className="bg-[#1b2233]/90 border border-[#d1b98a]/20 rounded-2xl p-6 shadow-lg shadow-black/30">
                    <h3 className="text-xl font-bold text-[#f4e9c8] mb-6">Phân Loại Vai Trò</h3>
                    {stats ? (
                        <div className="space-y-4">
                            <RoleBar
                                label="Người dùng thông thường"
                                count={stats.regularUsers || 0}
                                total={stats.totalUsers || 1}
                                color="bg-blue-500"
                            />
                            <RoleBar
                                label="Kiểm duyệt viên"
                                count={stats.moderatorUsers || 0}
                                total={stats.totalUsers || 1}
                                color="bg-purple-500"
                            />
                            <RoleBar
                                label="Quản trị viên"
                                count={stats.adminUsers || 0}
                                total={stats.totalUsers || 1}
                                color="bg-[#d1b98a]"
                            />
                        </div>
                    ) : (
                        <div className="text-center py-8 text-gray-400">
                            <p>Đang tải dữ liệu...</p>
                        </div>
                    )}
                </div>
            </div>

            {/* Recent Users / Trees */}
            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                {/* Users */}
                <div className="bg-[#1b2233]/90 border border-[#d1b98a]/20 rounded-2xl p-6 shadow-lg shadow-black/30">
                    <div className="flex justify-between items-center mb-6">
                        <h3 className="text-xl font-bold text-[#f4e9c8]">Người Dùng Gần Đây</h3>
                        <Link to="/admin/users" className="text-[#d1b98a] hover:text-[#f4e9c8] text-sm transition-colors flex">
                            Xem tất cả <ArrowRight size="18"/>
                        </Link>
                    </div>
                    <div className="space-y-3">
                        {users.slice(0, 5).map((user: User) => (
                            <div
                                key={user.id}
                                className="flex items-center justify-between p-3 bg-[#2e3a57]/40 rounded-lg hover:bg-[#2e3a57]/80 transition-colors"
                            >
                                <div className="flex items-center gap-3">
                                    <div className="relative">
                                        <div className="w-10 h-10 bg-[#d1b98a] rounded-lg flex items-center justify-center text-[#20283d] font-bold">
                                            {user.email?.charAt(0).toUpperCase()}
                                        </div>
                                        {!user.isBanned && user.lastLogin && (
                                            <div
                                                className={`absolute -bottom-0.5 -right-0.5 w-3 h-3 rounded-full border-2 border-[#1b2233] ${
                                                    new Date().getTime() - new Date(user.lastLogin).getTime() < 5 * 60 * 1000
                                                        ? 'bg-green-400'
                                                        : 'bg-gray-500'
                                                }`}
                                            />
                                        )}
                                    </div>
                                    <div>
                                        <p className="font-medium text-[#f4e9c8] text-sm">
                                            {user.username || user.email}
                                        </p>
                                        <p className="text-xs text-gray-400">{user.email}</p>
                                    </div>
                                </div>
                                <span
                                    className={`px-3 py-1 rounded-full text-xs font-medium ${
                                        user.isBanned
                                            ? 'bg-red-500/10 text-red-400 border border-red-400/40'
                                            : 'bg-green-500/10 text-green-400 border border-green-400/30'
                                    }`}
                                >
                  {user.isBanned ? 'Khóa' : 'Hoạt động'}
                </span>
                            </div>
                        ))}
                    </div>
                </div>

                <div className="bg-[#1b2233]/90 border border-[#d1b98a]/20 rounded-2xl p-6 shadow-lg shadow-black/30">
                    <div className="flex justify-between items-center mb-6">
                        <h3 className="text-xl font-bold text-[#f4e9c8]">Cây Gia Phả Mới</h3>
                        <Link to="/admin/trees" className="text-[#d1b98a] hover:text-[#f4e9c8] text-sm transition-colors flex">
                            Xem tất cả <ArrowRight size="18"/>
                        </Link>
                    </div>
                    <div className="space-y-3">
                        {familyTrees.slice(0, 5).map((tree) => (
                            <div
                                key={tree.id}
                                className="flex items-center justify-between p-3 bg-[#2e3a57]/40 rounded-lg hover:bg-[#2e3a57]/80 transition-colors"
                            >
                                <div className="flex items-center gap-3">
                                    <div className="w-10 h-10 bg-[#d1b98a]/90 rounded-lg flex items-center justify-center">
                                        <TreePine/>
                                    </div>
                                    <div>
                                        <p className="font-medium text-[#f4e9c8] text-sm">{tree.name}</p>
                                        <p className="text-xs text-gray-400 truncate max-w-xs">
                                            {tree.description || 'Không có mô tả'}
                                        </p>
                                    </div>
                                </div>
                                <span className="text-xs text-gray-400">
                  {new Date(tree.createdAt).toLocaleDateString('vi-VN')}
                </span>
                            </div>
                        ))}
                        {familyTrees.length === 0 && (
                            <div className="text-center text-gray-400 py-8">
                                <p>Chưa có cây gia phả</p>
                            </div>
                        )}
                    </div>
                </div>
            </div>
        </div>
    );
};

interface StatCardProps {
    title: string;
    value: string;
    children: ReactNode;
    change: string;
}
const StatCard: React.FC<StatCardProps> = ({ title, value, children, change }) => (
    <div className="bg-[#1b2233]/90 border border-[#d1b98a]/20 rounded-2xl p-6 shadow-lg shadow-black/30 hover:shadow-[#d1b98a]/20 transition-all">
        <div className="flex items-start justify-between mb-4">
            <div className="w-14 h-14 bg-gradient-to-br from-[#d1b98a] to-[#f4e9c8] rounded-xl flex items-center justify-center text-2xl text-[#20283d] font-semibold">
                {children}
            </div>
        </div>
        <div>
            <p className="text-gray-300 text-sm mb-1">{title}</p>
            <p className="text-4xl font-bold text-[#f4e9c8] mb-2">{value}</p>
            <p className="text-sm text-gray-400">{change}</p>
        </div>
    </div>
);

interface ActivityBarProps {
    label: string;
    value: number;
    count: number;
    total: number;
    color: string;
}
const ActivityBar: React.FC<ActivityBarProps> = ({ label, value, count, total, color }) => (
    <div>
        <div className="flex justify-between text-sm mb-2">
            <span className="text-gray-300">{label}</span>
            <span className="text-[#f4e9c8] font-medium">
                {count}/{total} ({value}%)
            </span>
        </div>
        <div className="w-full bg-[#2e3a57]/60 rounded-full h-2">
            <div
                className={`${color} h-2 rounded-full transition-all duration-500`}
                style={{ width: `${Math.min(value, 100)}%` }}
            />
        </div>
    </div>
);

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
                <span className="text-gray-300">{label}</span>
                <span className="text-[#f4e9c8] font-medium">
          {count} ({percentage.toFixed(1)}%)
        </span>
            </div>
            <div className="w-full bg-[#2e3a57]/60 rounded-full h-2">
                <div
                    className={`${color} h-2 rounded-full transition-all duration-500`}
                    style={{ width: `${percentage}%` }}
                />
            </div>
        </div>
    );
};

export default AdminDashboard;