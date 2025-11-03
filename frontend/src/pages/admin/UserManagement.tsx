// src/pages/admin/UserManagement.tsx
import React from 'react';
import { useNavigate } from 'react-router-dom';
import UserList from '../../components/admin/UserList';
import { User } from '../../types/ts_user';
import { useUsers } from '../../hooks/useUsers';

const UserManagement: React.FC = () => {
    const navigate = useNavigate();
    const { users, loading, error, banUser, unbanUser, refreshUsers } = useUsers();

    const handleViewDetail = (userId: string) => {
        navigate(`/admin/users/${userId}`);
    };

    if (loading) {
        return (
            <div className="flex flex-col justify-center items-center h-96">
                <div className="w-16 h-16 border-4 border-[#d1b98a] border-t-transparent rounded-full animate-spin"></div>
                <p className="text-[#f4e9c8]/80 mt-4">Đang tải danh sách người dùng...</p>
            </div>
        );
    }

    if (error) {
        return (
            <div className="bg-[#2e3a57]/80 border border-red-500/40 text-red-300 rounded-2xl p-8 shadow-lg shadow-black/30">
                <div className="flex items-start">
                    <span className="text-4xl mr-4">⚠️</span>
                    <div className="flex-1">
                        <h3 className="text-xl font-bold mb-2 text-[#f4e9c8]">Lỗi Tải Dữ Liệu</h3>
                        <p className="text-red-300 mb-4">{error}</p>
                        <button
                            onClick={refreshUsers}
                            className="px-6 py-3 bg-[#d1b98a] text-[#20283d] rounded-lg font-semibold hover:bg-[#f4e9c8] transition-all"
                        >
                            Thử Lại
                        </button>
                    </div>
                </div>
            </div>
        );
    }

    return (
        <div className="space-y-8">
            {/* Header */}
            <div className="flex justify-between items-center">
                <div>
                    <h2 className="text-4xl font-bold bg-gradient-to-r from-[#d1b98a] to-[#f4e9c8] bg-clip-text text-transparent mb-2">
                        Quản Lý Người Dùng
                    </h2>
                    <p className="text-[#f4e9c8]/70 text-lg">
                        Quản lý và giám sát tất cả người dùng trong hệ thống
                    </p>
                </div>

                <button
                    onClick={refreshUsers}
                    className="px-6 py-3 rounded-lg flex items-center font-semibold bg-[#d1b98a] text-[#20283d] hover:bg-[#f4e9c8] transition-all shadow-md shadow-black/30"
                >
                    <svg
                        className="w-5 h-5 mr-2"
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
                    Làm Mới
                </button>
            </div>

            {/* Stats Summary */}
            <div className="grid grid-cols-1 md:grid-cols-4 gap-6">
                <StatCard label="Tổng Người Dùng" value={users.length} icon="👥" color="gold" />
                <StatCard
                    label="Đang Hoạt Động"
                    value={users.filter((u: User) => !u.isBanned).length}
                    icon="✅"
                    color="green"
                />
                <StatCard
                    label="Đã Bị Khóa"
                    value={users.filter((u: User) => u.isBanned).length}
                    icon="🚫"
                    color="red"
                />
                <StatCard
                    label="Quản Trị Viên"
                    value={users.filter((u: User) => u.role === 'ADMIN').length}
                    icon="👑"
                    color="blue"
                />
            </div>

            {/* User List */}
            <div className="bg-[#1b2233]/90 border border-[#d1b98a]/20 rounded-2xl p-6 shadow-lg shadow-black/30">
                <UserList users={users} onBan={banUser} onUnban={unbanUser} onViewDetail={handleViewDetail} />
            </div>
        </div>
    );
};

// 🟡 StatCard (chỉ đổi style, không đổi logic)
interface StatCardProps {
    label: string;
    value: number;
    icon: string;
    color: 'blue' | 'green' | 'red' | 'gold';
}

const StatCard: React.FC<StatCardProps> = ({ label, value, icon, color }) => {
    const colorMap = {
        blue: 'from-[#3b82f6] to-[#2563eb]',
        green: 'from-green-400 to-green-600',
        red: 'from-red-400 to-red-600',
        gold: 'from-[#d1b98a] to-[#f4e9c8]',
    };

    return (
        <div
            className={`bg-gradient-to-br ${colorMap[color]} p-[1px] rounded-2xl shadow-lg shadow-black/30`}
        >
            <div className="bg-[#1b2233]/95 rounded-2xl p-6 flex justify-between items-center transition-all hover:shadow-[#d1b98a]/30 hover:-translate-y-1">
                <div>
                    <p className="text-gray-300 text-sm mb-1">{label}</p>
                    <p className="text-4xl font-bold text-[#f4e9c8]">{value}</p>
                </div>
                <div className="text-4xl opacity-90">{icon}</div>
            </div>
        </div>
    );
};

export default UserManagement;
