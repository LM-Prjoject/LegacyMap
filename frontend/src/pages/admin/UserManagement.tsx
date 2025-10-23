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
                <div className="w-16 h-16 border-4 border-[#D1B066] border-t-transparent rounded-full animate-spin"></div>
                <p className="text-white/80 mt-4">Đang tải danh sách người dùng...</p>
            </div>
        );
    }

    if (error) {
        return (
            <div className="bg-red-500/20 border border-red-400/50 rounded-xl p-8">
                <div className="flex items-start">
                    <span className="text-4xl mr-4">⚠️</span>
                    <div className="flex-1">
                        <h3 className="text-xl font-bold text-red-200 mb-2">Lỗi Tải Dữ Liệu</h3>
                        <p className="text-red-300 mb-4">{error}</p>
                        <button
                            onClick={refreshUsers}
                            className="px-6 py-3 bg-red-500 text-white rounded-lg hover:bg-red-600 transition-all font-semibold"
                        >
                            Thử Lại
                        </button>
                    </div>
                </div>
            </div>
        );
    }

    return (
        <div>
            {/* Header */}
            <div className="flex justify-between items-center mb-8">
                <div>
                    <h2 className="text-4xl font-bold text-white mb-2">
                        Quản Lý Người Dùng
                    </h2>
                    <p className="text-white/60 text-lg">
                        Quản lý và giám sát tất cả người dùng trong hệ thống
                    </p>
                </div>
                <button
                    onClick={refreshUsers}
                    className="px-6 py-3 bg-[#D1B066] text-[#084289] rounded-lg hover:bg-[#f4d88a] transition-all flex items-center font-bold"
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
            <div className="grid grid-cols-1 md:grid-cols-4 gap-6 mb-8">
                <StatCard
                    label="Tổng Người Dùng"
                    value={users.length}
                    icon="👥"
                    color="blue"
                />
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
                    color="gold"
                />
            </div>

            {/* User List */}
            <UserList
                users={users}
                onBan={banUser}
                onUnban={unbanUser}
                onViewDetail={handleViewDetail}
            />
        </div>
    );
};

interface StatCardProps {
    label: string;
    value: number;
    icon: string;
    color: 'blue' | 'green' | 'red' | 'gold';
}

const StatCard: React.FC<StatCardProps> = ({ label, value, icon, color }) => {
    const colorClasses = {
        blue: 'bg-[#2563eb]',
        green: 'bg-green-500',
        red: 'bg-red-500',
        gold: 'bg-[#D1B066]',
    };

    return (
        <div className={`${colorClasses[color]} rounded-xl p-6 shadow-lg`}>
            <div className="flex items-center justify-between">
                <div>
                    <p className="text-white/80 text-sm font-medium mb-1">{label}</p>
                    <p className="text-4xl font-bold text-white">{value}</p>
                </div>
                <div className="text-5xl opacity-80">{icon}</div>
            </div>
        </div>
    );
};

export default UserManagement;