// src/components/admin/UserList.tsx
import React, { useState } from 'react';
import { User } from '../../types/ts_user';
import UserCard from './UserCard';

interface UserListProps {
    users: User[];
    onBan: (userId: string) => Promise<void>;
    onUnban: (userId: string) => Promise<void>;
    onViewDetail?: (userId: string) => void;
}

const UserList: React.FC<UserListProps> = ({ users, onBan, onUnban, onViewDetail }) => {
    const [searchQuery, setSearchQuery] = useState('');
    const [roleFilter, setRoleFilter] = useState<string>('all');
    const [statusFilter, setStatusFilter] = useState<string>('all');

    const filteredUsers = users.filter(user => {
        const matchesSearch =
            user.email.toLowerCase().includes(searchQuery.toLowerCase()) ||
            `${user.firstName} ${user.lastName}`.toLowerCase().includes(searchQuery.toLowerCase());

        const matchesRole = roleFilter === 'all' || user.role === roleFilter;
        const matchesStatus =
            statusFilter === 'all' ||
            (statusFilter === 'banned' && user.isBanned) ||
            (statusFilter === 'active' && !user.isBanned);

        return matchesSearch && matchesRole && matchesStatus;
    });

    return (
        <div>
            {/* Filters */}
            <div className="mb-6 bg-[#084289] p-6 rounded-xl border border-[#0a4a9e] shadow-lg">
                <div className="flex items-center mb-4">
                    <div className="w-10 h-10 bg-[#D1B066] rounded-lg flex items-center justify-center mr-3">
                        <span className="text-xl">🔍</span>
                    </div>
                    <h3 className="text-lg font-bold text-white">Bộ Lọc & Tìm Kiếm</h3>
                </div>

                <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                    {/* Search */}
                    <div>
                        <label className="block text-sm font-medium text-white/80 mb-2">
                            Tìm kiếm
                        </label>
                        <input
                            type="text"
                            placeholder="Nhập tên hoặc email..."
                            value={searchQuery}
                            onChange={(e) => setSearchQuery(e.target.value)}
                            className="w-full px-4 py-2 bg-[#0a4a9e]/50 border border-[#D1B066]/30 rounded-lg text-white placeholder-white/40 focus:ring-2 focus:ring-[#D1B066] focus:border-[#D1B066] transition-all"
                        />
                    </div>

                    {/* Role Filter */}
                    <div>
                        <label className="block text-sm font-medium text-white/80 mb-2">
                            Vai trò
                        </label>
                        <select
                            value={roleFilter}
                            onChange={(e) => setRoleFilter(e.target.value)}
                            className="w-full px-4 py-2 bg-[#0a4a9e]/50 border border-[#D1B066]/30 rounded-lg text-white focus:ring-2 focus:ring-[#D1B066] focus:border-[#D1B066] transition-all cursor-pointer"
                        >
                            <option value="all" className="bg-[#084289]">Tất cả vai trò</option>
                            <option value="USER" className="bg-[#084289]">Người dùng</option>
                            <option value="ADMIN" className="bg-[#084289]">Quản trị viên</option>
                            <option value="MODERATOR" className="bg-[#084289]">Kiểm duyệt viên</option>
                        </select>
                    </div>

                    {/* Status Filter */}
                    <div>
                        <label className="block text-sm font-medium text-white/80 mb-2">
                            Trạng thái
                        </label>
                        <select
                            value={statusFilter}
                            onChange={(e) => setStatusFilter(e.target.value)}
                            className="w-full px-4 py-2 bg-[#0a4a9e]/50 border border-[#D1B066]/30 rounded-lg text-white focus:ring-2 focus:ring-[#D1B066] focus:border-[#D1B066] transition-all cursor-pointer"
                        >
                            <option value="all" className="bg-[#084289]">Tất cả trạng thái</option>
                            <option value="active" className="bg-[#084289]">Đang hoạt động</option>
                            <option value="banned" className="bg-[#084289]">Đã bị khóa</option>
                        </select>
                    </div>
                </div>

                {/* Results Count */}
                <div className="mt-4 pt-4 border-t border-[#0a4a9e]">
                    <div className="flex items-center text-sm text-white/70">
                        <span className="text-xl mr-2">📊</span>
                        <span>
                            Hiển thị <span className="text-[#D1B066] font-bold">{filteredUsers.length}</span> / {users.length} người dùng
                        </span>
                    </div>
                </div>
            </div>

            {/* User List */}
            <div className="grid gap-4">
                {filteredUsers.length > 0 ? (
                    filteredUsers.map(user => (
                        <UserCard
                            key={user.id}
                            user={user}
                            onBan={onBan}
                            onUnban={onUnban}
                            onViewDetail={onViewDetail}
                        />
                    ))
                ) : (
                    <div className="bg-[#084289] p-16 rounded-xl border-2 border-dashed border-[#0a4a9e] text-center">
                        <div className="text-6xl mb-4">🔍</div>
                        <p className="text-white/90 text-xl font-semibold mb-2">Không tìm thấy người dùng</p>
                        <p className="text-white/60 text-sm">
                            Hãy thử điều chỉnh bộ lọc của bạn
                        </p>
                    </div>
                )}
            </div>
        </div>
    );
};

export default UserList;