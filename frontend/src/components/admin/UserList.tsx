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
    const [currentPage, setCurrentPage] = useState(1);

    const usersPerPage = 5;

    // ✅ Lọc người dùng
    const filteredUsers = users.filter((user) => {
        const matchesSearch =
            user.email.toLowerCase().includes(searchQuery.toLowerCase()) ||
            `${user.firstName} ${user.lastName}`.toLowerCase().includes(searchQuery.toLowerCase());

        const matchesRole = roleFilter === 'all' || user.role === roleFilter || user.roleName === roleFilter;
        const matchesStatus =
            statusFilter === 'all' ||
            (statusFilter === 'banned' && user.isBanned) ||
            (statusFilter === 'active' && !user.isBanned);

        return matchesSearch && matchesRole && matchesStatus;
    });

    // ✅ Phân trang
    const totalPages = Math.ceil(filteredUsers.length / usersPerPage);
    const indexOfLast = currentPage * usersPerPage;
    const indexOfFirst = indexOfLast - usersPerPage;
    const currentUsers = filteredUsers.slice(indexOfFirst, indexOfLast);

    const handlePageChange = (page: number) => {
        if (page >= 1 && page <= totalPages) {
            setCurrentPage(page);
            window.scrollTo({ top: 0, behavior: 'smooth' });
        }
    };

    return (
        <div className="space-y-6">
            {/* Bộ lọc */}
            <div className="bg-[#1b2233]/95 p-6 rounded-2xl border border-[#d1b98a]/20 shadow-lg shadow-black/30">
                <div className="flex items-center mb-4">
                    <div className="w-10 h-10 bg-gradient-to-br from-[#d1b98a] to-[#f4e9c8] rounded-lg flex items-center justify-center mr-3 text-[#20283d] font-bold">
                        🔍
                    </div>
                    <h3 className="text-lg font-bold text-[#f4e9c8]">Bộ Lọc & Tìm Kiếm</h3>
                </div>

                <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                    {/* Tìm kiếm */}
                    <div>
                        <label className="block text-sm font-medium text-[#f4e9c8]/80 mb-2">Tìm kiếm</label>
                        <input
                            type="text"
                            placeholder="Nhập tên hoặc email..."
                            value={searchQuery}
                            onChange={(e) => {
                                setSearchQuery(e.target.value);
                                setCurrentPage(1);
                            }}
                            className="w-full px-4 py-2 bg-[#2e3a57]/70 border border-[#d1b98a]/30 rounded-lg text-[#f4e9c8] placeholder-[#f4e9c8]/40 focus:ring-2 focus:ring-[#d1b98a] focus:border-[#d1b98a] transition-all"
                        />
                    </div>

                    {/* Lọc vai trò */}
                    <div>
                        <label className="block text-sm font-medium text-[#f4e9c8]/80 mb-2">Vai trò</label>
                        <select
                            value={roleFilter}
                            onChange={(e) => {
                                setRoleFilter(e.target.value);
                                setCurrentPage(1);
                            }}
                            className="w-full px-4 py-2 bg-[#2e3a57]/70 border border-[#d1b98a]/30 rounded-lg text-[#f4e9c8] focus:ring-2 focus:ring-[#d1b98a] focus:border-[#d1b98a] transition-all cursor-pointer"
                        >
                            <option value="all">Tất cả vai trò</option>
                            <option value="USER">Người dùng</option>
                            <option value="ADMIN">Quản trị viên</option>
                            <option value="MODERATOR">Kiểm duyệt viên</option>
                        </select>
                    </div>

                    {/* Lọc trạng thái */}
                    <div>
                        <label className="block text-sm font-medium text-[#f4e9c8]/80 mb-2">Trạng thái</label>
                        <select
                            value={statusFilter}
                            onChange={(e) => {
                                setStatusFilter(e.target.value);
                                setCurrentPage(1);
                            }}
                            className="w-full px-4 py-2 bg-[#2e3a57]/70 border border-[#d1b98a]/30 rounded-lg text-[#f4e9c8] focus:ring-2 focus:ring-[#d1b98a] focus:border-[#d1b98a] transition-all cursor-pointer"
                        >
                            <option value="all">Tất cả trạng thái</option>
                            <option value="active">Đang hoạt động</option>
                            <option value="banned">Đã bị khóa</option>
                        </select>
                    </div>
                </div>

                {/* Kết quả */}
                <div className="mt-4 pt-4 border-t border-[#2e3a57]">
                    <div className="flex items-center text-sm text-[#f4e9c8]/80">
                        <span className="text-xl mr-2">📊</span>
                        <span>
              Hiển thị{' '}
                            <span className="text-[#d1b98a] font-bold">
                {currentUsers.length}
              </span>{' '}
                            / {filteredUsers.length} người dùng (tổng cộng {users.length})
            </span>
                    </div>
                    <div className="flex items-center text-sm text-[#f4e9c8]/60 mt-1">
                        <span className="mr-2">Trang:</span>
                        <span className="text-[#d1b98a] font-semibold">
                            {currentPage} / {totalPages}
                        </span>
                    </div>
                </div>
            </div>

            {/* Danh sách người dùng */}
            <div className="grid gap-4">
                {currentUsers.length > 0 ? (
                    currentUsers.map((user) => (
                        <UserCard
                            key={user.id}
                            user={user}
                            onBan={onBan}
                            onUnban={onUnban}
                            onViewDetail={onViewDetail}
                        />
                    ))
                ) : (
                    <div className="bg-[#1b2233]/90 p-16 rounded-2xl border-2 border-dashed border-[#2e3a57] text-center">
                        <div className="text-6xl mb-4">🔍</div>
                        <p className="text-[#f4e9c8] text-xl font-semibold mb-2">
                            Không tìm thấy người dùng
                        </p>
                        <p className="text-[#f4e9c8]/60 text-sm">
                            Hãy thử điều chỉnh bộ lọc của bạn
                        </p>
                    </div>
                )}
            </div>

            {/* ✅ Phân trang */}
            {totalPages > 1 && (
                <div className="flex justify-center items-center mt-6 gap-2">
                    <button
                        onClick={() => handlePageChange(currentPage - 1)}
                        disabled={currentPage === 1}
                        className={`px-4 py-2 rounded-lg font-semibold transition-all flex items-center gap-2 ${
                            currentPage === 1
                                ? 'bg-gray-500/20 text-gray-500 cursor-not-allowed'
                                : 'bg-[#2e3a57] text-[#f4e9c8] hover:bg-[#d1b98a] hover:text-[#20283d]'
                        }`}
                    >
                        ← Trước
                    </button>

                    <div className="flex gap-1 mx-4">
                        {[...Array(totalPages)].map((_, i) => {
                            const page = i + 1;
                            // Hiển thị tối đa 5 trang, với trang hiện tại ở giữa
                            if (
                                page === 1 ||
                                page === totalPages ||
                                (page >= currentPage - 1 && page <= currentPage + 1)
                            ) {
                                return (
                                    <button
                                        key={i}
                                        onClick={() => handlePageChange(page)}
                                        className={`min-w-10 h-10 rounded-lg text-sm font-semibold transition-all ${
                                            currentPage === page
                                                ? 'bg-[#d1b98a] text-[#20283d]'
                                                : 'bg-[#1b2233]/80 text-[#f4e9c8] hover:bg-[#2e3a57]'
                                        }`}
                                    >
                                        {page}
                                    </button>
                                );
                            } else if (page === currentPage - 2 || page === currentPage + 2) {
                                return (
                                    <span key={i} className="min-w-10 h-10 flex items-center justify-center text-[#f4e9c8]/40">
                                        ...
                                    </span>
                                );
                            }
                            return null;
                        })}
                    </div>

                    <button
                        onClick={() => handlePageChange(currentPage + 1)}
                        disabled={currentPage === totalPages}
                        className={`px-4 py-2 rounded-lg font-semibold transition-all flex items-center gap-2 ${
                            currentPage === totalPages
                                ? 'bg-gray-500/20 text-gray-500 cursor-not-allowed'
                                : 'bg-[#2e3a57] text-[#f4e9c8] hover:bg-[#d1b98a] hover:text-[#20283d]'
                        }`}
                    >
                        Sau →
                    </button>
                </div>
            )}
        </div>
    );
};

export default UserList;