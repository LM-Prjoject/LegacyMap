// src/pages/admin/FamilyTreesPage.tsx
import React, { useEffect, useState } from 'react';
import { adminApi, FamilyTree } from '@/api/ts_admin';

const FamilyTreesPage: React.FC = () => {
    const [familyTrees, setFamilyTrees] = useState<FamilyTree[]>([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);
    const [searchTerm, setSearchTerm] = useState('');
    const [showPublicOnly, setShowPublicOnly] = useState<boolean | null>(null);

    useEffect(() => {
        fetchFamilyTrees();
    }, []);

    const fetchFamilyTrees = async () => {
        try {
            setLoading(true);
            setError(null);
            const trees = await adminApi.getAllFamilyTrees();
            setFamilyTrees(trees || []);
        } catch (err: any) {
            const errorMsg = err.response?.data?.message || err.message || 'Unknown error';
            setError(errorMsg);
        } finally {
            setLoading(false);
        }
    };

    const formatDate = (dateString: string) => {
        return new Date(dateString).toLocaleDateString('vi-VN', {
            year: 'numeric',
            month: 'short',
            day: 'numeric'
        });
    };


    const filteredTrees = familyTrees.filter(tree => {
        const matchesSearch = searchTerm === '' ||
            tree.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
            tree.createdByEmail?.toLowerCase().includes(searchTerm.toLowerCase()) ||
            tree.createdByUsername?.toLowerCase().includes(searchTerm.toLowerCase());

        const matchesVisibility = showPublicOnly === null || tree.isPublic === showPublicOnly;

        return matchesSearch && matchesVisibility;
    });

    if (loading) {
        return (
            <div className="flex flex-col justify-center items-center h-96">
                <div className="w-16 h-16 border-4 border-[#D1B066] border-t-transparent rounded-full animate-spin"></div>
                <p className="text-white/80 mt-4">Đang tải danh sách cây gia phả...</p>
            </div>
        );
    }

    if (error) {
        return (
            <div className="bg-red-500/20 border border-red-400/50 rounded-xl p-8">
                <div className="flex items-center">
                    <span className="text-4xl mr-3">⚠️</span>
                    <div>
                        <h3 className="text-xl font-bold text-red-200 mb-2">Lỗi Tải Dữ Liệu</h3>
                        <p className="text-red-300 mb-4">{error}</p>
                        <button
                            onClick={fetchFamilyTrees}
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
                    <h1 className="text-4xl font-bold text-white mb-2 flex items-center">
                        <span className="text-4xl mr-3">🌳</span>
                        Quản Lý Cây Gia Phả
                    </h1>
                    <p className="text-white/60">
                        Xem và quản lý tất cả cây gia phả trong hệ thống
                    </p>
                </div>
                <button
                    onClick={fetchFamilyTrees}
                    className="flex items-center px-6 py-3 bg-[#D1B066] text-[#084289] rounded-lg hover:bg-[#f4d88a] transition-all font-bold"
                >
                    <span className="mr-2">🔄</span>
                    Làm Mới
                </button>
            </div>

            {/* Stats Cards */}
            <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-8">
                <div className="bg-[#2563eb] rounded-xl p-6 shadow-lg">
                    <div className="flex items-center justify-between">
                        <div>
                            <p className="text-white/80 text-sm mb-1">Tổng Cây Gia Phả</p>
                            <p className="text-4xl font-bold text-white">{familyTrees.length}</p>
                        </div>
                        <div className="text-5xl">🌳</div>
                    </div>
                </div>

                <div className="bg-green-500 rounded-xl p-6 shadow-lg">
                    <div className="flex items-center justify-between">
                        <div>
                            <p className="text-white/80 text-sm mb-1">Cây Công Khai</p>
                            <p className="text-4xl font-bold text-white">
                                {familyTrees.filter(t => t.isPublic).length}
                            </p>
                        </div>
                        <div className="text-5xl">🌍</div>
                    </div>
                </div>

                <div className="bg-orange-500 rounded-xl p-6 shadow-lg">
                    <div className="flex items-center justify-between">
                        <div>
                            <p className="text-white/80 text-sm mb-1">Cây Riêng Tư</p>
                            <p className="text-4xl font-bold text-white">
                                {familyTrees.filter(t => !t.isPublic).length}
                            </p>
                        </div>
                        <div className="text-5xl">🔒</div>
                    </div>
                </div>
            </div>

            {/* Search and Filter Bar */}
            <div className="bg-[#084289] rounded-xl border border-[#0a4a9e] p-6 mb-6 shadow-lg">
                <div className="flex flex-col md:flex-row gap-4">
                    {/* Search Input */}
                    <div className="flex-1">
                        <input
                            type="text"
                            placeholder="🔍 Tìm kiếm theo tên, email hoặc người tạo..."
                            value={searchTerm}
                            onChange={(e) => setSearchTerm(e.target.value)}
                            className="w-full px-4 py-3 bg-[#0a4a9e]/50 border border-[#D1B066]/30 rounded-lg text-white placeholder-white/40 focus:ring-2 focus:ring-[#D1B066] focus:border-[#D1B066] transition-all"
                        />
                    </div>

                    {/* Visibility Filter */}
                    <div className="flex gap-2">
                        <button
                            onClick={() => setShowPublicOnly(null)}
                            className={`px-5 py-3 rounded-lg font-medium transition-all ${
                                showPublicOnly === null
                                    ? 'bg-[#D1B066] text-[#084289]'
                                    : 'bg-[#0a4a9e]/50 text-white hover:bg-[#0a4a9e]'
                            }`}
                        >
                            Tất cả
                        </button>
                        <button
                            onClick={() => setShowPublicOnly(true)}
                            className={`px-5 py-3 rounded-lg font-medium transition-all ${
                                showPublicOnly === true
                                    ? 'bg-green-500 text-white'
                                    : 'bg-[#0a4a9e]/50 text-white hover:bg-[#0a4a9e]'
                            }`}
                        >
                            🌍 Công khai
                        </button>
                        <button
                            onClick={() => setShowPublicOnly(false)}
                            className={`px-5 py-3 rounded-lg font-medium transition-all ${
                                showPublicOnly === false
                                    ? 'bg-orange-500 text-white'
                                    : 'bg-[#0a4a9e]/50 text-white hover:bg-[#0a4a9e]'
                            }`}
                        >
                            🔒 Riêng tư
                        </button>
                    </div>
                </div>

                {/* Results count */}
                {searchTerm || showPublicOnly !== null ? (
                    <div className="mt-4 text-sm text-white/70">
                        Hiển thị {filteredTrees.length} / {familyTrees.length} cây gia phả
                    </div>
                ) : null}
            </div>

            {/* Trees List */}
            {filteredTrees.length === 0 ? (
                <div className="bg-[#084289] border-2 border-dashed border-[#0a4a9e] rounded-xl p-16 text-center">
                    <div className="text-6xl mb-4">🌱</div>
                    <h3 className="text-2xl font-bold text-white mb-2">
                        {familyTrees.length === 0 ? 'Chưa Có Cây Gia Phả' : 'Không Tìm Thấy Kết Quả'}
                    </h3>
                    <p className="text-white/60">
                        {familyTrees.length === 0
                            ? 'Cây gia phả sẽ xuất hiện ở đây khi người dùng tạo.'
                            : 'Hãy thử điều chỉnh bộ lọc của bạn.'
                        }
                    </p>
                </div>
            ) : (
                <div className="bg-[#084289] rounded-xl border border-[#0a4a9e] overflow-hidden shadow-lg">
                    <table className="min-w-full">
                        <thead className="bg-[#0a4a9e]">
                        <tr>
                            <th className="px-6 py-4 text-left text-xs font-semibold text-white/80 uppercase tracking-wider">
                                Tên Cây
                            </th>
                            <th className="px-6 py-4 text-left text-xs font-semibold text-white/80 uppercase tracking-wider">
                                Người Tạo
                            </th>
                            <th className="px-6 py-4 text-left text-xs font-semibold text-white/80 uppercase tracking-wider">
                                Trạng Thái
                            </th>
                            <th className="px-6 py-4 text-left text-xs font-semibold text-white/80 uppercase tracking-wider">
                                Ngày Tạo
                            </th>
                        </tr>
                        </thead>
                        <tbody className="divide-y divide-[#0a4a9e]">
                        {filteredTrees.map((tree) => (
                            <tr key={tree.id} className="hover:bg-[#0a4a9e]/30 transition-colors">
                                <td className="px-6 py-4 whitespace-nowrap">
                                    <div className="flex items-center">
                                        <div className="w-10 h-10 bg-green-500 rounded-lg flex items-center justify-center mr-3">
                                            <span className="text-xl">🌳</span>
                                        </div>
                                        <div>
                                            <div className="text-sm font-medium text-white">
                                                {tree.name}
                                            </div>
                                            {tree.description && (
                                                <div className="text-sm text-white/60 truncate max-w-xs">
                                                    {tree.description}
                                                </div>
                                            )}
                                        </div>
                                    </div>
                                </td>
                                <td className="px-6 py-4 whitespace-nowrap">
                                    <div className="text-sm text-white">
                                        {tree.createdByUsername || 'Không rõ'}
                                    </div>
                                    <div className="text-sm text-white/60">
                                        {tree.createdByEmail || 'Không có email'}
                                    </div>
                                </td>
                                <td className="px-6 py-4 whitespace-nowrap">
                                    <span className={`px-3 py-1 inline-flex text-xs leading-5 font-semibold rounded-lg ${
                                        tree.isPublic
                                            ? 'bg-green-500/20 text-green-300'
                                            : 'bg-gray-500/20 text-gray-300'
                                    }`}>
                                        {tree.isPublic ? '🌍 Công khai' : '🔒 Riêng tư'}
                                    </span>
                                </td>
                                <td className="px-6 py-4 whitespace-nowrap text-sm text-white/70">
                                    {formatDate(tree.createdAt)}
                                </td>
                            </tr>
                        ))}
                        </tbody>
                    </table>
                </div>
            )}
        </div>
    );
};

export default FamilyTreesPage;