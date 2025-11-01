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
            const errorMsg =
                err.response?.data?.message || err.message || 'Unknown error';
            setError(errorMsg);
        } finally {
            setLoading(false);
        }
    };

    const formatDate = (dateString: string) => {
        return new Date(dateString).toLocaleDateString('vi-VN', {
            year: 'numeric',
            month: 'short',
            day: 'numeric',
        });
    };

    const filteredTrees = familyTrees.filter((tree) => {
        const matchesSearch =
            searchTerm === '' ||
            tree.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
            tree.createdByEmail?.toLowerCase().includes(searchTerm.toLowerCase()) ||
            tree.createdByUsername?.toLowerCase().includes(searchTerm.toLowerCase());

        const matchesVisibility =
            showPublicOnly === null || tree.isPublic === showPublicOnly;

        return matchesSearch && matchesVisibility;
    });

    if (loading) {
        return (
            <div className="flex flex-col justify-center items-center h-96">
                <div className="w-16 h-16 border-4 border-[#d1b98a] border-t-transparent rounded-full animate-spin"></div>
                <p className="text-[#f4e9c8]/80 mt-4">
                    Đang tải danh sách cây gia phả...
                </p>
            </div>
        );
    }

    if (error) {
        return (
            <div className="bg-red-900/30 border border-red-700/50 rounded-xl p-8">
                <div className="flex items-center">
                    <span className="text-4xl mr-3">⚠️</span>
                    <div>
                        <h3 className="text-xl font-bold text-red-300 mb-2">
                            Lỗi Tải Dữ Liệu
                        </h3>
                        <p className="text-red-200 mb-4">{error}</p>
                        <button
                            onClick={fetchFamilyTrees}
                            className="px-6 py-3 bg-gradient-to-br from-[#d1b98a] to-[#f4e9c8] text-[#20283d] rounded-lg hover:scale-105 transition-all font-semibold"
                        >
                            Thử Lại
                        </button>
                    </div>
                </div>
            </div>
        );
    }

    return (
        <div className="text-[#f4e9c8]">
            {/* Header */}
            <div className="flex justify-between items-center mb-8">
                <div>
                    <h1 className="text-4xl font-bold mb-2 flex items-center">
                        <span className="text-4xl mr-3">🌳</span>
                        Quản Lý Cây Gia Phả
                    </h1>
                    <p className="text-[#f4e9c8]/70">
                        Xem và quản lý tất cả cây gia phả trong hệ thống
                    </p>
                </div>
                <button
                    onClick={fetchFamilyTrees}
                    className="flex items-center px-6 py-3 bg-gradient-to-br from-[#d1b98a] to-[#f4e9c8] text-[#20283d] rounded-lg hover:scale-105 transition-all font-bold"
                >
                    <span className="mr-2">🔄</span>
                    Làm Mới
                </button>
            </div>

            {/* Stats Cards */}
            <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-8">
                <StatCard
                    label="Tổng Cây Gia Phả"
                    value={familyTrees.length}
                    icon="🌳"
                    color="gold"
                />
                <StatCard
                    label="Cây Công Khai"
                    value={familyTrees.filter((t) => t.isPublic).length}
                    icon="🌍"
                    color="green"
                />
                <StatCard
                    label="Cây Riêng Tư"
                    value={familyTrees.filter((t) => !t.isPublic).length}
                    icon="🔒"
                    color="blue"
                />
            </div>

            {/* Search and Filter */}
            <div className="bg-gradient-to-br from-[#1b2233] to-[#2e3a57] border border-[#2e3a57] rounded-xl p-6 mb-6 shadow-lg">
                <div className="flex flex-col md:flex-row gap-4">
                    <input
                        type="text"
                        placeholder="🔍 Tìm kiếm theo tên, email hoặc người tạo..."
                        value={searchTerm}
                        onChange={(e) => setSearchTerm(e.target.value)}
                        className="flex-1 px-4 py-3 bg-[#2e3a57]/70 border border-[#d1b98a]/30 rounded-lg text-[#f4e9c8] placeholder-[#f4e9c8]/40 focus:ring-2 focus:ring-[#d1b98a] focus:border-[#d1b98a] transition-all"
                    />

                    <div className="flex gap-2">
                        <FilterButton
                            active={showPublicOnly === null}
                            onClick={() => setShowPublicOnly(null)}
                            label="Tất cả"
                        />
                        <FilterButton
                            active={showPublicOnly === true}
                            onClick={() => setShowPublicOnly(true)}
                            label="🌍 Công khai"
                        />
                        <FilterButton
                            active={showPublicOnly === false}
                            onClick={() => setShowPublicOnly(false)}
                            label="🔒 Riêng tư"
                        />
                    </div>
                </div>

                {(searchTerm || showPublicOnly !== null) && (
                    <div className="mt-4 text-sm text-[#f4e9c8]/60">
                        Hiển thị {filteredTrees.length} / {familyTrees.length} cây gia phả
                    </div>
                )}
            </div>

            {/* Table */}
            {filteredTrees.length === 0 ? (
                <div className="bg-gradient-to-br from-[#1b2233] to-[#2e3a57] border border-[#2e3a57] rounded-xl p-16 text-center">
                    <div className="text-6xl mb-4">🌱</div>
                    <h3 className="text-2xl font-bold mb-2">
                        {familyTrees.length === 0
                            ? 'Chưa Có Cây Gia Phả'
                            : 'Không Tìm Thấy Kết Quả'}
                    </h3>
                    <p className="text-[#f4e9c8]/60">
                        {familyTrees.length === 0
                            ? 'Cây gia phả sẽ xuất hiện ở đây khi người dùng tạo.'
                            : 'Hãy thử điều chỉnh bộ lọc của bạn.'}
                    </p>
                </div>
            ) : (
                <div className="bg-gradient-to-br from-[#1b2233] to-[#2e3a57] rounded-xl border border-[#2e3a57] overflow-hidden shadow-lg">
                    <table className="min-w-full">
                        <thead className="bg-[#2e3a57]/80">
                        <tr>
                            {['Tên Cây', 'Người Tạo', 'Trạng Thái', 'Ngày Tạo'].map(
                                (h) => (
                                    <th
                                        key={h}
                                        className="px-6 py-4 text-left text-xs font-semibold text-[#f4e9c8]/70 uppercase tracking-wider"
                                    >
                                        {h}
                                    </th>
                                )
                            )}
                        </tr>
                        </thead>
                        <tbody className="divide-y divide-[#2e3a57]/60">
                        {filteredTrees.map((tree) => (
                            <tr
                                key={tree.id}
                                className="hover:bg-[#2e3a57]/40 transition-all duration-200"
                            >
                                <td className="px-6 py-4 whitespace-nowrap">
                                    <div className="flex items-center">
                                        <div className="w-10 h-10 bg-gradient-to-br from-[#d1b98a] to-[#f4e9c8] rounded-lg flex items-center justify-center mr-3 text-[#20283d] text-xl">
                                            🌳
                                        </div>
                                        <div>
                                            <div className="text-sm font-medium">
                                                {tree.name}
                                            </div>
                                            {tree.description && (
                                                <div className="text-sm text-[#f4e9c8]/60 truncate max-w-xs">
                                                    {tree.description}
                                                </div>
                                            )}
                                        </div>
                                    </div>
                                </td>
                                <td className="px-6 py-4 whitespace-nowrap text-sm">
                                    <div>{tree.createdByUsername || 'Không rõ'}</div>
                                    <div className="text-[#f4e9c8]/60">
                                        {tree.createdByEmail || 'Không có email'}
                                    </div>
                                </td>
                                <td className="px-6 py-4 whitespace-nowrap">
                    <span
                        className={`px-3 py-1 inline-flex text-xs leading-5 font-semibold rounded-lg ${
                            tree.isPublic
                                ? 'bg-green-700/20 text-green-300 border border-green-500/30'
                                : 'bg-gray-700/30 text-gray-300 border border-gray-500/30'
                        }`}
                    >
                      {tree.isPublic ? '🌍 Công khai' : '🔒 Riêng tư'}
                    </span>
                                </td>
                                <td className="px-6 py-4 whitespace-nowrap text-sm text-[#f4e9c8]/70">
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

/* ========== Sub Components ========== */

const StatCard = ({
                      label,
                      value,
                      icon,
                      color,
                  }: {
    label: string;
    value: number;
    icon: string;
    color: 'gold' | 'green' | 'blue';
}) => {
    const colors = {
        gold: 'from-[#d1b98a] to-[#f4e9c8] text-[#20283d]',
        green: 'from-green-700/40 to-green-600/20 text-green-200',
        blue: 'from-blue-700/40 to-blue-600/20 text-blue-200',
    };
    return (
        <div
            className={`bg-gradient-to-br ${colors[color]} rounded-xl p-6 shadow-lg border border-[#2e3a57]/50`}
        >
            <div className="flex items-center justify-between">
                <div>
                    <p className="text-sm font-medium mb-1 opacity-80">{label}</p>
                    <p className="text-4xl font-bold">{value}</p>
                </div>
                <div className="text-5xl opacity-80">{icon}</div>
            </div>
        </div>
    );
};

const FilterButton = ({
                          active,
                          onClick,
                          label,
                      }: {
    active: boolean;
    onClick: () => void;
    label: string;
}) => (
    <button
        onClick={onClick}
        className={`
      px-5 py-3 rounded-lg font-medium transition-all border
      ${
            active
                ? 'bg-gradient-to-br from-[#d1b98a] to-[#f4e9c8] text-[#20283d] border-[#f4e9c8]'
                : 'bg-[#2e3a57]/50 text-[#f4e9c8] border-[#d1b98a]/30 hover:bg-[#2e3a57]/70'
        }
    `}
    >
        {label}
    </button>
);

export default FamilyTreesPage;
