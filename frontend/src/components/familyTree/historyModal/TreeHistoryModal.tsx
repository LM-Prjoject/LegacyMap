import React, { useState, useEffect } from 'react';
import { treeHistoryApi, TreeHistoryItem } from '../../../api/treeHistory';
import { formatDistanceToNow } from 'date-fns';
import { vi } from 'date-fns/locale';

interface Props {
    treeId: string;
    isOpen: boolean;
    onClose: () => void;
}

export const TreeHistoryModal: React.FC<Props> = ({ treeId, isOpen, onClose }) => {
    const [history, setHistory] = useState<TreeHistoryItem[]>([]);
    const [loading, setLoading] = useState(false);
    const [page, setPage] = useState(0);
    const [hasMore, setHasMore] = useState(true);

    useEffect(() => {
        if (isOpen) {
            loadHistory();
        }
    }, [isOpen, page]);

    const loadHistory = async () => {
        setLoading(true);
        try {
            const response = await treeHistoryApi.getHistory(treeId, page, 20);
            setHistory(prev => page === 0 ? response.content : [...prev, ...response.content]);
            setHasMore(response.number < response.totalPages - 1);
        } catch (error) {
            console.error('Failed to load history', error);
        } finally {
            setLoading(false);
        }
    };

    const getActionText = (action: string) => {
        switch (action) {
            case 'CREATED': return 'Đã thêm';
            case 'UPDATED': return 'Đã cập nhật';
            case 'DELETED': return 'Đã xóa';
            default: return action;
        }
    };

    const getActionColor = (action: string) => {
        switch (action) {
            case 'CREATED': return 'text-green-400 bg-green-500/10 border-green-500/30';
            case 'UPDATED': return 'text-blue-400 bg-blue-500/10 border-blue-500/30';
            case 'DELETED': return 'text-red-400 bg-red-500/10 border-red-500/30';
            default: return 'text-gray-400 bg-gray-500/10 border-gray-500/30';
        }
    };

    if (!isOpen) return null;

    return (
        <div className="fixed inset-0 z-50 flex items-center justify-center p-4">
            {/* Backdrop */}
            <div
                className="absolute inset-0 bg-gradient-to-br from-[#0f1419]/95 via-[#1e2a3a]/95 to-[#0f1419]/95 backdrop-blur-sm"
                onClick={onClose}
            />

            {/* Glow effects */}
            <div className="absolute top-1/4 left-1/4 w-96 h-96 bg-[#ffd89b]/10 rounded-full blur-[120px] pointer-events-none" />
            <div className="absolute bottom-1/4 right-1/4 w-96 h-96 bg-[#d4af7a]/10 rounded-full blur-[120px] pointer-events-none" />

            <div className="relative bg-gradient-to-br from-[#1e2a3a]/95 via-[#0f1419]/90 to-[#1e2a3a]/95 backdrop-blur-2xl rounded-2xl shadow-[0_0_60px_rgba(255,216,155,0.15),0_20px_80px_rgba(0,0,0,0.6)] w-full max-w-3xl max-h-[85vh] flex flex-col border-2 border-[#ffd89b]/20 overflow-hidden">

                {/* Top glow */}
                <div className="absolute top-0 left-0 right-0 h-[2px] bg-gradient-to-r from-transparent via-[#ffd89b] to-transparent opacity-60" />

                {/* Header */}
                <div className="relative flex items-center justify-between p-6 border-b border-[#ffd89b]/20 bg-gradient-to-r from-[#ffd89b]/5 to-transparent">
                    <div className="flex items-center gap-3">
                        <div className="relative p-2.5 bg-gradient-to-br from-[#ffd89b]/20 to-[#d4af7a]/20 rounded-xl border border-[#ffd89b]/30">
                            <span className="text-2xl">📜</span>
                        </div>
                        <div>
                            <h2 className="text-xl font-bold text-[#ffd89b]">Lịch sử chỉnh sửa</h2>
                            <p className="text-sm text-gray-400">Theo dõi các thay đổi trong cây gia phả</p>
                        </div>
                    </div>
                    <button
                        onClick={onClose}
                        className="p-2 hover:bg-[#ffd89b]/10 rounded-lg transition-all text-gray-400 hover:text-[#ffd89b] border border-transparent hover:border-[#ffd89b]/30"
                    >
                        ✕
                    </button>
                </div>

                {/* Content */}
                <div className="flex-1 overflow-y-auto p-6 custom-scrollbar">
                    {loading && page === 0 ? (
                        <div className="text-center py-12">
                            <div className="inline-block w-10 h-10 border-3 border-[#ffd89b]/30 border-t-[#ffd89b] rounded-full animate-spin mb-4" />
                            <p className="text-gray-400 font-medium">Đang tải lịch sử...</p>
                        </div>
                    ) : history.length === 0 ? (
                        <div className="text-center py-12">
                            <div className="text-7xl opacity-20 mb-6">📜</div>
                            <h3 className="text-xl font-bold text-white mb-3">Chưa có lịch sử chỉnh sửa</h3>
                            <p className="text-gray-400 mb-6 max-w-md mx-auto">
                                Lịch sử các thay đổi về thành viên và mối quan hệ sẽ được hiển thị tại đây.
                            </p>
                        </div>
                    ) : (
                        <div className="space-y-3">
                            {history.map((item) => (
                                <div
                                    key={item.id}
                                    className="bg-gradient-to-r from-white/5 to-white/10 hover:from-white/10 hover:to-white/15 border border-[#ffd89b]/10 hover:border-[#ffd89b]/30 rounded-xl p-4 transition-all duration-300"
                                >
                                    <div className="flex gap-4">
                                        {/* Avatar */}
                                        <div className="flex-shrink-0">
                                            {item.userAvatar ? (
                                                <img
                                                    src={item.userAvatar}
                                                    alt={item.userName}
                                                    className="w-12 h-12 rounded-full border-2 border-[#ffd89b]/30"
                                                />
                                            ) : (
                                                <div className="w-12 h-12 rounded-full bg-gradient-to-br from-[#d4af7a] to-[#ffd89b] flex items-center justify-center text-[#0f1419] font-bold text-lg">
                                                    {item.userName.charAt(0).toUpperCase()}
                                                </div>
                                            )}
                                        </div>

                                        {/* Content */}
                                        <div className="flex-1">
                                            <div className="flex items-center gap-2 mb-2 flex-wrap">
                                                <span className="font-bold text-white">{item.userName}</span>
                                                <span className={`px-2.5 py-1 text-xs font-semibold rounded-full border ${getActionColor(item.action)}`}>
                                                    {getActionText(item.action)}
                                                </span>
                                                <span className="text-xs text-gray-500">
                                                    {formatDistanceToNow(new Date(item.createdAt), {
                                                        addSuffix: true,
                                                        locale: vi
                                                    })}
                                                </span>
                                            </div>

                                            <p className="text-gray-300 text-sm mb-3">{item.description}</p>

                                            {item.entityType === 'MEMBER' && (
                                                <span className="inline-flex items-center gap-1.5 px-3 py-1.5 bg-blue-500/10 text-blue-400 text-xs rounded-lg border border-blue-500/30">
                                                    👤 Thành viên
                                                </span>
                                            )}
                                            {item.entityType === 'RELATIONSHIP' && (
                                                <span className="inline-flex items-center gap-1.5 px-3 py-1.5 bg-purple-500/10 text-purple-400 text-xs rounded-lg border border-purple-500/30">
                                                    🔗 Quan hệ
                                                </span>
                                            )}
                                        </div>
                                    </div>
                                </div>
                            ))}

                            {/* Load More */}
                            {hasMore && (
                                <button
                                    onClick={() => setPage(p => p + 1)}
                                    disabled={loading}
                                    className="w-full py-3 bg-gradient-to-r from-white/5 to-white/10 hover:from-[#ffd89b]/10 hover:to-[#ffd89b]/20 text-[#ffd89b] font-medium rounded-xl border border-[#ffd89b]/30 transition-all duration-300"
                                >
                                    {loading ? 'Đang tải...' : 'Xem thêm lịch sử'}
                                </button>
                            )}
                        </div>
                    )}
                </div>

                {/* Bottom glow */}
                <div className="absolute bottom-0 left-0 right-0 h-[2px] bg-gradient-to-r from-transparent via-[#ffd89b] to-transparent opacity-60" />
            </div>

            <style>{`
                .custom-scrollbar::-webkit-scrollbar {
                    width: 8px;
                }
                .custom-scrollbar::-webkit-scrollbar-track {
                    background: rgba(255, 216, 155, 0.05);
                    border-radius: 4px;
                }
                .custom-scrollbar::-webkit-scrollbar-thumb {
                    background: rgba(255, 216, 155, 0.3);
                    border-radius: 4px;
                }
                .custom-scrollbar::-webkit-scrollbar-thumb:hover {
                    background: rgba(255, 216, 155, 0.5);
                }
            `}</style>
        </div>
    );
};