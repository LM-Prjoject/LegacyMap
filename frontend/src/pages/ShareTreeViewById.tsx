import { useEffect, useState } from 'react';
import { useParams, useNavigate } from 'react-router-dom';
import api, { type FamilyTree, type Person, type Relationship } from '@/api/trees';
import { showToast } from '@/lib/toast';
import TreeGraph from '@/components/familyTree/TreeGraph';
import { Loader, Lock, ArrowLeft, Users, Heart, Eye, Edit3 } from 'lucide-react';

/**
 * Trang xem cây gia phả theo treeId (dành cho user có quyền VIEW)
 * Khác với SharedTreeView (dùng shareToken), trang này dùng treeId trực tiếp
 */
export default function ShareTreeViewById() {
    const { treeId } = useParams<{ treeId: string }>();
    const navigate = useNavigate();
    const [loading, setLoading] = useState(true);
    const [tree, setTree] = useState<FamilyTree | null>(null);
    const [members, setMembers] = useState<Person[]>([]);
    const [relationships, setRelationships] = useState<Relationship[]>([]);
    const [error, setError] = useState<string | null>(null);

    const userStr = localStorage.getItem('user');
    const user = userStr ? JSON.parse(userStr) : null;
    const userId = user?.id;

    useEffect(() => {
        if (!treeId || !userId) {
            setError('Thiếu thông tin truy cập');
            setLoading(false);
            return;
        }

        loadTree();
    }, [treeId, userId]);

    const loadTree = async () => {
        if (!treeId || !userId) return;

        setLoading(true);
        setError(null);

        try {
            // Kiểm tra quyền truy cập
            const { accessLevel } = await api.checkTreeAccess(treeId, userId);

            // Nếu có quyền edit/admin → chuyển sang TreeDetails
            if (accessLevel === 'edit' || accessLevel === 'admin') {
                navigate(`/trees/${treeId}`);
                return;
            }

            // Load tree data với quyền viewer
            const [treeData, membersData, relationshipsData] = await Promise.all([
                // ✅ SỬA: Dùng listTrees() - trả về cả cây owned và cây được share
                api.listTrees(userId).then(trees => 
                    trees.find(t => t.id === treeId) || null
                ),
                // Dùng API thông thường, backend sẽ tự check quyền
                api.listMembers(userId, treeId),
                api.listRelationships(userId, treeId),
            ]);

            if (!treeData) {
                throw new Error('Không tìm thấy cây gia phả');
            }

            setTree(treeData);
            setMembers(membersData);

            // Chuẩn hóa quan hệ
            const normalizedRelationships = relationshipsData.map(rel => {
                if (String(rel.type).toUpperCase() === 'CHILD') {
                    return {
                        ...rel,
                        type: 'PARENT' as const,
                        fromPersonId: rel.toPersonId,
                        toPersonId: rel.fromPersonId
                    };
                }
                return rel;
            });

            setRelationships(normalizedRelationships);

        } catch (e: any) {
            const errorMsg = e?.message || 'Không thể tải cây gia phả';
            setError(errorMsg);
            showToast.error(errorMsg);
        } finally {
            setLoading(false);
        }
    };

    const handleRequestEditAccess = async () => {
        if (!tree?.id || !userId) {
            showToast.error('Không thể gửi yêu cầu');
            return;
        }

        try {
            await api.requestEditAccess(userId, tree.id);
            showToast.success('Đã gửi yêu cầu quyền chỉnh sửa đến chủ sở hữu');
        } catch (e: any) {
            showToast.error(e?.message || 'Gửi yêu cầu thất bại');
        }
    };

    if (loading) {
        return (
            <div className="min-h-screen bg-gradient-to-br from-slate-900 via-blue-900 to-slate-900 flex items-center justify-center">
                <div className="text-center">
                    <Loader className="w-12 h-12 animate-spin text-blue-400 mx-auto mb-4" />
                    <p className="text-slate-300 text-lg">Đang tải cây gia phả...</p>
                </div>
            </div>
        );
    }

    if (error || !tree) {
        return (
            <div className="min-h-screen bg-gradient-to-br from-slate-900 via-blue-900 to-slate-900 flex items-center justify-center p-4">
                <div className="bg-white/10 backdrop-blur-lg rounded-2xl p-8 max-w-md w-full border border-white/20">
                    <div className="text-center">
                        <Lock className="w-16 h-16 text-red-400 mx-auto mb-4" />
                        <h2 className="text-2xl font-bold text-white mb-2">
                            Không thể truy cập
                        </h2>
                        <p className="text-slate-300 mb-6">
                            {error || 'Cây gia phả không tồn tại hoặc bạn không có quyền truy cập.'}
                        </p>
                        <button
                            onClick={() => navigate('/dashboard')}
                            className="px-6 py-3 bg-blue-600 hover:bg-blue-700 text-white rounded-lg transition-colors flex items-center gap-2 mx-auto"
                        >
                            <ArrowLeft className="w-4 h-4" />
                            Về dashboard
                        </button>
                    </div>
                </div>
            </div>
        );
    }

    const livingMembers = members.filter(member => !member.deathDate).length;
    const coupleRelationships = relationships.filter(rel => String(rel.type).toUpperCase() === 'SPOUSE').length;

    return (
        <div className="min-h-screen bg-gradient-to-br from-slate-900 via-blue-900 to-slate-900 relative">
            {/* Header */}
            <header className="bg-white/10 backdrop-blur-lg border-b border-white/20 sticky top-0 z-50 relative">
                <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-4">
                    <div className="flex items-center justify-between">
                        <button
                            onClick={() => navigate('/dashboard')}
                            className="flex items-center gap-2 text-slate-300 hover:text-white transition-colors"
                        >
                            <ArrowLeft className="w-5 h-5" />
                            <span className="hidden sm:inline">Về dashboard</span>
                        </button>

                        <div className="text-center flex-1">
                            <h1 className="text-2xl font-bold text-white">{tree.name}</h1>
                            {tree.description && (
                                <p className="text-sm text-slate-300 mt-1">{tree.description}</p>
                            )}
                        </div>

                        <div className="flex items-center gap-2">
                            <div className="flex items-center gap-1 px-3 py-1.5 bg-gray-500/20 text-gray-300 rounded-full text-sm">
                                <Eye className="w-4 h-4" />
                                <span>Chỉ xem</span>
                            </div>
                        </div>
                    </div>
                </div>
            </header>

            {/* Stats */}
            <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6 relative">
                <div className="grid grid-cols-1 md:grid-cols-3 gap-4 mb-6">
                    <div className="bg-white/10 backdrop-blur-lg rounded-xl p-4 border border-white/20 text-center">
                        <div className="flex items-center justify-center gap-2 text-white">
                            <Users className="w-5 h-5" />
                            <span className="text-2xl font-bold">{members.length}</span>
                        </div>
                        <p className="text-slate-300 text-sm mt-1">Thành viên</p>
                    </div>
                    <div className="bg-white/10 backdrop-blur-lg rounded-xl p-4 border border-white/20 text-center">
                        <div className="flex items-center justify-center gap-2 text-green-400">
                            <Users className="w-5 h-5" />
                            <span className="text-2xl font-bold">{livingMembers}</span>
                        </div>
                        <p className="text-slate-300 text-sm mt-1">Thành viên còn sống</p>
                    </div>
                    <div className="bg-white/10 backdrop-blur-lg rounded-xl p-4 border border-white/20 text-center">
                        <div className="flex items-center justify-center gap-2 text-pink-400">
                            <Heart className="w-5 h-5" />
                            <span className="text-2xl font-bold">{coupleRelationships}</span>
                        </div>
                        <p className="text-slate-300 text-sm mt-1">Cặp đôi</p>
                    </div>
                </div>
            </div>

            {/* Main Content */}
            <main className="p-4 sm:p-6 lg:p-8 relative">
                <div className="max-w-7xl mx-auto">
                    {members.length === 0 ? (
                        <div className="bg-white/10 backdrop-blur-lg rounded-2xl p-12 text-center border border-white/20">
                            <p className="text-slate-300 text-lg">
                                Cây gia phả chưa có thành viên nào.
                            </p>
                        </div>
                    ) : (
                        <div className="bg-white rounded-2xl p-6 shadow-xl">
                            <TreeGraph
                                persons={members}
                                relationships={relationships}
                                onNodeClick={(id) => console.log('Clicked member:', id)}
                            />
                        </div>
                    )}
                </div>
            </main>

            {/* Footer */}
            <footer className="bg-white/5 backdrop-blur-lg border-t border-white/10 py-6 mt-12 relative">
                <div className="max-w-7xl mx-auto px-4 text-center text-slate-400 text-sm">
                    <p className="text-base mb-4">
                        <span className="text-slate-300">Bạn đang xem cây gia phả này với quyền chỉ xem</span>
                    </p>

                    <button
                        onClick={handleRequestEditAccess}
                        className="px-6 py-2.5 bg-blue-600 hover:bg-blue-700 text-white rounded-lg transition-colors font-medium shadow-lg inline-flex items-center gap-2"
                    >
                        <Edit3 className="w-4 h-4" />
                        Yêu cầu quyền chỉnh sửa
                    </button>
                </div>
            </footer>
        </div>
    );
}
