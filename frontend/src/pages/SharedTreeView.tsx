import { useEffect, useState } from 'react';
import { useParams, useNavigate } from 'react-router-dom';
import api, { type FamilyTree, type Person, type Relationship } from '@/api/trees';
import { showToast } from '@/lib/toast';
import TreeGraph from '@/components/familyTree/TreeGraph';
import { Loader, Lock, Globe, ArrowLeft, Users, Heart, Eye, Edit3 } from 'lucide-react';

// ✅ THÊM: Import các component auth
import SignIn from '@/pages/auth/SignIn';
import SignUp from '@/pages/auth/SignUp';
import PasswordReset from '@/pages/auth/password-reset';

export default function SharedTreeView() {
    const { shareToken } = useParams<{ shareToken: string }>();
    const navigate = useNavigate();
    const [loading, setLoading] = useState(true);
    const [tree, setTree] = useState<FamilyTree | null>(null);
    const [members, setMembers] = useState<Person[]>([]);
    const [relationships, setRelationships] = useState<Relationship[]>([]);
    const [error, setError] = useState<string | null>(null);
    const [accessLevel, setAccessLevel] = useState<'view' | 'edit' | 'admin' | null>(null);

    // ✅ THÊM: State quản lý modal auth
    const [showAuthModal, setShowAuthModal] = useState<'signin' | 'signup' | 'reset' | null>(null);

    // Check xem có editMode từ URL không
    const [editMode, setEditMode] = useState(false);
    const isLoggedIn = !!localStorage.getItem('authToken');

    useEffect(() => {
        if (!shareToken) {
            setError('Link chia sẻ không hợp lệ');
            setLoading(false);
            return;
        }

        // Kiểm tra editMode từ URL
        const urlParams = new URLSearchParams(window.location.search);
        if (urlParams.get('editMode') === 'true') {
            setEditMode(true);
        }

        loadSharedTree();
    }, [shareToken]);

    // HOÀN TOÀN THAY THẾ loadSharedTree bằng phiên bản mới dùng getSharedTreeAccessInfo
    const loadSharedTree = async () => {
        if (!shareToken) return;

        setLoading(true);
        setError(null);

        try {
            // Lấy userId nếu đã đăng nhập
            const userStr = localStorage.getItem('user');
            const userId = userStr ? JSON.parse(userStr).id : null;

            // BƯỚC 1: Gọi API mới để lấy thông tin access
            const accessInfo = await api.getSharedTreeAccessInfo(shareToken, userId);

            console.log('Access Info:', accessInfo);

            // BƯỚC 2: Nếu user đã đăng nhập VÀ có quyền edit → chuyển thẳng vào trang chỉnh sửa
            if (userId && accessInfo.canEdit) {
                console.log('User có quyền edit → chuyển đến TreeDetails');

                // Lưu cây vào dashboard (nếu chưa có)
                try {
                    await api.saveSharedTreeToDashboard(userId, accessInfo.treeId);
                    showToast.success('Cây gia phả đã được thêm vào danh sách của bạn');
                } catch (e: any) {
                    console.warn('Tree có thể đã có trong dashboard:', e.message);
                    // Không throw lỗi → vẫn redirect
                }

                // Chuyển hướng đến trang chỉnh sửa chính thức
                navigate(`/trees/${accessInfo.treeId}`);
                return; // DỪNG HOÀN TOÀN tại đây
            }

            // BƯỚC 3: Nếu không có quyền edit → hiển thị trang xem
            const treeData = await api.getSharedTree(shareToken, userId);
            setTree(treeData);
            setAccessLevel(accessInfo.canEdit ? 'edit' : 'view');

            // Load thành viên và quan hệ
            const [membersData, relationshipsData] = await Promise.all([
                api.getSharedTreeMembers(shareToken, userId),
                api.getSharedTreeRelationships(shareToken, userId),
            ]);

            // Chuẩn hóa quan hệ: CHILD → PARENT (đảo chiều)
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

            setMembers(membersData);
            setRelationships(normalizedRelationships);

        } catch (e: any) {
            const errorMsg = e?.message || 'Không thể tải cây gia phả';
            setError(errorMsg);
            showToast.error(errorMsg);
        } finally {
            setLoading(false);
        }
    };

    // ✅ SỬA: Thay vì navigate, hiển thị modal
    const handleLoginRedirect = () => {
        if (!shareToken) return;
        localStorage.setItem('redirectAfterLogin', `/trees/shared/${shareToken}`);
        setShowAuthModal('signin');
    };

    // Bật/tắt chế độ chỉnh sửa (chỉ dành cho người có quyền)
    const handleToggleEditMode = () => {
        if (editMode) {
            setEditMode(false);
            window.history.replaceState({}, '', `/trees/shared/${shareToken}`);
            showToast.info('Đã tắt chế độ chỉnh sửa');
        } else {
            setEditMode(true);
            window.history.replaceState({}, '', `/trees/shared/${shareToken}?editMode=true`);
            showToast.success('Đã bật chế độ chỉnh sửa');
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
                            onClick={() => navigate('/')}
                            className="px-6 py-3 bg-blue-600 hover:bg-blue-700 text-white rounded-lg transition-colors flex items-center gap-2 mx-auto"
                        >
                            <ArrowLeft className="w-4 h-4" />
                            Về trang chủ
                        </button>
                    </div>
                </div>
            </div>
        );
    }

    const livingMembers = members.filter(member => !member.deathDate).length;
    const coupleRelationships = relationships.filter(rel => String(rel.type).toUpperCase() === 'SPOUSE').length;

    return (
        // ✅ SỬA: Thêm DragonsBackground
        <div className="min-h-screen bg-gradient-to-br from-slate-900 via-blue-900 to-slate-900 relative">
            {/* Header */}
            <header className="bg-white/10 backdrop-blur-lg border-b border-white/20 sticky top-0 z-50 relative">
                <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-4">
                    <div className="flex items-center justify-between">
                        <button
                            onClick={() => navigate('/')}
                            className="flex items-center gap-2 text-slate-300 hover:text-white transition-colors"
                        >
                            <ArrowLeft className="w-5 h-5" />
                            <span className="hidden sm:inline">Quay lại</span>
                        </button>

                        <div className="text-center flex-1">
                            <h1 className="text-2xl font-bold text-white">{tree.name}</h1>
                            {tree.description && (
                                <p className="text-sm text-slate-300 mt-1">{tree.description}</p>
                            )}

                            {editMode && (
                                <div className="mt-2 px-3 py-1 bg-orange-500/20 text-orange-300 rounded-full text-sm inline-flex items-center gap-1">
                                    <Edit3 className="w-3 h-3" />
                                    <span>Chế độ chỉnh sửa</span>
                                </div>
                            )}
                        </div>

                        <div className="flex items-center gap-2">
                            {tree.isPublic ? (
                                <div className="flex items-center gap-1 px-3 py-1.5 bg-green-500/20 text-green-300 rounded-full text-sm">
                                    <Globe className="w-4 h-4" />
                                    <span>Công khai</span>
                                </div>
                            ) : (
                                <div className="flex items-center gap-1 px-3 py-1.5 bg-blue-500/20 text-blue-300 rounded-full text-sm">
                                    <Lock className="w-4 h-4" />
                                    <span className="hidden sm:inline">Riêng tư</span>
                                </div>
                            )}

                            {accessLevel && (
                                <div className={`flex items-center gap-1 px-3 py-1.5 rounded-full text-sm ${
                                    accessLevel === 'edit' ? 'bg-orange-500/20 text-orange-300' : 'bg-gray-500/20 text-gray-300'
                                }`}>
                                    {accessLevel === 'edit' ? <Edit3 className="w-4 h-4" /> : <Eye className="w-4 h-4" />}
                                    <span>{accessLevel === 'edit' ? 'Có thể sửa' : 'Chỉ xem'}</span>
                                </div>
                            )}
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
                    <p>Được chia sẻ từ <span className="text-white font-semibold">LegacyMap</span></p>
                    <p className="mt-2">
                        {accessLevel === 'edit'
                            ? 'Bạn có quyền chỉnh sửa cây gia phả này.'
                            : tree.isPublic
                                ? 'Đây là cây gia phả công khai.'
                                : 'Cây gia phả này được chia sẻ với bạn.'}

                        {!isLoggedIn && (
                            <button
                                onClick={handleLoginRedirect}
                                className="ml-2 text-blue-400 hover:text-blue-300 underline font-semibold"
                            >
                                Đăng nhập để xem thêm
                            </button>
                        )}

                        {/* Nút bật/tắt edit mode nếu có quyền */}
                        {isLoggedIn && accessLevel === 'edit' && (
                            <button
                                onClick={handleToggleEditMode}
                                className="ml-4 px-4 py-2 bg-orange-500 hover:bg-orange-600 text-white rounded-lg transition-colors text-sm"
                            >
                                {editMode ? 'Đang chỉnh sửa' : 'Bật chế độ chỉnh sửa'}
                            </button>
                        )}
                    </p>
                </div>
            </footer>

            {/* ✅ THÊM: Auth Modals */}
            {showAuthModal === 'signin' && (
                <SignIn
                    onClose={() => setShowAuthModal(null)}
                    onShowPasswordReset={() => setShowAuthModal('reset')}
                    onShowSignUp={() => setShowAuthModal('signup')}
                />
            )}

            {showAuthModal === 'signup' && (
                <SignUp
                    onClose={() => setShowAuthModal(null)}
                    onShowSignIn={() => setShowAuthModal('signin')}
                />
            )}

            {showAuthModal === 'reset' && (
                <PasswordReset
                    onClose={() => setShowAuthModal(null)}
                    onShowSignIn={() => setShowAuthModal('signin')}
                />
            )}
        </div>
    );
}