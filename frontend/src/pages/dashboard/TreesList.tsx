import { useEffect, useState, useCallback, useMemo } from "react";
import Navbar from "@/components/layout/Navbar";
import FamilyTreeModal from "@/components/familyTree/familyTreeModal/FamilyTreeModal";
import { uploadCoverToSupabase } from "@/lib/upload";
import api, { FamilyTree } from "@/api/trees";
import { Loader, Pencil, Trash2, Eye } from "lucide-react";
import bg from "@/assets/bg.jpg";
import PopupModal from "@/components/popupModal/PopupModal";
import { useNavigate } from "react-router-dom";

export default function TreesList() {
    const [showModal, setShowModal] = useState(false);
    const [trees, setTrees] = useState<FamilyTree[]>([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState("");
    const [editingTree, setEditingTree] = useState<FamilyTree | null>(null);
    const [deleteTarget, setDeleteTarget] = useState<FamilyTree | null>(null);
    const [deleting, setDeleting] = useState(false);

    const navigate = useNavigate();
    const user = useMemo(() => JSON.parse(localStorage.getItem("user") || "{}"), []);
    const userId: string | undefined = user?.id;

    const load = useCallback(async () => {
        if (!userId) return;
        setLoading(true);
        setError("");
        try {
            const data = await api.listTrees(userId);
            setTrees(data);
        } catch (e: any) {
            setError(e?.message || "Không tải được danh sách");
        } finally {
            setLoading(false);
        }
    }, [userId]);

    useEffect(() => {
        load();
    }, [load]);

    const goToDetail = (id: string) => navigate(`/trees/${id}`);

    const startEdit = (tree: FamilyTree) => setEditingTree(tree);

    const confirmDelete = async () => {
        if (!deleteTarget || !userId) return;
        setDeleting(true);
        const prev = trees;
        setTrees(cur => cur.filter(x => x.id !== deleteTarget.id));
        try {
            await api.deleteTree(userId, deleteTarget.id);
        } catch (e: any) {
            setTrees(prev);
            setError(e?.message || "Xóa thất bại");
        } finally {
            setDeleting(false);
            setDeleteTarget(null);
        }
    };

    return (
        <div className="relative min-h-screen flex flex-col overflow-hidden">
            <img src={bg} alt="Background" className="absolute top-0 left-0 w-full h-full object-cover -z-10" />
            <div className="absolute inset-0 bg-slate-900/30 -z-10" />
            <Navbar />
            <main className="flex-1 w-full px-8 lg:px-20 py-6">
                <div className="flex items-center justify-between mb-4">
                    <h2 className="text-ivory text-xl font-semibold">Cây của tôi</h2>
                    <button
                        onClick={() => setShowModal(true)}
                        className="px-4 py-2 rounded-lg font-semibold text-[#20283d] bg-gradient-to-br from-[#b49e7b] to-[#d1b98a] shadow-[0_6px_20px_rgba(209,185,138,0.35)] hover:brightness-110 transition-all h-11 px-5 text-[15px]"
                    >
                        Tạo gia phả
                    </button>
                </div>
                <div className="rounded-xl p-2 sm:p-4">
                    {loading ? (
                        <div className="flex items-center gap-2 text-slate-600">
                            <Loader className="animate-spin" size={18} /> Đang tải...
                        </div>
                    ) : error ? (
                        <div className="text-red-600">{error}</div>
                    ) : trees.length === 0 ? (
                        <div className="text-slate-500">Chưa có gia phả nào. Hãy tạo mới.</div>
                    ) : (
                        <ul className="grid sm:grid-cols-2 lg:grid-cols-3 gap-4">
                            {trees.map(t => (
                                <li
                                    key={t.id}
                                    className="rounded-xl p-4 shadow-sm border border-white/10 bg-white/10 backdrop-blur-sm hover:bg-white/50 hover:backdrop-blur-lg hover:shadow-[0_0_20px_rgba(255,255,255,0.4)] hover:border-white/40 hover:scale-[1.02]"
                                    style={{ position: 'relative', zIndex: 1 }}
                                >
                                    <button className="block w-full text-left" onClick={() => goToDetail(t.id)}>
                                        {t.coverImageUrl ? (
                                            <img src={t.coverImageUrl} alt={t.name} className="w-full h-32 object-cover rounded-lg mb-3 border" />
                                        ) : (
                                            <div className="w-full h-32 bg-slate-100 rounded-lg mb-3 grid place-items-center text-slate-400 text-sm">
                                                Không có ảnh bìa
                                            </div>
                                        )}
                                    </button>
                                    <div className="flex items-start justify-between gap-3">
                                        <div className="min-w-0">
                                            <h4 className="font-semibold text-slate-50 drop-shadow-sm">{t.name}</h4>
                                            <p className="text-sm text-slate-100/80 line-clamp-2">{t.description || "—"}</p>
                                            <p className="mt-1 text-xs text-slate-200/70">{t.isPublic ? "Công khai" : "Riêng tư"}</p>
                                        </div>
                                        <div className="flex items-center gap-1 sm:gap-2">
                                            <button className="p-2 rounded-lg hover:bg-slate-100" onClick={() => goToDetail(t.id)}>
                                                <Eye size={16} />
                                            </button>
                                            <button className="p-2 rounded-lg hover:bg-slate-100" onClick={() => startEdit(t)}>
                                                <Pencil size={16} />
                                            </button>
                                            <button className="p-2 rounded-lg hover:bg-red-50 text-red-500" onClick={() => setDeleteTarget(t)}>
                                                <Trash2 size={16} />
                                            </button>
                                        </div>
                                    </div>
                                </li>
                            ))}
                        </ul>
                    )}
                </div>
                {showModal && (
                    <FamilyTreeModal
                        userId={userId!}
                        onClose={() => setShowModal(false)}
                        uploadImage={uploadCoverToSupabase}
                        onCreated={tree => {
                            if (tree?.id) setTrees(prev => [tree, ...prev]);
                            else load();
                            setShowModal(false);
                        }}

                    />
                )}
                {editingTree && (
                    <FamilyTreeModal
                        userId={userId!}
                        isEdit
                        initialData={editingTree}
                        uploadImage={uploadCoverToSupabase}
                        onUpdated={updated => {
                            setTrees(prev => prev.map(t => (t.id === updated.id ? updated : t)));
                            setEditingTree(null);
                        }}
                        onClose={() => setEditingTree(null)}
                    />
                )}
                <PopupModal
                    show={!!deleteTarget}
                    onClose={() => (!deleting ? setDeleteTarget(null) : undefined)}
                    onConfirm={!deleting ? confirmDelete : undefined}
                    title="Xóa gia phả?"
                    body={<>Bạn có chắc muốn xóa <strong>{deleteTarget?.name}</strong>?</>}
                    confirmText="Xóa"
                    cancelText="Hủy"
                    variant="danger"
                    loading={deleting}
                />
            </main>
        </div>
    );
}