import { useState, useEffect } from "react";
import { X, Link2, Users, Copy, Check, Mail, Trash2, ExternalLink } from "lucide-react";
import api, { type TreeShareResponse, type TreeAccessResponse } from "@/api/trees";
import { showToast } from "@/lib/toast";

type Props = {
    isOpen: boolean;
    onClose: () => void;
    treeId: string;
    userId: string;
    treeName?: string;
};

export default function ShareTreeModal({ isOpen, onClose, treeId, userId, treeName }: Props) {
    const [loading, setLoading] = useState(false);
    const [publicLink, setPublicLink] = useState<TreeShareResponse | null>(null);
    const [sharedUsers, setSharedUsers] = useState<TreeAccessResponse[]>([]);
    const [copied, setCopied] = useState(false);

    const [showShareForm, setShowShareForm] = useState(false);
    const [email, setEmail] = useState("");
    const [accessLevel, setAccessLevel] = useState<"view" | "edit">("view");
    const [sharing, setSharing] = useState(false);

    // Thêm state cho permission
    const [publicPermission, setPublicPermission] = useState<"view" | "edit">("view");

    useEffect(() => {
        if (isOpen) {
            loadShareData();
        }
    }, [isOpen, treeId, userId]);

    const loadShareData = async () => {
        setLoading(true);
        try {
            const [link, users] = await Promise.all([
                api.generatePublicShareLink(userId, treeId, publicPermission),
                api.getSharedUsers(userId, treeId),
            ]);

            // ✅ THÊM: Sync publicPermission từ response
            if (link.sharePermission) {
                setPublicPermission(link.sharePermission as "view" | "edit");
            }

            setPublicLink(link);
            setSharedUsers(users);
        } catch (e: any) {
            showToast.error(e?.message || "Không tải được thông tin chia sẻ");
        } finally {
            setLoading(false);
        }
    };

    const copyLink = () => {
        if (!publicLink) return;
        navigator.clipboard.writeText(publicLink.shareUrl);
        setCopied(true);
        showToast.success("Đã sao chép link!");
        setTimeout(() => setCopied(false), 2000);
    };

    const handleShareWithUser = async () => {
        if (!email.trim() || sharing) return;

        setSharing(true);
        try {
            await api.shareWithUser(userId, treeId, {
                email: email.trim(),
                accessLevel,
            });
            showToast.success(`Đã chia sẻ với ${email}`);
            setEmail("");
            setShowShareForm(false);
            await loadShareData(); // ✅ Reload để cập nhật permission
        } catch (e: any) {
            showToast.error(e?.message || "Chia sẻ thất bại");
        } finally {
            setSharing(false);
        }
    };

    const handleRevokeAccess = async (targetUserId: string, userEmail: string) => {
        try {
            await api.revokeAccess(userId, treeId, targetUserId);
            showToast.success(`Đã thu hồi quyền của ${userEmail}`);
            await loadShareData(); // ✅ Reload để cập nhật danh sách
        } catch (e: any) {
            showToast.error(e?.message || "Thu hồi quyền thất bại");
        }
    };

    // ✅ Thêm hàm xử lý thay đổi public permission
    const handlePublicPermissionChange = async (newPermission: "view" | "edit") => {
        try {
            // Tạo lại public link với permission mới
            const newLink = await api.generatePublicShareLink(userId, treeId, newPermission);
            setPublicLink(newLink);
            setPublicPermission(newPermission); // ✅ Cập nhật state từ response
            showToast.success(`Đã cập nhật quyền truy cập công khai thành: ${newPermission === "view" ? "Chỉ xem" : "Có thể chỉnh sửa"}`);
        } catch (e: any) {
            showToast.error(e?.message || "Cập nhật quyền thất bại");
        }
    };

    if (!isOpen) return null;

    return (
        <div className="fixed inset-0 z-50 flex items-center justify-center p-4">
            {/* Backdrop with gradient */}
            <div
                className="absolute inset-0 bg-gradient-to-br from-[#0f1419]/95 via-[#1e2a3a]/95 to-[#0f1419]/95 backdrop-blur-sm"
                onClick={onClose}
            />

            {/* Ambient glow effects */}
            <div className="absolute top-1/4 left-1/4 w-96 h-96 bg-[#ffd89b]/10 rounded-full blur-[120px] pointer-events-none" />
            <div className="absolute bottom-1/4 right-1/4 w-96 h-96 bg-[#d4af7a]/10 rounded-full blur-[120px] pointer-events-none" />

            <div className="relative bg-gradient-to-br from-[#1e2a3a]/95 via-[#0f1419]/90 to-[#1e2a3a]/95 backdrop-blur-2xl rounded-2xl shadow-[0_0_60px_rgba(255,216,155,0.15),0_20px_80px_rgba(0,0,0,0.6)] w-full max-w-2xl max-h-[90vh] flex flex-col border-2 border-[#ffd89b]/20 overflow-hidden">
                {/* Decorative top border glow */}
                <div className="absolute top-0 left-0 right-0 h-[2px] bg-gradient-to-r from-transparent via-[#ffd89b] to-transparent opacity-60" />

                {/* Header */}
                <div className="relative flex items-center justify-between p-6 border-b border-[#ffd89b]/20 bg-gradient-to-r from-[#ffd89b]/5 to-transparent">
                    <div className="flex items-center gap-3">
                        <div className="relative p-2.5 bg-gradient-to-br from-[#ffd89b]/20 to-[#d4af7a]/20 rounded-xl border border-[#ffd89b]/30 shadow-[0_0_20px_rgba(255,216,155,0.2)]">
                            <Users className="w-5 h-5 text-[#ffd89b]" />
                            <div className="absolute inset-0 bg-[#ffd89b]/10 rounded-xl blur-sm" />
                        </div>
                        <div>
                            <h2 className="text-xl font-bold text-[#ffd89b]" style={{
                                textShadow: '0 0 15px rgba(255,216,155,0.3), 0 2px 4px rgba(0,0,0,0.5)'
                            }}>
                                Chia sẻ cây gia phả
                            </h2>
                            <p className="text-sm text-gray-400">{treeName || "Gia phả"}</p>
                        </div>
                    </div>
                    <button
                        onClick={onClose}
                        className="p-2 hover:bg-[#ffd89b]/10 rounded-lg transition-all duration-300 text-gray-400 hover:text-[#ffd89b] border border-transparent hover:border-[#ffd89b]/30"
                    >
                        <X className="w-5 h-5" />
                    </button>
                </div>

                {/* Content */}
                <div className="flex-1 overflow-y-auto p-6 space-y-6 custom-scrollbar">
                    {loading ? (
                        <div className="text-center py-8">
                            <div className="inline-block w-8 h-8 border-2 border-[#ffd89b]/30 border-t-[#ffd89b] rounded-full animate-spin" />
                            <p className="mt-3 text-gray-400">Đang tải...</p>
                        </div>
                    ) : (
                        <>
                            {/* Public Link Section */}
                            <section className="space-y-3">
                                <div className="flex items-center gap-2">
                                    <Link2 className="w-5 h-5 text-[#ffd89b]" />
                                    <h3 className="font-semibold text-white">Link chia sẻ công khai</h3>
                                </div>
                                <p className="text-sm text-gray-400">
                                    Bất kỳ ai có link này đều có thể xem cây gia phả
                                </p>

                                {/* Thêm dropdown chọn quyền TRƯỚC phần input link */}
                                <div className="mb-3">
                                    <label className="block text-sm font-medium text-gray-300 mb-2">
                                        Quyền truy cập link công khai
                                    </label>
                                    <div className="flex gap-2">
                                        <button
                                            type="button"
                                            onClick={() => handlePublicPermissionChange("view")}
                                            className={`flex-1 px-4 py-2.5 rounded-lg border transition-all duration-300 text-sm font-medium ${
                                                publicPermission === "view"
                                                    ? "bg-gradient-to-r from-[#d4af7a] to-[#ffd89b] text-[#0f1419] border-[#ffd89b] shadow-[0_8px_25px_rgba(255,216,155,0.3)]"
                                                    : "bg-white/5 text-gray-300 border-[#ffd89b]/30 hover:bg-white/10 hover:border-[#ffd89b]/50"
                                            }`}
                                        >
                                            👀 Chỉ xem
                                        </button>
                                        <button
                                            type="button"
                                            onClick={() => handlePublicPermissionChange("edit")}
                                            className={`flex-1 px-4 py-2.5 rounded-lg border transition-all duration-300 text-sm font-medium ${
                                                publicPermission === "edit"
                                                    ? "bg-gradient-to-r from-[#d4af7a] to-[#ffd89b] text-[#0f1419] border-[#ffd89b] shadow-[0_8px_25px_rgba(255,216,155,0.3)]"
                                                    : "bg-white/5 text-gray-300 border-[#ffd89b]/30 hover:bg-white/10 hover:border-[#ffd89b]/50"
                                            }`}
                                        >
                                            ✏️ Có thể chỉnh sửa
                                        </button>
                                    </div>
                                    <p className="text-xs text-gray-500 mt-2">
                                        {publicPermission === "view"
                                            ? "Người nhận chỉ có thể xem, không thể chỉnh sửa"
                                            : "Người nhận cần đăng nhập để chỉnh sửa cây gia phả"}
                                    </p>
                                </div>

                                <div className="flex gap-2">
                                    <div className="flex-1 flex items-center gap-2 px-4 py-3 bg-gradient-to-r from-white/5 to-white/10 border border-[#ffd89b]/20 rounded-lg backdrop-blur-sm">
                                        <ExternalLink className="w-4 h-4 text-[#ffd89b]/60 flex-shrink-0" />
                                        <input
                                            type="text"
                                            readOnly
                                            value={publicLink?.shareUrl || ""}
                                            className="flex-1 bg-transparent text-sm text-gray-300 outline-none"
                                        />
                                    </div>
                                    <button
                                        onClick={copyLink}
                                        className="relative overflow-hidden px-4 py-3 bg-gradient-to-r from-[#d4af7a] via-[#ffd89b] to-[#d4af7a] bg-[length:200%_100%] hover:bg-[position:100%] text-[#0f1419] rounded-lg transition-all duration-500 flex items-center gap-2 font-semibold shadow-[0_8px_30px_rgba(255,216,155,0.3)] hover:shadow-[0_12px_40px_rgba(255,216,155,0.5)] hover:scale-105 group border border-[#ffd89b]/30"
                                    >
                                        {copied ? <Check className="w-4 h-4" /> : <Copy className="w-4 h-4" />}
                                        {copied ? "Đã sao chép" : "Sao chép"}
                                        <div className="absolute inset-0 -translate-x-full group-hover:translate-x-full transition-transform duration-1000 bg-gradient-to-r from-transparent via-white/30 to-transparent skew-x-12" />
                                    </button>
                                </div>
                            </section>

                            <hr className="border-[#ffd89b]/20" />

                            {/* Share with specific users */}
                            <section className="space-y-3">
                                <div className="flex items-center justify-between">
                                    <div className="flex items-center gap-2">
                                        <Mail className="w-5 h-5 text-[#ffd89b]" />
                                        <h3 className="font-semibold text-white">
                                            Người được chia sẻ ({sharedUsers.length})
                                        </h3>
                                    </div>
                                    {!showShareForm && (
                                        <button
                                            onClick={() => setShowShareForm(true)}
                                            className="px-3 py-1.5 text-sm bg-[#ffd89b]/10 hover:bg-[#ffd89b]/20 text-[#ffd89b] rounded-lg transition-all duration-300 border border-[#ffd89b]/30 hover:border-[#ffd89b]/50 hover:shadow-[0_4px_20px_rgba(255,216,155,0.2)]"
                                        >
                                            + Chia sẻ với người dùng
                                        </button>
                                    )}
                                </div>

                                {/* Share Form */}
                                {showShareForm && (
                                    <div className="p-4 bg-gradient-to-br from-white/5 to-white/10 rounded-lg space-y-4 border border-[#ffd89b]/20 backdrop-blur-sm">
                                        <div>
                                            <label className="block text-sm font-medium text-gray-300 mb-2">
                                                Email người nhận
                                            </label>
                                            <input
                                                type="email"
                                                value={email}
                                                onChange={(e) => setEmail(e.target.value)}
                                                placeholder="example@gmail.com"
                                                className="w-full px-3 py-2 bg-white/5 border border-[#ffd89b]/30 rounded-lg focus:ring-2 focus:ring-[#ffd89b]/50 focus:border-[#ffd89b] transition-all text-white placeholder-gray-500 outline-none"
                                                onKeyPress={(e) => {
                                                    if (e.key === 'Enter') {
                                                        e.preventDefault();
                                                        handleShareWithUser();
                                                    }
                                                }}
                                            />
                                        </div>
                                        <div>
                                            <label className="block text-sm font-medium text-gray-300 mb-2">
                                                Quyền truy cập
                                            </label>
                                            <div className="flex gap-2">
                                                <button
                                                    type="button"
                                                    onClick={() => setAccessLevel("view")}
                                                    className={`flex-1 px-4 py-3 rounded-lg border transition-all duration-300 text-sm font-medium ${
                                                        accessLevel === "view"
                                                            ? "bg-gradient-to-r from-[#d4af7a] to-[#ffd89b] text-[#0f1419] border-[#ffd89b] shadow-[0_8px_25px_rgba(255,216,155,0.3)]"
                                                            : "bg-white/5 text-gray-300 border-[#ffd89b]/30 hover:bg-white/10 hover:border-[#ffd89b]/50"
                                                    }`}
                                                >
                                                    👀 Chỉ xem
                                                </button>
                                                <button
                                                    type="button"
                                                    onClick={() => setAccessLevel("edit")}
                                                    className={`flex-1 px-4 py-3 rounded-lg border transition-all duration-300 text-sm font-medium ${
                                                        accessLevel === "edit"
                                                            ? "bg-gradient-to-r from-[#d4af7a] to-[#ffd89b] text-[#0f1419] border-[#ffd89b] shadow-[0_8px_25px_rgba(255,216,155,0.3)]"
                                                            : "bg-white/5 text-gray-300 border-[#ffd89b]/30 hover:bg-white/10 hover:border-[#ffd89b]/50"
                                                    }`}
                                                >
                                                    ✏️ Có thể chỉnh sửa
                                                </button>
                                            </div>
                                            <p className="text-xs text-gray-500 mt-2">
                                                {accessLevel === "view"
                                                    ? "Người nhận chỉ có thể xem, không thể chỉnh sửa"
                                                    : "Người nhận cần đăng nhập để chỉnh sửa cây gia phả"}
                                            </p>
                                        </div>
                                        <div className="flex gap-2">
                                            <button
                                                type="button"
                                                onClick={handleShareWithUser}
                                                disabled={sharing}
                                                className="flex-1 px-4 py-2 bg-gradient-to-r from-[#d4af7a] to-[#ffd89b] hover:from-[#ffd89b] hover:to-[#d4af7a] disabled:from-gray-600 disabled:to-gray-700 text-[#0f1419] font-semibold rounded-lg transition-all duration-300 shadow-[0_8px_25px_rgba(255,216,155,0.3)] hover:shadow-[0_12px_35px_rgba(255,216,155,0.5)] hover:scale-105 disabled:hover:scale-100"
                                            >
                                                {sharing ? "Đang chia sẻ..." : "Chia sẻ"}
                                            </button>
                                            <button
                                                type="button"
                                                onClick={() => {
                                                    setShowShareForm(false);
                                                    setEmail("");
                                                }}
                                                className="px-4 py-2 bg-white/5 hover:bg-white/10 text-gray-300 border border-[#ffd89b]/30 rounded-lg transition-all duration-300"
                                            >
                                                Hủy
                                            </button>
                                        </div>
                                    </div>
                                )}

                                {/* Shared Users List */}
                                <div className="space-y-2">
                                    {sharedUsers.length === 0 ? (
                                        <p className="text-sm text-gray-500 text-center py-4">
                                            Chưa chia sẻ với ai
                                        </p>
                                    ) : (
                                        sharedUsers.map((user) => (
                                            <div
                                                key={user.userId}
                                                className="flex items-center justify-between p-3 bg-gradient-to-r from-white/5 to-white/10 rounded-lg hover:from-white/10 hover:to-white/15 transition-all duration-300 border border-[#ffd89b]/10 hover:border-[#ffd89b]/30"
                                            >
                                                <div className="flex-1">
                                                    <div className="font-medium text-white">{user.userName}</div>
                                                    <div className="text-sm text-gray-400">{user.userEmail}</div>
                                                </div>
                                                <div className="flex items-center gap-3">
                                                    <span className={`px-2 py-1 text-xs rounded-full font-medium ${
                                                        user.accessLevel === "edit"
                                                            ? "bg-[#ffd89b]/20 text-[#ffd89b] border border-[#ffd89b]/30"
                                                            : "bg-blue-500/20 text-blue-300 border border-blue-500/30"
                                                    }`}>
                                                        {user.accessLevel === "edit" ? "Chỉnh sửa" : "Chỉ xem"}
                                                    </span>
                                                    <button
                                                        onClick={() => handleRevokeAccess(user.userId, user.userEmail)}
                                                        className="p-1.5 hover:bg-red-500/20 text-red-400 hover:text-red-300 rounded transition-all duration-300 border border-transparent hover:border-red-500/30"
                                                        title="Thu hồi quyền"
                                                    >
                                                        <Trash2 className="w-4 h-4" />
                                                    </button>
                                                </div>
                                            </div>
                                        ))
                                    )}
                                </div>
                            </section>
                        </>
                    )}
                </div>

                {/* Footer */}
                <div className="p-6 border-t border-[#ffd89b]/20 bg-gradient-to-r from-[#ffd89b]/5 to-transparent">
                    <button
                        onClick={onClose}
                        className="w-full px-4 py-2 bg-white/5 hover:bg-white/10 border border-[#ffd89b]/30 hover:border-[#ffd89b]/50 text-gray-300 hover:text-white rounded-lg transition-all duration-300 font-medium hover:shadow-[0_8px_25px_rgba(255,216,155,0.15)]"
                    >
                        Đóng
                    </button>
                </div>

                {/* Decorative bottom border glow */}
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
}