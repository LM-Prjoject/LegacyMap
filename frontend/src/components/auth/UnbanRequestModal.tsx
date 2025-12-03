import { useState, useEffect, FormEvent } from "react";
import { X } from "lucide-react";
import { authApi } from "@/api/auth";

interface UnbanRequestModalProps {
    open: boolean;
    onClose: () => void;
    identifier: string;
}

export default function UnbanRequestModal({
                                              open,
                                              onClose,
                                              identifier,
                                          }: UnbanRequestModalProps) {
    const [reason, setReason] = useState("");
    const [loading, setLoading] = useState(false);
    const [errorMsg, setErrorMsg] = useState("");
    const [successMsg, setSuccessMsg] = useState("");
    const [accountInput, setAccountInput] = useState(identifier || "");
    const [isClosing, setIsClosing] = useState(false);

    useEffect(() => {
        if (open) {
            setAccountInput(identifier || "");
            setIsClosing(false);
        }
    }, [open, identifier]);

    if (!open) return null;

    const handleSubmit = async (e: FormEvent) => {
        e.preventDefault();
        setErrorMsg("");
        setSuccessMsg("");

        const trimmedId = accountInput.trim();
        if (!trimmedId) {
            setErrorMsg("Vui lòng nhập email hoặc username.");
            return;
        }
        if (!reason.trim()) {
            setErrorMsg("Vui lòng nhập lý do mở khóa tài khoản.");
            return;
        }

        try {
            setLoading(true);
            const res = await authApi.createUnbanRequest(trimmedId, reason.trim());

            setSuccessMsg(
                res.message ||
                "Yêu cầu mở khóa đã được ghi nhận, vui lòng chờ admin xử lý."
            );
            setReason("");
        } catch (err: any) {
            console.error(err);
            setErrorMsg(
                err?.response?.data?.message ||
                err?.message ||
                "Gửi yêu cầu mở khóa thất bại."
            );
        } finally {
            setLoading(false);
        }
    };

    const handleClose = () => {
        if (loading) return;

        setIsClosing(true);

        setTimeout(() => {
            setErrorMsg("");
            setSuccessMsg("");
            setReason("");
            onClose();
            setIsClosing(false);
        }, 280);
    };

    return (
        <div className="fixed inset-0 z-50 flex items-center justify-center p-4">
            <div
                className={
                    "relative bg-gradient-to-br from-[#1e2a3a]/95 via-[#0f1419]/90 to-[#1e2a3a]/95 " +
                    "backdrop-blur-2xl rounded-2xl shadow-[0_0_60px_rgba(255,216,155,0.15),0_20px_80px_rgba(0,0,0,0.6)] " +
                    "w-[450px] max-h-[90vh] flex flex-col border-2 border-[#ffd89b]/20 overflow-hidden " +
                    (isClosing
                            ? "animate-[zoomOut_0.28s_ease-in]"
                            : "animate-[zoomIn_0.28s_ease-out]"
                    )
                }
            >
                <div className="flex items-center justify-between px-5 py-4 border-b">
                    <h2
                        className="text-xl font-bold text-[#ffd89b]"
                        style={{
                            textShadow:
                                "0 0 15px rgba(255,216,155,0.3), 0 2px 4px rgba(0,0,0,0.5)",
                        }}
                    >
                        Yêu cầu mở khóa tài khoản
                    </h2>
                    <button type="button" onClick={handleClose}>
                        <X className="w-5 h-5 text-[#ffd89b]" />
                    </button>
                </div>

                <form onSubmit={handleSubmit} className="px-5 py-4 space-y-4">
                    <div className="space-y-1">
                        <label className="text-sm font-medium text-white">
                            Tài khoản <span className="text-[#ffd89b]">*</span>
                        </label>

                        <input
                            value={accountInput}
                            onChange={(e) => setAccountInput(e.target.value)}
                            className="w-full rounded-lg border border-white px-3 py-2 text-sm bg-white/5 text-white focus:outline-none focus:ring-2 focus:ring-[#ffd89b]/50 transition-all"
                            placeholder="email@domain.com hoặc username"
                        />
                    </div>

                    <div className="space-y-1">
                        <label className="text-sm font-medium text-white">
                            Mô tả <span className="text-[#ffd89b]">*</span>
                        </label>
                        <textarea
                            value={reason}
                            onChange={(e) => setReason(e.target.value)}
                            rows={4}
                            placeholder="Mô tả lý do bạn cho rằng tài khoản nên được mở khóa..."
                            className="w-full rounded-lg border border-white px-3 py-2 text-sm bg-white/5 text-white focus:outline-none focus:ring-2 focus:ring-[#ffd89b]/50 transition-all"
                        />
                    </div>

                    {errorMsg && (
                        <p className="text-xs text-red-600 font-medium">{errorMsg}</p>
                    )}
                    {successMsg && (
                        <p className="text-xs text-green-600 font-medium">{successMsg}</p>
                    )}

                    <div className="flex justify-end gap-2 pt-2">
                        <button
                            type="button"
                            onClick={handleClose}
                            className="px-3 py-2 text-sm bg-white/5 hover:bg-white/10 border border-[#ffd89b]/30 hover:border-[#ffd89b]/50 text-gray-300 hover:text-white rounded-lg transition-all duration-300 font-medium"
                            disabled={loading}
                        >
                            Hủy
                        </button>
                        <button
                            type="submit"
                            disabled={loading}
                            className="text-sm relative overflow-hidden px-6 py-2.5 bg-gradient-to-r from-[#d4af7a] via-[#ffd89b] to-[#d4af7a] bg-[length:200%_100%] hover:bg-[position:100%] text-[#0f1419] rounded-lg transition-all duration-500 font-semibold shadow-[0_8px_30px_rgba(255,216,155,0.3)] hover:shadow-[0_12px_40px_rgba(255,216,155,0.5)] hover:scale-105 disabled:opacity-50 disabled:cursor-not-allowed disabled:hover:scale-100 group"
                        >
                            {loading ? "Đang gửi..." : "Gửi yêu cầu"}
                        </button>
                    </div>
                </form>
            </div>
        </div>
    );
}