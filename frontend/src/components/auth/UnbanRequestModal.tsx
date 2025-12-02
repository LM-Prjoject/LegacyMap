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

    useEffect(() => {
        if (open) {
            setAccountInput(identifier || "");
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
        setErrorMsg("");
        setSuccessMsg("");
        setReason("");
        onClose();
    };

    return (
        <div className="fixed inset-0 z-[60] flex items-center justify-center bg-black/40">
            <div className="bg-white rounded-2xl shadow-2xl w-full max-w-md mx-4 relative">
                <div className="flex items-center justify-between px-5 py-4 border-b">
                    <h2 className="text-lg font-semibold text-gray-900">
                        Yêu cầu mở khóa tài khoản
                    </h2>
                    <button
                        type="button"
                        onClick={handleClose}
                        className="p-1 rounded-full hover:bg-gray-100"
                    >
                        <X className="w-5 h-5 text-gray-500" />
                    </button>
                </div>

                <form onSubmit={handleSubmit} className="px-5 py-4 space-y-4">
                    <div className="space-y-1">
                        <label className="text-sm font-medium text-gray-700">
                            Tài khoản <span className="text-red-500">*</span>
                        </label>

                        <input
                            value={accountInput}
                            onChange={(e) => setAccountInput(e.target.value)}
                            className="w-full rounded-lg border border-gray-300 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-500 focus:border-amber-500"
                            placeholder="email@domain.com hoặc username"
                        />
                    </div>

                    <div className="space-y-1">
                        <label className="text-sm font-medium text-gray-700">
                            Mô tả <span className="text-red-500">*</span>
                        </label>
                        <textarea
                            value={reason}
                            onChange={(e) => setReason(e.target.value)}
                            rows={4}
                            placeholder="Mô tả lý do bạn cho rằng tài khoản nên được mở khóa..."
                            className="w-full rounded-lg border border-gray-300 px-3 py-2 text-sm resize-none focus:outline-none focus:ring-2 focus:ring-amber-500 focus:border-amber-500"
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
                            className="px-3 py-2 text-sm rounded-lg border border-gray-300 text-gray-700 hover:bg-gray-50"
                            disabled={loading}
                        >
                            Hủy
                        </button>
                        <button
                            type="submit"
                            disabled={loading}
                            className="px-4 py-2 text-sm rounded-lg bg-[#DEC593] text-white font-medium hover:bg-amber-400 disabled:opacity-60 disabled:cursor-not-allowed"
                        >
                            {loading ? "Đang gửi..." : "Gửi yêu cầu"}
                        </button>
                    </div>
                </form>
            </div>
        </div>
    );
}
