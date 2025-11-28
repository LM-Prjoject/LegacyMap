import { Bell, X } from "lucide-react";
import type { NotificationResponse } from "@/types/notification";

export interface NotificationDetailModalProps {
    isOpen: boolean;
    notification: NotificationResponse | null;
    onClose: () => void;
    onApprove: (n: NotificationResponse) => void;
    onReject: (n: NotificationResponse) => void;
    showModerationActions: boolean;
    actionLoading?: boolean;
}

const NotificationDetailModal = ({
                                     isOpen,
                                     notification,
                                     onClose,
                                     onApprove,
                                     onReject,
                                     showModerationActions,
                                     actionLoading = false,
                                 }: NotificationDetailModalProps) => {
    if (!isOpen || !notification) return null;

    const createdAt = new Date(notification.createdAt).toLocaleString("vi-VN");

    const related = (notification.relatedEntity ?? {}) as {
        type?: string;
        userEmail?: string;
        reason?: string;
        userId?: string;
        [key: string]: any;
    };

    const isUnbanRequest = related?.type === "unban_request";

    return (
        <div
            className="fixed inset-0 z-50 flex items-center justify-center"
            style={{ background: "rgba(42, 53, 72, 0.5)", backdropFilter: "blur(8px)" }}
        >
            <div className="relative w-full max-w-lg mx-4">
                <div
                    className="rounded-3xl shadow-2xl p-6"
                    style={{
                        background:
                            "linear-gradient(135deg, rgba(255, 245, 220, 0.97) 0%, rgba(255, 235, 200, 0.95) 50%, rgba(255, 245, 220, 0.97) 100%)",
                        border: "3px solid rgba(255, 216, 155, 0.8)",
                        boxShadow: "0 20px 60px rgba(42, 53, 72, 0.3)",
                    }}
                >
                    <div className="flex items-start gap-3 mb-4">
                        <div
                            className="w-10 h-10 rounded-full flex items-center justify-center flex-shrink-0"
                            style={{
                                background:
                                    "linear-gradient(135deg, rgba(180, 158, 123, 0.15) 0%, rgba(209, 185, 138, 0.2) 100%)",
                                border: "2px solid rgba(180, 158, 123, 0.4)",
                            }}
                        >
                            <Bell className="w-5 h-5" style={{ color: "#2a3548" }} />
                        </div>
                        <div className="flex-1">
                            <h3 className="text-xl font-black mb-1" style={{ color: "#2a3548" }}>
                                {notification.title}
                            </h3>
                            <p className="text-xs" style={{ color: "#6b7280" }}>
                                Thời gian: {createdAt}
                            </p>
                        </div>
                        <button
                            onClick={onClose}
                            className="p-1 rounded-lg transition-all hover:bg-black/5"
                        >
                            <X className="w-5 h-5" style={{ color: "#2a3548" }} />
                        </button>
                    </div>

                    <div
                        className="rounded-2xl p-4 mb-3"
                        style={{
                            backgroundColor: "rgba(255,255,255,0.9)",
                            border: "1px solid rgba(209, 213, 219, 0.8)",
                        }}
                    >
                        {isUnbanRequest && (
                            <>
                                <p className="text-s font-semibold text-gray-500 mb-1">
                                    Thông tin:
                                </p>
                                <div className="text-sm space-y-2 text-gray-700">
                                    <p>
                                        <span className="font-semibold">Người yêu cầu:&nbsp;</span>
                                        {related.userEmail || "Không rõ"}
                                    </p>
                                    <p>
                                        <span className="font-semibold">Lý do:&nbsp;</span>
                                        <span className="whitespace-pre-line">
                                            {related.reason || "Không cung cấp lý do"}
                                        </span>
                                    </p>
                                </div>
                            </>
                        )}
                    </div>

                    <div className="flex items-center justify-between">
                        {showModerationActions && (
                            <div className="flex gap-3">
                                <button
                                    disabled={actionLoading}
                                    onClick={() => onReject(notification)}
                                    className={`px-4 py-2 rounded-xl text-sm font-semibold shadow-md transition-all ${
                                        actionLoading ? "opacity-50 cursor-not-allowed" : "hover:brightness-110"
                                    }`}
                                    style={{
                                        background:
                                            "linear-gradient(135deg, rgba(239, 68, 68, 0.95) 0%, rgba(220, 38, 38, 0.95) 100%)",
                                        color: "white",
                                        border: "2px solid rgba(220, 38, 38, 0.7)",
                                    }}
                                >
                                    {actionLoading ? "Đang xử lý..." : "Từ chối"}
                                </button>

                                <button
                                    disabled={actionLoading}
                                    onClick={() => onApprove(notification)}
                                    className={`px-4 py-2 rounded-xl text-sm font-semibold shadow-md transition-all ${
                                        actionLoading ? "opacity-50 cursor-not-allowed" : "hover:brightness-110"
                                    }`}
                                    style={{
                                        background:
                                            "linear-gradient(135deg, rgba(34, 197, 94, 0.95) 0%, rgba(22, 163, 74, 0.95) 100%)",
                                        color: "white",
                                        border: "2px solid rgba(22, 163, 74, 0.7)",
                                    }}
                                >
                                    {actionLoading ? "Đang xử lý..." : "Xác nhận"}
                                </button>
                            </div>
                        )}
                    </div>
                </div>
            </div>
        </div>
    );
};

export default NotificationDetailModal;