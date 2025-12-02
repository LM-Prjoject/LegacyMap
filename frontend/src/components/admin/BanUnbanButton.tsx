import React, { useState } from 'react';
import PopupModal from "@/components/popupModal/PopupModal";
import { LockIcon, LockOpen, Crown } from "lucide-react";

interface BanUnbanButtonProps {
    userId: string;
    isBanned: boolean;
    role?: string;
    onBan: (userId: string) => Promise<void>;
    onUnban: (userId: string) => Promise<void>;
    displayName?: string;
}

const BanUnbanButton: React.FC<BanUnbanButtonProps> = ({
                                                           userId,
                                                           isBanned,
                                                           role,
                                                           onBan,
                                                           onUnban,
                                                           displayName,
                                                       }) => {
    const [loading, setLoading] = useState(false);
    const [showConfirm, setShowConfirm] = useState(false);

    const isAdmin = role === 'ADMIN' || role === 'admin';
    const name = displayName || userId;

    if (isAdmin && !isBanned) {
        return (
            <button
                disabled
                className="
                    w-full px-4 py-2 rounded-lg font-semibold text-sm flex items-center justify-center gap-2
                    bg-gray-600/30 text-gray-400 cursor-not-allowed
                    border border-gray-500/30
                "
            >
                <span className="flex items-center gap-2">
                    <Crown size={16} />
                    <span>Admin</span>
                </span>
            </button>
        );
    }

    const handleOpenConfirm = () => {
        if (loading) return;
        setShowConfirm(true);
    };

    const handleCloseConfirm = () => {
        if (loading) return;
        setShowConfirm(false);
    };

    const handleConfirm = async () => {
        setLoading(true);
        try {
            if (isBanned) {
                await onUnban(userId);
            } else {
                await onBan(userId);
            }
            setShowConfirm(false);
        } catch (error) {
            console.error('Error toggling ban status:', error);
        } finally {
            setLoading(false);
        }
    };

    return (
        <>
            <button
                onClick={handleOpenConfirm}
                disabled={loading}
                className={`
                    w-full px-4 py-2 rounded-lg font-semibold text-sm flex items-center justify-center gap-2
                    transition-all duration-300 disabled:opacity-60 disabled:cursor-not-allowed
                    shadow-md shadow-black/30 hover:shadow-[#d1b98a]/30
                    ${
                    isBanned
                        ? 'bg-gradient-to-br from-[#d1b98a] to-[#f4e9c8] text-[#20283d] hover:from-[#f4e9c8] hover:to-[#ffffff]'
                        : 'bg-gradient-to-br from-[#7f1d1d] to-[#b91c1c] text-[#f4e9c8] hover:from-[#b91c1c] hover:to-[#ef4444]'
                }
                `}
            >
                {loading ? (
                    <span className="flex items-center justify-center gap-2">
                        <svg
                            className="animate-spin h-4 w-4 text-current"
                            viewBox="0 0 24 24"
                        >
                            <circle
                                className="opacity-25"
                                cx="12"
                                cy="12"
                                r="10"
                                stroke="currentColor"
                                strokeWidth="4"
                                fill="none"
                            />
                            <path
                                className="opacity-75"
                                fill="currentColor"
                                d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
                            />
                        </svg>
                        Đang xử lý...
                    </span>
                ) : (
                    <span className="flex items-center gap-2">
                        {isBanned ? (
                            <>
                                <LockOpen />
                                <span>Mở khóa</span>
                            </>
                        ) : (
                            <>
                                <LockIcon size="20" />
                                <span>Khóa</span>
                            </>
                        )}
                    </span>
                )}
            </button>

            <PopupModal
                show={showConfirm}
                onClose={handleCloseConfirm}
                onConfirm={handleConfirm}
                title={isBanned ? "Mở khóa tài khoản" : "Khóa tài khoản"}
                body={
                    <>
                        Bạn có chắc muốn {isBanned ? "mở khóa" : "khóa"} tài khoản{" "}
                        <strong>{name}</strong> không?
                    </>
                }
                confirmText="Có"
                cancelText="Không"
                variant="danger"
                loading={loading}
                disableCloseWhileLoading={true}
            />
        </>
    );
};

export default BanUnbanButton;