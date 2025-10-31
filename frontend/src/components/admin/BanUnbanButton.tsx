import React, { useState } from 'react';

interface BanUnbanButtonProps {
    userId: string;
    isBanned: boolean;
    role?: string; // 🔥 THÊM PROP ROLE
    onBan: (userId: string) => Promise<void>;
    onUnban: (userId: string) => Promise<void>;
}

const BanUnbanButton: React.FC<BanUnbanButtonProps> = ({
                                                           userId,
                                                           isBanned,
                                                           role, // 🔥 NHẬN PROP ROLE
                                                           onBan,
                                                           onUnban,
                                                       }) => {
    const [loading, setLoading] = useState(false);

    const handleClick = async () => {
        setLoading(true);
        try {
            if (isBanned) {
                await onUnban(userId);
            } else {
                await onBan(userId);
            }
        } catch (error) {
            console.error('Error toggling ban status:', error);
        } finally {
            setLoading(false);
        }
    };

    // 🔥 KIỂM TRA NẾU USER LÀ ADMIN THÌ ẨN NÚT BAN
    const isAdmin = role === 'ADMIN' || role === 'admin';

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
                    <span>👑</span>
                    <span>Admin</span>
                </span>
            </button>
        );
    }

    return (
        <button
            onClick={handleClick}
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
                            <span>✅</span>
                            <span>Mở khóa</span>
                        </>
                    ) : (
                        <>
                            <span>🚫</span>
                            <span>Khóa</span>
                        </>
                    )}
                </span>
            )}
        </button>
    );
};

export default BanUnbanButton;