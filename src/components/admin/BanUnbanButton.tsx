// src/components/admin/BanUnbanButton.tsx
import React, { useState } from 'react';

interface BanUnbanButtonProps {
    userId: string;
    isBanned: boolean;
    onBan: (userId: string) => Promise<void>;
    onUnban: (userId: string) => Promise<void>;
}

const BanUnbanButton: React.FC<BanUnbanButtonProps> = ({
                                                           userId,
                                                           isBanned,
                                                           onBan,
                                                           onUnban
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

    return (
        <button
            onClick={handleClick}
            disabled={loading}
            className={`px-4 py-2 rounded font-medium transition-colors disabled:opacity-50 disabled:cursor-not-allowed ${
                isBanned
                    ? 'bg-green-500 text-white hover:bg-green-600'
                    : 'bg-red-500 text-white hover:bg-red-600'
            }`}
        >
            {loading ? (
                <span className="flex items-center">
                    <svg
                        className="animate-spin h-4 w-4 mr-2"
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
                    Processing...
                </span>
            ) : (
                isBanned ? '✅ Unban' : '🚫 Ban'
            )}
        </button>
    );
};

export default BanUnbanButton;