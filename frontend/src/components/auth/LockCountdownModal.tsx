import { X, Timer } from "lucide-react";

interface LockCountdownModalProps {
    open: boolean;
    secondsLeft: number;
    onClose: () => void;
}

export default function LockCountdownModal({
                                               open,
                                               secondsLeft,
                                               onClose,
                                           }: LockCountdownModalProps) {
    if (!open || secondsLeft <= 0) return null;

    return (
        <>
            {/* Overlay layer */}
            <div
                className="fixed top-0 left-0 right-0 bottom-0"
                style={{
                    background: "rgba(0,0,0,0.35)",
                    zIndex: 999,
                    width: '100vw',
                    height: '100vh'
                }}
                onClick={onClose}
            />

            {/* Modal content layer */}
            <div
                className="fixed top-0 left-0 right-0 bottom-0 flex items-center justify-center p-4"
                style={{
                    zIndex: 1100,
                    pointerEvents: 'none'
                }}
            >
                <div
                    className="w-full max-w-sm rounded-3xl p-6 relative"
                    style={{
                        background: "linear-gradient(135deg, #fff7e5 0%, #ffe4b8 100%)",
                        border: "2px solid rgba(255, 216, 155, 0.8)",
                        boxShadow: "0 18px 50px rgba(0,0,0,0.35)",
                        pointerEvents: 'auto'
                    }}
                    onClick={(e) => e.stopPropagation()}
                >
                    <button
                        className="absolute top-3 right-3 p-1.5 rounded-full hover:scale-110 transition-transform"
                        style={{
                            background: "rgba(42,53,72,0.08)",
                            border: "1px solid rgba(42,53,72,0.15)",
                        }}
                        onClick={onClose}
                    >
                        <X className="w-4 h-4" style={{ color: "#2a3548" }} />
                    </button>

                    <div className="flex flex-col items-center text-center gap-3">
                        <div
                            className="w-16 h-16 rounded-full flex items-center justify-center mb-1"
                            style={{
                                background: "#ffeb3b",
                                boxShadow: "0 0 0 4px rgba(255,235,59,0.5)",
                            }}
                        >
                            <Timer className="w-9 h-9" style={{ color: "#000" }} />
                        </div>

                        <p className="font-bold text-lg" style={{ color: "#2a3548" }}>
                            Tài khoản đang bị tạm khóa
                        </p>
                        <p className="text-sm font-medium" style={{ color: "#2a3548" }}>
                            Vui lòng thử lại sau{" "}
                            <span className="font-black text-base">{secondsLeft} giây</span>.
                        </p>
                        <p className="text-xs opacity-80 mt-1" style={{ color: "#2a3548" }}>
                            Bạn có thể đóng thông báo này và quay lại sau.
                        </p>
                    </div>
                </div>
            </div>
        </>
    );
}