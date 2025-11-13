// password-reset.tsx - Đã cập nhật UI với background trong suốt
import { useEffect, useState } from 'react';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { z } from 'zod';
import { X } from 'lucide-react';
import DragonsBackground from '@/components/visual/DragonsBackground';

const forgotPasswordSchema = z.object({
    email: z.string().email('Email không hợp lệ'),
});

const resetPasswordSchema = z
    .object({
        password: z.string().min(8, 'Mật khẩu phải có ít nhất 8 ký tự'),
        confirmPassword: z.string(),
    })
    .refine((data) => data.password === data.confirmPassword, {
        message: 'Mật khẩu xác nhận không khớp',
        path: ['confirmPassword'],
    });

type ForgotPasswordFormData = z.infer<typeof forgotPasswordSchema>;
type ResetPasswordFormData = z.infer<typeof resetPasswordSchema>;

interface PasswordResetProps {
    onClose: () => void;
    onShowSignIn: () => void;
    token?: string;
}

const PasswordReset = ({ onClose, onShowSignIn, token: initialToken }: PasswordResetProps) => {
    const [isEmailSent, setIsEmailSent] = useState<boolean>(false);
    const [isResetSuccess, setIsResetSuccess] = useState<boolean>(false);
    const [isRecovery, setIsRecovery] = useState<boolean>(false);

    useEffect(() => {
        const urlParams = new URLSearchParams(window.location.search);
        const urlToken = urlParams.get('token');
        const effectiveToken = initialToken ?? urlToken;
        setIsRecovery(!!effectiveToken);
    }, []);

    useEffect(() => {
        if (isResetSuccess) {
            const t = setTimeout(() => onShowSignIn(), 1200);
            return () => clearTimeout(t);
        }
    }, [isResetSuccess, onShowSignIn]);

    // FIXED: Quản lý scroll giống như SignUp
    useEffect(() => {
        const originalStyle = window.getComputedStyle(document.body).overflow;
        document.body.style.overflow = 'hidden';

        return () => {
            document.body.style.overflow = originalStyle;
        };
    }, []);

    const {
        register: registerForgot,
        handleSubmit: handleForgotSubmit,
        formState: { errors: forgotErrors, isSubmitting: isSending },
    } = useForm<ForgotPasswordFormData>({ resolver: zodResolver(forgotPasswordSchema) });

    const onForgotPassword = async (data: ForgotPasswordFormData) => {
        try {
            const API_BASE = (import.meta.env.VITE_API_BASE_URL as string) || 'http://localhost:8080/legacy/api';
            const params = new URLSearchParams();
            params.set('redirect', `${window.location.origin}`);

            const res = await fetch(`${API_BASE}/auth/password/forgot?${params.toString()}`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ email: data.email }),
            });
            const payload = await res.json().catch(() => ({} as any));
            if (res.ok && payload?.success) {
                setIsEmailSent(true);
            } else {
                const msg = payload?.message || 'Gửi email thất bại. Vui lòng thử lại.';
                alert(msg);
            }
        } catch (error) {
            console.error('Error sending reset email:', error);
            alert('Gửi email thất bại. Vui lòng thử lại.');
        }
    };

    const {
        register: registerReset,
        handleSubmit: handleResetSubmit,
        formState: { errors: resetErrors, isSubmitting: isResetting },
    } = useForm<ResetPasswordFormData>({ resolver: zodResolver(resetPasswordSchema) });

    const onResetPassword = async (data: ResetPasswordFormData) => {
        try {
            const API_BASE = (import.meta.env.VITE_API_BASE_URL as string) || 'http://localhost:8080/legacy/api';
            const params = new URLSearchParams(window.location.search);
            const urlToken = params.get('token');
            const token = initialToken ?? urlToken;
            if (!token) {
                alert('Thiếu token đặt lại trong liên kết.');
                return;
            }

            const res = await fetch(`${API_BASE}/auth/password/reset`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ token, newPassword: data.password }),
            });
            const payload = await res.json().catch(() => ({} as any));
            if (res.ok && payload?.success) {
                setIsResetSuccess(true);
            } else {
                const msg = payload?.message || 'Đặt lại mật khẩu thất bại. Vui lòng thử lại.';
                alert(msg);
            }
        } catch (error) {
            console.error('Error resetting password:', error);
            alert('Đặt lại mật khẩu thất bại. Vui lòng thử lại.');
        }
    };

    if (isRecovery) {
        return (
            // FIXED: Background trong suốt giống SignUp
            <div className="fixed inset-0 z-50 overflow-hidden" style={{background: 'rgba(42, 53, 72, 0.25)', backdropFilter: 'blur(8px)'}}>
                <div className="flex min-h-full items-center justify-center p-4">
                    <DragonsBackground
                        size={360}
                        showGrid
                        left={{ enabled: true, flipX: true, delayMs: 0 }}
                        right={{ enabled: true, flipX: false, delayMs: 250 }}
                    />

                    {/* FIXED: Container scroll duy nhất giống SignUp */}
                    <div className="relative w-full max-w-md max-h-[90vh] z-10 mx-auto overflow-y-auto">
                        <div className="relative rounded-3xl shadow-2xl p-8" style={{
                            background: 'linear-gradient(135deg, rgba(255, 245, 220, 0.95) 0%, rgba(255, 235, 200, 0.9) 25%, rgba(255, 245, 220, 0.95) 50%, rgba(255, 235, 200, 0.9) 75%, rgba(255, 245, 220, 0.95) 100%)',
                            border: '3px solid rgba(255, 216, 155, 0.6)',
                            boxShadow: '0 20px 60px rgba(42, 53, 72, 0.3), inset 0 0 100px rgba(255, 255, 255, 0.5)'
                        }}>
                            <div className="flex justify-between items-center mb-6">
                                <h2 className="text-3xl font-black tracking-tight" style={{
                                    color: '#2a3548',
                                    textShadow: '0 3px 15px rgba(42, 53, 72, 0.3)'
                                }}>ĐẶT LẠI MẬT KHẨU</h2>
                                <button
                                    onClick={onClose}
                                    className="p-2 rounded-xl transition-all hover:scale-110"
                                    style={{
                                        background: 'linear-gradient(135deg, #2a3548 0%, #3d4a5f 100%)',
                                        border: '2px solid rgba(255, 216, 155, 0.5)'
                                    }}
                                >
                                    <X className="h-5 w-5" style={{color: 'rgb(255, 216, 155)'}} />
                                </button>
                            </div>

                            {isResetSuccess ? (
                                <div className="text-center py-6">
                                    <div className="font-black text-lg mb-4" style={{color: '#2a3548'}}>ĐẶT LẠI MẬT KHẨU THÀNH CÔNG!</div>
                                    <button
                                        type="button"
                                        onClick={onShowSignIn}
                                        className="rounded-xl py-3 px-6 font-semibold transition-all shadow-lg hover:shadow-xl"
                                        style={{
                                            background: 'linear-gradient(135deg, #2a3548 0%, #3d4a5f 100%)',
                                            color: 'rgb(255, 216, 155)',
                                            border: '2px solid rgba(255, 216, 155, 0.5)'
                                        }}
                                    >
                                        QUAY LẠI ĐĂNG NHẬP
                                    </button>
                                </div>
                            ) : (
                                <form className="mt-6 space-y-5" onSubmit={handleResetSubmit(onResetPassword)}>
                                    <div>
                                        <label className="block text-sm font-semibold mb-2" style={{color: '#2a3548'}}>MẬT KHẨU MỚI</label>
                                        <input
                                            {...registerReset('password')}
                                            type="password"
                                            className="w-full rounded-xl border-2 px-4 py-3 outline-none focus:ring-2 focus:ring-[#2a3548] font-medium"
                                            style={{
                                                background: 'linear-gradient(135deg, rgba(255, 255, 255, 0.8) 0%, rgba(255, 255, 255, 0.6) 100%)',
                                                borderColor: 'rgba(42, 53, 72, 0.3)',
                                                color: '#2a3548'
                                            }}
                                            placeholder="Nhập mật khẩu mới"
                                        />
                                        {resetErrors.password && (
                                            <p className="text-red-600 text-sm mt-2 font-medium" style={{color: '#2a3548'}}>{resetErrors.password.message}</p>
                                        )}
                                    </div>

                                    <div>
                                        <label className="block text-sm font-semibold mb-2" style={{color: '#2a3548'}}>XÁC NHẬN MẬT KHẨU</label>
                                        <input
                                            {...registerReset('confirmPassword')}
                                            type="password"
                                            className="w-full rounded-xl border-2 px-4 py-3 outline-none focus:ring-2 focus:ring-[#2a3548] font-medium"
                                            style={{
                                                background: 'linear-gradient(135deg, rgba(255, 255, 255, 0.8) 0%, rgba(255, 255, 255, 0.6) 100%)',
                                                borderColor: 'rgba(42, 53, 72, 0.3)',
                                                color: '#2a3548'
                                            }}
                                            placeholder="Nhập lại mật khẩu"
                                        />
                                        {resetErrors.confirmPassword && (
                                            <p className="text-red-600 text-sm mt-2 font-medium" style={{color: '#2a3548'}}>{resetErrors.confirmPassword.message}</p>
                                        )}
                                    </div>

                                    <div>
                                        <button
                                            type="submit"
                                            disabled={isResetting}
                                            className="w-full rounded-xl py-3 font-semibold transition-all shadow-lg hover:shadow-xl disabled:opacity-60"
                                            style={{
                                                background: 'linear-gradient(135deg, #2a3548 0%, #3d4a5f 100%)',
                                                color: 'rgb(255, 216, 155)',
                                                border: '2px solid rgba(255, 216, 155, 0.5)'
                                            }}
                                        >
                                            {isResetting ? 'ĐANG ĐẶT LẠI…' : 'ĐẶT LẠI MẬT KHẨU'}
                                        </button>
                                    </div>
                                </form>
                            )}
                        </div>
                    </div>
                </div>
            </div>
        );
    }

    // Form nhập email để nhận link đặt lại
    return (
        // FIXED: Background trong suốt giống SignUp
        <div className="fixed inset-0 z-50 overflow-hidden" style={{background: 'rgba(42, 53, 72, 0.25)', backdropFilter: 'blur(8px)'}}>
            <div className="flex min-h-full items-center justify-center p-4">
                <DragonsBackground
                    size={360}
                    showGrid
                    left={{ enabled: true, flipX: true, delayMs: 0 }}
                    right={{ enabled: true, flipX: false, delayMs: 250 }}
                />

                {/* FIXED: Container scroll duy nhất giống SignUp */}
                <div className="relative w-full max-w-md max-h-[90vh] z-10 mx-auto overflow-y-auto">
                    <div className="relative rounded-3xl shadow-2xl p-8" style={{
                        background: 'linear-gradient(135deg, rgba(255, 245, 220, 0.95) 0%, rgba(255, 235, 200, 0.9) 25%, rgba(255, 245, 220, 0.95) 50%, rgba(255, 235, 200, 0.9) 75%, rgba(255, 245, 220, 0.95) 100%)',
                        border: '3px solid rgba(255, 216, 155, 0.6)',
                        boxShadow: '0 20px 60px rgba(42, 53, 72, 0.3), inset 0 0 100px rgba(255, 255, 255, 0.5)'
                    }}>
                        <div className="flex justify-between items-center mb-6">
                            <h2 className="text-3xl font-black tracking-tight" style={{
                                color: '#2a3548',
                                textShadow: '0 3px 15px rgba(42, 53, 72, 0.3)'
                            }}>QUÊN MẬT KHẨU</h2>
                            <button
                                onClick={onClose}
                                className="p-2 rounded-xl transition-all hover:scale-110"
                                style={{
                                    background: 'linear-gradient(135deg, #2a3548 0%, #3d4a5f 100%)',
                                    border: '2px solid rgba(255, 216, 155, 0.5)'
                                }}
                            >
                                <X className="h-5 w-5" style={{color: 'rgb(255, 216, 155)'}} />
                            </button>
                        </div>

                        <p className="text-sm font-semibold mb-6" style={{color: '#2a3548'}}>Nhập email để nhận liên kết đặt lại mật khẩu</p>

                        {isEmailSent ? (
                            <div className="text-center py-6">
                                <div className="font-black text-lg mb-4" style={{color: '#2a3548'}}>ĐÃ GỬI EMAIL ĐẶT LẠI MẬT KHẨU!</div>
                                <p className="text-sm font-medium mb-6" style={{color: '#2a3548'}}>Vui lòng kiểm tra hộp thư của bạn và làm theo hướng dẫn.</p>
                                <button
                                    type="button"
                                    onClick={onShowSignIn}
                                    className="rounded-xl py-3 px-6 font-semibold transition-all shadow-lg hover:shadow-xl"
                                    style={{
                                        background: 'linear-gradient(135deg, #2a3548 0%, #3d4a5f 100%)',
                                        color: 'rgb(255, 216, 155)',
                                        border: '2px solid rgba(255, 216, 155, 0.5)'
                                    }}
                                >
                                    QUAY LẠI ĐĂNG NHẬP
                                </button>
                            </div>
                        ) : (
                            <form className="mt-6 space-y-5" onSubmit={handleForgotSubmit(onForgotPassword)}>
                                <div>
                                    <label className="block text-sm font-semibold mb-2" style={{color: '#2a3548'}}>EMAIL</label>
                                    <input
                                        {...registerForgot('email')}
                                        type="email"
                                        className="w-full rounded-xl border-2 px-4 py-3 outline-none focus:ring-2 focus:ring-[#2a3548] font-medium"
                                        style={{
                                            background: 'linear-gradient(135deg, rgba(255, 255, 255, 0.8) 0%, rgba(255, 255, 255, 0.6) 100%)',
                                            borderColor: 'rgba(42, 53, 72, 0.3)',
                                            color: '#2a3548'
                                        }}
                                        placeholder="Nhập email của bạn"
                                    />
                                    {forgotErrors.email && (
                                        <p className="text-red-600 text-sm mt-2 font-medium" style={{color: '#2a3548'}}>{forgotErrors.email.message}</p>
                                    )}
                                </div>

                                <div>
                                    <button
                                        type="submit"
                                        disabled={isSending}
                                        className="w-full rounded-xl py-3 font-semibold transition-all shadow-lg hover:shadow-xl disabled:opacity-60"
                                        style={{
                                            background: 'linear-gradient(135deg, #2a3548 0%, #3d4a5f 100%)',
                                            color: 'rgb(255, 216, 155)',
                                            border: '2px solid rgba(255, 216, 155, 0.5)'
                                        }}
                                    >
                                        {isSending ? 'ĐANG GỬI…' : 'GỬI LIÊN KẾT ĐẶT LẠI'}
                                    </button>
                                </div>

                                <div className="text-center">
                                    <button
                                        type="button"
                                        onClick={onShowSignIn}
                                        className="text-sm font-semibold hover:underline"
                                        style={{color: '#2a3548'}}
                                    >
                                        QUAY LẠI ĐĂNG NHẬP
                                    </button>
                                </div>
                            </form>
                        )}
                    </div>
                </div>
            </div>
        </div>
    );
};

export default PasswordReset;