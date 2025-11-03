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
            <div className="fixed inset-0 z-50 overflow-y-auto">
                <div className="fixed inset-0 bg-black bg-opacity-50 transition-opacity" />

                <div className="flex min-h-full items-center justify-center p-4 relative">
                    <DragonsBackground/>

                    <div className="relative w-full max-w-md">
                        <div className="relative rounded-2xl bg-white shadow-2xl p-6 md:p-8">
                            <div className="flex justify-between items-center mb-2">
                                <h2 className="text-2xl md:text-3xl font-bold text-[#0c3a73]">Đặt lại mật khẩu</h2>
                                <button onClick={onClose} className="text-gray-400 hover:text-gray-600 transition-colors">
                                    <X className="h-6 w-6" />
                                </button>
                            </div>

                            {isResetSuccess ? (
                                <div className="text-center">
                                    <div className="text-green-600 mb-4">Đặt lại mật khẩu thành công!</div>
                                    <button
                                        type="button"
                                        onClick={onShowSignIn}
                                        className="text-indigo-600 hover:text-indigo-500 font-medium"
                                    >
                                        Quay lại đăng nhập
                                    </button>
                                </div>
                            ) : (
                                <form className="mt-8 space-y-4" onSubmit={handleResetSubmit(onResetPassword)}>
                                    <div>
                                        <label className="block text-sm font-medium text-slate-700 mb-1">Mật khẩu mới</label>
                                        <input
                                            {...registerReset('password')}
                                            type="password"
                                            className="w-full rounded-lg border border-slate-200 px-3 py-2 outline-none focus:ring-2 focus:ring-[#1e63c7]"
                                            placeholder="Nhập mật khẩu mới"
                                        />
                                        {resetErrors.password && (
                                            <p className="text-red-500 text-sm mt-1">{resetErrors.password.message}</p>
                                        )}
                                    </div>

                                    <div>
                                        <label className="block text-sm font-medium text-slate-700 mb-1">Xác nhận mật khẩu</label>
                                        <input
                                            {...registerReset('confirmPassword')}
                                            type="password"
                                            className="w-full rounded-lg border border-slate-200 px-3 py-2 outline-none focus:ring-2 focus:ring-[#1e63c7]"
                                            placeholder="Nhập lại mật khẩu"
                                        />
                                        {resetErrors.confirmPassword && (
                                            <p className="text-red-500 text-sm mt-1">{resetErrors.confirmPassword.message}</p>
                                        )}
                                    </div>

                                    <div>
                                        <button
                                            type="submit"
                                            disabled={isResetting}
                                            className="w-full rounded-lg bg-[#1e63c7] hover:bg-[#0c3a73] text-white font-semibold py-2 transition-all disabled:opacity-60"
                                        >
                                            {isResetting ? 'Đang đặt lại…' : 'Đặt lại mật khẩu'}
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
        <div className="fixed inset-0 z-50 overflow-y-auto">
            <div className="fixed inset-0 bg-black bg-opacity-50 transition-opacity" />

            <div className="flex min-h-full items-center justify-center p-4 relative">
                {/* Nền rồng tái sử dụng */}
                <DragonsBackground
                    // size={380}
                    // showGrid
                />

                <div className="relative w-full max-w-md">
                    <div className="relative rounded-2xl bg-white shadow-2xl p-6 md:p-8">
                        <div className="flex justify-between items-center mb-2">
                            <h2 className="text-2xl md:text-3xl font-bold text-[#0c3a73]">Quên mật khẩu</h2>
                            <button onClick={onClose} className="text-gray-400 hover:text-gray-600 transition-colors">
                                <X className="h-6 w-6" />
                            </button>
                        </div>

                        <p className="text-sm text-slate-600 mb-6">Nhập email để nhận liên kết đặt lại mật khẩu</p>

                        {isEmailSent ? (
                            <div className="text-center">
                                <div className="text-green-600 mb-4">Đã gửi email đặt lại mật khẩu!</div>
                                <p className="text-gray-600 mb-4">Vui lòng kiểm tra hộp thư của bạn và làm theo hướng dẫn.</p>
                                <button
                                    type="button"
                                    onClick={onShowSignIn}
                                    className="text-indigo-600 hover:text-indigo-500 font-medium"
                                >
                                    Quay lại đăng nhập
                                </button>
                            </div>
                        ) : (
                            <form className="mt-8 space-y-4" onSubmit={handleForgotSubmit(onForgotPassword)}>
                                <div>
                                    <label className="block text-sm font-medium text-slate-700 mb-1">Email</label>
                                    <input
                                        {...registerForgot('email')}
                                        type="email"
                                        className="w-full rounded-lg border border-slate-200 px-3 py-2 outline-none focus:ring-2 focus:ring-[#1e63c7]"
                                        placeholder="Nhập email của bạn"
                                    />
                                    {forgotErrors.email && (
                                        <p className="text-red-500 text-sm mt-1">{forgotErrors.email.message}</p>
                                    )}
                                </div>

                                <div>
                                    <button
                                        type="submit"
                                        disabled={isSending}
                                        className="w-full rounded-lg bg-[#1e63c7] hover:bg-[#0c3a73] text-white font-semibold py-2 transition-all disabled:opacity-60"
                                    >
                                        {isSending ? 'Đang gửi…' : 'Gửi liên kết đặt lại'}
                                    </button>
                                </div>

                                <div className="text-center">
                                    <button
                                        type="button"
                                        onClick={onShowSignIn}
                                        className="text-indigo-600 hover:text-indigo-500 font-medium"
                                    >
                                        Quay lại đăng nhập
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