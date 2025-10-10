// src/pages/auth/password-reset.tsx
import { useEffect, useState } from 'react';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { z } from 'zod';
import { supabase } from '@/lib/supabaseClient'; // Xóa useNavigate import
import { X } from 'lucide-react';
import { Player } from '@lottiefiles/react-lottie-player';

const DRAGON_URL = '/lottie/Chinese_Dragon_Cartoon_Character2.json';

function Dragons() {
    return (
        <>
            <div className="pointer-events-none absolute left-2 md:left-6 top-1/2 -translate-y-1/2 hidden sm:block">
                <div className="dragon-move-in-left">
                    <Player autoplay loop src={DRAGON_URL} style={{ width: 380, height: 380, pointerEvents: 'none' }} />
                </div>
            </div>

            <div className="pointer-events-none absolute right-2 md:right-6 top-1/2 -translate-y-1/2 hidden sm:block [animation-delay:200ms]">
                <div className="scale-x-[-1] dragon-move-in-right">
                    <Player autoplay loop src={DRAGON_URL} style={{ width: 380, height: 380, pointerEvents: 'none' }} />
                </div>
            </div>

            <svg className="absolute inset-0 w-full h-full opacity-[.06] pointer-events-none">
                <defs>
                    <pattern id="grid" width="40" height="40" patternUnits="userSpaceOnUse">
                        <circle cx="20" cy="20" r="1" fill="#d4af37" />
                    </pattern>
                </defs>
                <rect width="100%" height="100%" fill="url(#grid)" />
            </svg>

            <style>{`
        .dragon-move-in-left  { animation: dragon-in-left  6s ease-in-out infinite; will-change: transform; }
        .dragon-move-in-right { animation: dragon-in-right 6s ease-in-out infinite; will-change: transform; }
        @keyframes dragon-in-left  { 0%,100% { transform: translateX(0) } 50% { transform: translateX(16px) } }
        @keyframes dragon-in-right { 0%,100% { transform: translateX(0) } 50% { transform: translateX(-16px) } }
      `}</style>
        </>
    )
}

const forgotPasswordSchema = z.object({
    email: z.string().email('Email không hợp lệ'),
});

const resetPasswordSchema = z.object({
    password: z.string().min(6, 'Mật khẩu phải có ít nhất 6 ký tự'),
    confirmPassword: z.string(),
}).refine((data) => data.password === data.confirmPassword, {
    message: 'Mật khẩu xác nhận không khớp',
    path: ['confirmPassword'],
});

type ForgotPasswordFormData = z.infer<typeof forgotPasswordSchema>;
type ResetPasswordFormData = z.infer<typeof resetPasswordSchema>;

interface PasswordResetProps {
    onClose: () => void;
    onShowSignIn: () => void;
}

const PasswordReset = ({ onClose, onShowSignIn }: PasswordResetProps) => {
    // Xóa dòng này: const navigate = useNavigate();

    const [isEmailSent, setIsEmailSent] = useState<boolean>(false);
    const [isResetSuccess, setIsResetSuccess] = useState<boolean>(false);
    const [isRecovery, setIsRecovery] = useState<boolean>(false);

    // Kiểm tra trạng thái "recovery" do Supabase trả khi user bấm link trong email
    useEffect(() => {
        const checkSession = async () => {
            const { data } = await supabase.auth.getSession();
            const urlParams = new URLSearchParams(window.location.search);
            const type = urlParams.get('type');
            const hasSession = data.session !== null;
            const isRecoveryType = type === 'recovery';
            const isEmailConfirmed = data.session?.user?.email_confirmed_at !== undefined;

            setIsRecovery(hasSession && (isRecoveryType || isEmailConfirmed));
        };
        checkSession();
    }, []);

    // Gửi email đặt lại mật khẩu
    const {
        register: registerForgot,
        handleSubmit: handleForgotSubmit,
        formState: { errors: forgotErrors, isSubmitting: isSending },
    } = useForm<ForgotPasswordFormData>({ resolver: zodResolver(forgotPasswordSchema) });

    const onForgotPassword = async (data: ForgotPasswordFormData) => {
        try {
            const baseUrl = import.meta.env.VITE_APP_URL || '';
            const redirectTo = baseUrl.replace(/\/$/, '') + '/password-reset';

            const { error } = await supabase.auth.resetPasswordForEmail(data.email, {
                redirectTo,
            });
            if (error) throw error;
            setIsEmailSent(true);
        } catch (error: unknown) {
            const err = error as { message?: string };
            console.error('Error sending reset email:', error);
            alert(err?.message ?? 'Gửi email thất bại. Vui lòng thử lại.');
        }
    };

    // Đặt lại mật khẩu khi đang ở trạng thái recovery
    const {
        register: registerReset,
        handleSubmit: handleResetSubmit,
        formState: { errors: resetErrors, isSubmitting: isResetting },
    } = useForm<ResetPasswordFormData>({ resolver: zodResolver(resetPasswordSchema) });

    const onResetPassword = async (data: ResetPasswordFormData) => {
        try {
            const { error } = await supabase.auth.updateUser({ password: data.password });
            if (error) throw error;
            setIsResetSuccess(true);
        } catch (error: unknown) {
            const err = error as { message?: string };
            console.error('Error resetting password:', error);
            alert(err?.message ?? 'Đặt lại mật khẩu thất bại. Vui lòng thử lại.');
        }
    };

    // Nếu ở trạng thái recovery → hiển thị form đặt lại mật khẩu
    if (isRecovery) {
        return (
            <div className="fixed inset-0 z-50 overflow-y-auto">
                <div className="fixed inset-0 bg-black bg-opacity-50 transition-opacity"></div>

                <div className="flex min-h-full items-center justify-center p-4">
                    <div className="relative w-full max-w-md">
                        <div className="relative rounded-2xl bg-white shadow-2xl p-6 md:p-8">
                            <div className="flex justify-between items-center mb-2">
                                <h2 className="text-2xl md:text-3xl font-bold text-[#0c3a73]">
                                    Đặt lại mật khẩu
                                </h2>
                                <button
                                    onClick={onClose}
                                    className="text-gray-400 hover:text-gray-600 transition-colors"
                                >
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
                                        {resetErrors.password && <p className="text-red-500 text-sm mt-1">{resetErrors.password.message}</p>}
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

    // Chưa ở trạng thái recovery → form nhập email
    return (
        <div className="fixed inset-0 z-50 overflow-y-auto">
            <div className="fixed inset-0 bg-black bg-opacity-50 transition-opacity"></div>

            <div className="flex min-h-full items-center justify-center p-4">
                <div className="relative w-full max-w-md">
                    <div className="relative rounded-2xl bg-white shadow-2xl p-6 md:p-8">
                        <div className="flex justify-between items-center mb-2">
                            <h2 className="text-2xl md:text-3xl font-bold text-[#0c3a73]">
                                Quên mật khẩu
                            </h2>
                            <button
                                onClick={onClose}
                                className="text-gray-400 hover:text-gray-600 transition-colors"
                            >
                                <X className="h-6 w-6" />
                            </button>
                        </div>

                        <p className="text-sm text-slate-600 mb-6">
                            Nhập email để nhận liên kết đặt lại mật khẩu
                        </p>

                        {isEmailSent ? (
                            <div className="text-center">
                                <div className="text-green-600 mb-4">Đã gửi email đặt lại mật khẩu!</div>
                                <p className="text-gray-600 mb-4">
                                    Vui lòng kiểm tra hộp thư của bạn và làm theo hướng dẫn.
                                </p>
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
                                    {forgotErrors.email && <p className="text-red-500 text-sm mt-1">{forgotErrors.email.message}</p>}
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

            {/* 2 con rồng */}
            <Dragons />
        </div>
    );
};

export default PasswordReset;