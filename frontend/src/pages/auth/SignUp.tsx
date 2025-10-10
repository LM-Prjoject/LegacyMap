// src/pages/auth/SignUp.tsx
import React from 'react';
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

const signupSchema = z.object({
    name: z.string().min(2, 'Tên phải có ít nhất 2 ký tự'),
    email: z.string().email('Email không hợp lệ'),
    password: z.string().min(6, 'Mật khẩu phải có ít nhất 6 ký tự'),
    confirmPassword: z.string(),
}).refine((data) => data.password === data.confirmPassword, {
    message: 'Mật khẩu xác nhận không khớp',
    path: ['confirmPassword'],
});

type SignupFormData = z.infer<typeof signupSchema>;

interface SignUpProps {
    onClose: () => void;
    onShowSignIn: () => void;
}

const SignUp: React.FC<SignUpProps> = ({ onClose, onShowSignIn }) => {
    // Xóa dòng này: const navigate = useNavigate();

    const {
        register,
        handleSubmit,
        formState: { errors, isSubmitting },
    } = useForm<SignupFormData>({ resolver: zodResolver(signupSchema) });

    const onSubmit = async (data: SignupFormData) => {
        try {
            const redirectTo =
                import.meta.env.VITE_APP_URL?.replace(/\/$/, '') +
                '/password-reset';
            const { error } = await supabase.auth.signUp({
                email: data.email,
                password: data.password,
                options: {
                    emailRedirectTo: redirectTo,
                    data: { name: data.name },
                },
            });
            if (error) throw error;

            // Thông báo & đóng modal
            alert('Đăng ký thành công! Vui lòng kiểm tra email để xác minh tài khoản.');
            onClose();
        } catch (err: any) {
            console.error('Signup error:', err);
            alert(err?.message ?? 'Đăng ký thất bại. Vui lòng thử lại.');
        }
    };

    return (
        <div className="fixed inset-0 z-50 overflow-y-auto">
            {/* Background overlay */}
            <div className="fixed inset-0 bg-black bg-opacity-50 transition-opacity"></div>

            <div className="flex min-h-full items-center justify-center p-4">
                <div className="relative w-full max-w-md">
                    <div className="relative rounded-2xl bg-white shadow-2xl p-6 md:p-8">
                        {/* Nút X đóng */}
                        <div className="flex justify-between items-center mb-2">
                            <h1 className="text-2xl md:text-3xl font-bold text-[#0c3a73]">Đăng ký</h1>
                            <button
                                onClick={onClose}
                                className="text-gray-400 hover:text-gray-600 transition-colors"
                            >
                                <X className="h-6 w-6" />
                            </button>
                        </div>

                        <form className="mt-8 space-y-4" onSubmit={handleSubmit(onSubmit)}>
                            <div>
                                <label className="block text-sm font-medium text-slate-700 mb-1">Họ và tên</label>
                                <input
                                    {...register('name')}
                                    type="text"
                                    className="w-full rounded-lg border border-slate-200 px-3 py-2 outline-none focus:ring-2 focus:ring-[#1e63c7]"
                                    placeholder="Nhập họ và tên"
                                />
                                {errors.name && <p className="text-red-500 text-sm mt-1">{errors.name.message}</p>}
                            </div>

                            <div>
                                <label className="block text-sm font-medium text-slate-700 mb-1">Email</label>
                                <input
                                    {...register('email')}
                                    type="email"
                                    className="w-full rounded-lg border border-slate-200 px-3 py-2 outline-none focus:ring-2 focus:ring-[#1e63c7]"
                                    placeholder="Nhập email"
                                />
                                {errors.email && <p className="text-red-500 text-sm mt-1">{errors.email.message}</p>}
                            </div>

                            <div>
                                <label className="block text-sm font-medium text-slate-700 mb-1">Mật khẩu</label>
                                <input
                                    {...register('password')}
                                    type="password"
                                    className="w-full rounded-lg border border-slate-200 px-3 py-2 outline-none focus:ring-2 focus:ring-[#1e63c7]"
                                    placeholder="Nhập mật khẩu"
                                />
                                {errors.password && (
                                    <p className="text-red-500 text-sm mt-1">{errors.password.message}</p>
                                )}
                            </div>

                            <div>
                                <label className="block text-sm font-medium text-slate-700 mb-1">Xác nhận mật khẩu</label>
                                <input
                                    {...register('confirmPassword')}
                                    type="password"
                                    className="w-full rounded-lg border border-slate-200 px-3 py-2 outline-none focus:ring-2 focus:ring-[#1e63c7]"
                                    placeholder="Nhập lại mật khẩu"
                                />
                                {errors.confirmPassword && (
                                    <p className="text-red-500 text-sm mt-1">{errors.confirmPassword.message}</p>
                                )}
                            </div>

                            <div>
                                <button
                                    type="submit"
                                    disabled={isSubmitting}
                                    className="w-full rounded-lg bg-[#1e63c7] hover:bg-[#0c3a73] text-white font-semibold py-2 transition-all disabled:opacity-60"
                                >
                                    {isSubmitting ? 'Đang xử lý…' : 'Đăng ký'}
                                </button>
                            </div>

                            <div className="text-center">
                                <p className="text-sm text-slate-600">
                                    Đã có tài khoản?{' '}
                                    <button
                                        type="button"
                                        onClick={onShowSignIn}
                                        className="text-[#1e63c7] hover:underline font-semibold"
                                    >
                                        Đăng nhập
                                    </button>
                                </p>
                            </div>
                        </form>
                    </div>
                </div>
            </div>

            {/* 2 con rồng */}
            <Dragons />
        </div>
    );
};

export default SignUp;