// src/pages/auth/SignUp.tsx
import React, { useState } from 'react';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { z } from 'zod';
import { X, CheckCircle, Mail, Clock, AlertCircle } from 'lucide-react';
import { Player } from '@lottiefiles/react-lottie-player';
import { authApi } from '@/api/auth';

const DRAGON_URL = '/lottie/Chinese_Dragon_Cartoon_Character2.json';

// Thêm component SuccessModal
interface SuccessModalProps {
    isOpen: boolean;
    onClose: () => void;
    email: string;
}

const SuccessModal: React.FC<SuccessModalProps> = ({ isOpen, onClose, email }) => {
    if (!isOpen) return null;

    return (
        <div className="fixed inset-0 z-[60] flex items-center justify-center p-4">
            {/* Background overlay */}
            <div className="absolute inset-0 bg-black bg-opacity-50" onClick={onClose}></div>

            {/* Modal content */}
            <div className="relative bg-white rounded-2xl shadow-2xl max-w-md w-full p-6 md:p-8 animate-scale-in">
                {/* Success icon */}
                <div className="flex justify-center mb-4">
                    <div className="w-20 h-20 bg-green-100 rounded-full flex items-center justify-center">
                        <CheckCircle className="w-12 h-12 text-green-600" />
                    </div>
                </div>

                {/* Title */}
                <h2 className="text-2xl md:text-3xl font-bold text-center text-gray-900 mb-4">
                    Đăng Ký Thành Công!
                </h2>

                {/* Message */}
                <div className="text-center text-gray-600 mb-6 space-y-3">
                    <p className="flex items-center justify-center gap-2">
                        <Mail className="w-5 h-5 text-blue-500" />
                        <span>Email xác minh đã được gửi đến:</span>
                    </p>
                    <p className="font-semibold text-blue-600 break-all">{email}</p>

                    <div className="bg-blue-50 border border-blue-200 rounded-lg p-4 mt-4 text-left">
                        <div className="flex items-start gap-3 mb-2">
                            <Clock className="w-5 h-5 text-blue-500 mt-0.5 flex-shrink-0" />
                            <div>
                                <p className="font-medium text-blue-800">Vui lòng kiểm tra email</p>
                                <p className="text-sm text-blue-600 mt-1">
                                    • Kiểm tra hộp thư đến và thư mục spam<br/>
                                    • Link xác minh có hiệu lực trong 24 giờ<br/>
                                    • Xác minh email để bắt đầu sử dụng
                                </p>
                            </div>
                        </div>
                    </div>
                </div>

                {/* Action buttons */}
                <div className="flex gap-3">
                    <button
                        onClick={onClose}
                        className="flex-1 bg-blue-600 hover:bg-blue-700 text-white font-semibold py-3 px-4 rounded-lg transition-colors"
                    >
                        Đã hiểu
                    </button>
                    <button
                        onClick={() => window.open('https://mail.google.com', '_blank')}
                        className="flex-1 border border-blue-600 text-blue-600 hover:bg-blue-50 font-semibold py-3 px-4 rounded-lg transition-colors"
                    >
                        Mở Gmail
                    </button>
                </div>

                {/* Decorative elements */}
                <div className="absolute -top-4 -right-4 w-8 h-8 bg-green-500 rounded-full opacity-20 animate-pulse"></div>
                <div className="absolute -bottom-4 -left-4 w-6 h-6 bg-blue-500 rounded-full opacity-20 animate-pulse"></div>
            </div>

            <style>{`
                @keyframes scale-in {
                    0% { transform: scale(0.8); opacity: 0; }
                    100% { transform: scale(1); opacity: 1; }
                }
                .animate-scale-in {
                    animation: scale-in 0.3s ease-out;
                }
            `}</style>
        </div>
    );
};

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
    username: z.string().min(3, 'Username phải có ít nhất 3 ký tự').max(20, 'Username tối đa 20 ký tự'),
    email: z.string().email('Email không hợp lệ'),
    password: z.string().min(8, 'Mật khẩu phải có ít nhất 8 ký tự')
        .regex(/^(?=.*[A-Za-z])(?=.*\d)(?=.*[@$!%*#?&])[A-Za-z\d@$!%*#?&]+$/,
            'Mật khẩu phải chứa ít nhất 1 chữ cái, 1 số và 1 ký tự đặc biệt'),
    confirmPassword: z.string(),
    fullName: z.string().min(2, 'Họ tên phải có ít nhất 2 ký tự'),
    clanName: z.string().optional(),
    gender: z.string().optional(),
    phone: z.string().optional(),
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
    const [errorMessage, setErrorMessage] = useState('');
    const [showSuccessModal, setShowSuccessModal] = useState(false);
    const [registeredEmail, setRegisteredEmail] = useState('');

    const {
        register,
        handleSubmit,
        formState: { errors, isSubmitting },
    } = useForm<SignupFormData>({ resolver: zodResolver(signupSchema) });

    const onSubmit = async (data: SignupFormData) => {
        setErrorMessage('');

        try {
            console.log('🚀 Bắt đầu đăng ký...', { username: data.username, email: data.email });

            // Gọi API
            await authApi.register({
                username: data.username,
                email: data.email,
                password: data.password,
                fullName: data.fullName,
                clanName: data.clanName,
                gender: data.gender,
                phone: data.phone,
            });

            // Hiển thị modal thành công
            setRegisteredEmail(data.email);
            setShowSuccessModal(true);

        } catch (err: any) {
            console.error('❌ Lỗi đăng ký:', err);

            let errorMsg = 'Đăng ký thất bại. Vui lòng thử lại.';

            if (err.message?.includes('timeout')) {
                errorMsg = 'Kết nối quá chậm. Vui lòng kiểm tra internet và thử lại.';
            } else if (err.response?.data?.message) {
                errorMsg = err.response.data.message;
            } else if (err.message) {
                errorMsg = err.message;
            }

            setErrorMessage(errorMsg);
        }
    };

    const handleSuccessModalClose = () => {
        setShowSuccessModal(false);
        onClose(); // Đóng cả form đăng ký
    };

    return (
        <>
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
                                    disabled={isSubmitting}
                                    className="text-gray-400 hover:text-gray-600 transition-colors disabled:opacity-50"
                                >
                                    <X className="h-6 w-6" />
                                </button>
                            </div>

                            {/* Error Message */}
                            {errorMessage && (
                                <div className="mb-4 p-4 bg-red-50 border border-red-200 rounded-lg flex items-start gap-3">
                                    <AlertCircle className="w-5 h-5 text-red-500 mt-0.5 flex-shrink-0" />
                                    <div>
                                        <p className="font-medium text-red-800">Có lỗi xảy ra</p>
                                        <p className="text-red-600 text-sm mt-1">{errorMessage}</p>
                                    </div>
                                </div>
                            )}

                            <form className="mt-8 space-y-4" onSubmit={handleSubmit(onSubmit)}>
                                <div>
                                    <label className="block text-sm font-medium text-slate-700 mb-1">Username</label>
                                    <input
                                        {...register('username')}
                                        type="text"
                                        disabled={isSubmitting}
                                        className="w-full rounded-lg border border-slate-200 px-3 py-2 outline-none focus:ring-2 focus:ring-[#1e63c7] disabled:opacity-50"
                                        placeholder="Nhập username"
                                    />
                                    {errors.username && <p className="text-red-500 text-sm mt-1">{errors.username.message}</p>}
                                </div>

                                <div>
                                    <label className="block text-sm font-medium text-slate-700 mb-1">Họ và tên</label>
                                    <input
                                        {...register('fullName')}
                                        type="text"
                                        disabled={isSubmitting}
                                        className="w-full rounded-lg border border-slate-200 px-3 py-2 outline-none focus:ring-2 focus:ring-[#1e63c7] disabled:opacity-50"
                                        placeholder="Nhập họ và tên"
                                    />
                                    {errors.fullName && <p className="text-red-500 text-sm mt-1">{errors.fullName.message}</p>}
                                </div>

                                <div>
                                    <label className="block text-sm font-medium text-slate-700 mb-1">Email</label>
                                    <input
                                        {...register('email')}
                                        type="email"
                                        disabled={isSubmitting}
                                        className="w-full rounded-lg border border-slate-200 px-3 py-2 outline-none focus:ring-2 focus:ring-[#1e63c7] disabled:opacity-50"
                                        placeholder="Nhập email"
                                    />
                                    {errors.email && <p className="text-red-500 text-sm mt-1">{errors.email.message}</p>}
                                </div>

                                <div>
                                    <label className="block text-sm font-medium text-slate-700 mb-1">Mật khẩu</label>
                                    <input
                                        {...register('password')}
                                        type="password"
                                        disabled={isSubmitting}
                                        className="w-full rounded-lg border border-slate-200 px-3 py-2 outline-none focus:ring-2 focus:ring-[#1e63c7] disabled:opacity-50"
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
                                        disabled={isSubmitting}
                                        className="w-full rounded-lg border border-slate-200 px-3 py-2 outline-none focus:ring-2 focus:ring-[#1e63c7] disabled:opacity-50"
                                        placeholder="Nhập lại mật khẩu"
                                    />
                                    {errors.confirmPassword && (
                                        <p className="text-red-500 text-sm mt-1">{errors.confirmPassword.message}</p>
                                    )}
                                </div>

                                <div>
                                    <label className="block text-sm font-medium text-slate-700 mb-1">Tên dòng họ (tùy chọn)</label>
                                    <input
                                        {...register('clanName')}
                                        type="text"
                                        disabled={isSubmitting}
                                        className="w-full rounded-lg border border-slate-200 px-3 py-2 outline-none focus:ring-2 focus:ring-[#1e63c7] disabled:opacity-50"
                                        placeholder="Nhập tên dòng họ"
                                    />
                                </div>

                                <div>
                                    <button
                                        type="submit"
                                        disabled={isSubmitting}
                                        className="w-full rounded-lg bg-[#1e63c7] hover:bg-[#0c3a73] text-white font-semibold py-2 transition-all disabled:opacity-60 disabled:cursor-not-allowed"
                                    >
                                        {isSubmitting ? (
                                            <span className="flex items-center justify-center gap-2">
                                                <svg className="animate-spin h-5 w-5" viewBox="0 0 24 24">
                                                    <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4" fill="none"/>
                                                    <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"/>
                                                </svg>
                                                Đang xử lý...
                                            </span>
                                        ) : 'Đăng ký'}
                                    </button>
                                </div>

                                <div className="text-center">
                                    <p className="text-sm text-slate-600">
                                        Đã có tài khoản?{' '}
                                        <button
                                            type="button"
                                            onClick={onShowSignIn}
                                            disabled={isSubmitting}
                                            className="text-[#1e63c7] hover:underline font-semibold disabled:opacity-50"
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

            {/* Modal thông báo thành công */}
            <SuccessModal
                isOpen={showSuccessModal}
                onClose={handleSuccessModalClose}
                email={registeredEmail}
            />
        </>
    );
};

export default SignUp;