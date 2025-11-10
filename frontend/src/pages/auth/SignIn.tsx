import { useForm } from 'react-hook-form';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { useState, useEffect } from 'react';
import { Eye, EyeOff, Mail, Lock, X, CheckCircle } from 'lucide-react';
import { authApi } from '@/api/auth';
import { useNavigate } from 'react-router-dom';
import DragonsBackground from '@/components/visual/DragonsBackground';
import { FcGoogle } from "react-icons/fc";

const isEmail = (s: string) => /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(s);
const isUsername = (s: string) => /^[a-zA-Z0-9._-]{3,30}$/.test(s);

export const signInSchema = z.object({
    identifier: z
        .string()
        .trim()
        .min(3, 'Nhập email hoặc username')
        .refine((v) => isEmail(v) || isUsername(v), { message: 'Email hoặc username không hợp lệ' }),
    password: z.string().min(8, 'Mật khẩu tối thiểu 8 ký tự'),
});
export type SignInInput = z.infer<typeof signInSchema>;
type SignInFormData = SignInInput;

interface SignInProps {
    onClose: () => void;
    onShowPasswordReset: () => void;
    onShowSignUp: () => void;
}

export default function SignIn({ onClose, onShowPasswordReset, onShowSignUp }: SignInProps) {
    const [showPwd, setShowPwd] = useState(false);
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState('');
    const [verificationSuccess, setVerificationSuccess] = useState(false);
    const [autoLoginSuccess, setAutoLoginSuccess] = useState(false);
    const [countdown, setCountdown] = useState(3);
    const [userName, setUserName] = useState('');
    const navigate = useNavigate();

    useEffect(() => {
        const urlParams = new URLSearchParams(window.location.search);

        const errorParam = urlParams.get('error');
        if (errorParam) {
            let errorMessage = '';

            switch(errorParam) {
                case 'banned':
                    errorMessage = 'Tài khoản của bạn đã bị khóa.';
                    break;
                case 'disabled':
                    errorMessage = 'Tài khoản đã bị vô hiệu hóa.';
                    break;
                case 'auth_failed':
                    errorMessage = 'Đăng nhập Google thất bại. Vui lòng thử lại.';
                    break;
                case 'user_not_found':
                    errorMessage = 'Tài khoản Google chưa được đăng ký trong hệ thống.';
                    break;
                case 'missing_email':
                    errorMessage = 'Không lấy được email từ Google. Vui lòng thử lại.';
                    break;
                default:
                    errorMessage = `Đăng nhập thất bại: ${errorParam}`;
            }

            setError(errorMessage);
            window.history.replaceState({}, '', window.location.pathname);
        }

        // Xử lý email verification
        const token = urlParams.get('token');
        if (token) handleEmailVerification(token);
    }, []);

    const handleEmailVerification = async (token: string) => {
        try {
            setLoading(true);
            setError('');
            const response = await authApi.verifyEmail(token);

            if (response.success && response.result) {
                const { user, token: authToken } = response.result;
                if (!authToken) throw new Error('Đã xảy ra lỗi. Vui lòng thử lại sau.');

                setUserName(user?.profile?.fullName ?? user?.email ?? 'người dùng');
                localStorage.setItem('authToken', authToken);
                localStorage.setItem('user', JSON.stringify(user));
                setVerificationSuccess(true);
                setAutoLoginSuccess(true);

                const countdownInterval = setInterval(() => {
                    setCountdown((prev) => {
                        if (prev <= 1) {
                            clearInterval(countdownInterval);
                            navigate('/');
                            return 0;
                        }
                        return prev - 1;
                    });
                }, 1000);
            } else {
                setError(response.message || 'Xác minh email thất bại');
            }
        } catch {
            setError('Xác minh email thất bại. Vui lòng thử lại.');
        } finally {
            setLoading(false);
        }
    };

    const { register, handleSubmit, formState: { errors } } = useForm<SignInFormData>({
        resolver: zodResolver(signInSchema),
    });

    const onSubmit = async (data: SignInFormData) => {
        try {
            setLoading(true);
            setError('');

            const response = await authApi.login({
                identifier: data.identifier,
                password: data.password,
            });

            if (response.result?.token) {
                const { token, user } = response.result;

                localStorage.setItem('authToken', token);
                localStorage.setItem('user', JSON.stringify(user));
                window.location.href = '/';
            } else {
                throw new Error('No token received from server');
            }
        } catch (error: any) {

            let errorMessage = 'Đăng nhập thất bại. Vui lòng thử lại.';

            if (error.response?.data?.message) {
                const backendMessage = error.response.data.message;

                if (backendMessage.includes('banned') || backendMessage.includes('USER_BANNED')) {
                    errorMessage = 'Tài khoản của bạn đã bị khóa';
                } else if (backendMessage.includes('disabled') || backendMessage.includes('ACCOUNT_DISABLED')) {
                    errorMessage = 'Tài khoản đã bị vô hiệu hóa.';
                } else if (backendMessage.includes('not verified') || backendMessage.includes('ACCOUNT_NOT_VERIFIED')) {
                    errorMessage = 'Vui lòng xác minh email trước khi đăng nhập.';
                } else if (backendMessage.includes('credentials') || backendMessage.includes('INVALID_CREDENTIALS')) {
                    errorMessage = 'Tài khoản hoặc mật khẩu không đúng.';
                } else if (backendMessage.includes('Google') || backendMessage.includes('OAUTH_GOOGLE_ONLY')) {
                    errorMessage = 'Tài khoản này chỉ có thể đăng nhập bằng Google.';
                } else {
                    errorMessage = backendMessage;
                }
            } else if (error.message) {
                errorMessage = error.message;
            }

            setError(errorMessage);
        } finally {
            setLoading(false);
        }
    };

    const getBackendBase = () =>
        (import.meta.env.VITE_API_BASE_URL || 'http://localhost:8080/legacy/api').replace(/\/api\/?$/, '');

    const handleGoogleLogin = () => {
        window.location.assign(`${getBackendBase()}/oauth2/authorization/google`);
    };

    return (
        <div className="fixed inset-0 z-50 overflow-y-auto">
            <div className="fixed inset-0 bg-black bg-opacity-50 transition-opacity" />

            <div className="flex min-h-full items-center justify-center p-4 relative">
                <DragonsBackground
                    size={380}
                    showGrid
                    left={{ enabled: true, flipX: true, delayMs: 0 }}
                    right={{ enabled: true, flipX: false, delayMs: 200 }}
                />

                <div className="relative w-full max-w-md">
                    <div className="relative rounded-2xl bg-white shadow-2xl p-6 md:p-8">
                        <div className="flex justify-between items-center mb-2">
                            <h1 className="text-2xl md:text-3xl font-bold text-[#0c3a73]">
                                {verificationSuccess ? 'Xác minh email thành công!' : 'Đăng nhập'}
                            </h1>
                            <button onClick={onClose} className="text-gray-400 hover:text-gray-600 transition-colors">
                                <X className="h-6 w-6" />
                            </button>
                        </div>

                        {verificationSuccess ? (
                            <div className="text-center">
                                <div className="mb-6">
                                    <div className="mx-auto w-16 h-16 bg-green-100 rounded-full flex items-center justify-center mb-4">
                                        <CheckCircle className="h-8 w-8 text-green-600" />
                                    </div>
                                    <p className="text-lg text-green-600 font-semibold mb-2">Email của bạn đã được xác minh thành công!</p>
                                    {autoLoginSuccess && (
                                        <>
                                            <p className="text-sm text-slate-600 mb-4">
                                                Đăng nhập tự động thành công. Chào mừng{' '}
                                                <span className="font-semibold text-[#1e63c7]">{userName}</span>!
                                            </p>
                                            <p className="text-sm text-slate-600">
                                                Chuyển đến trang chủ trong{' '}
                                                <span className="font-semibold text-[#1e63c7]">{countdown}</span> giây...
                                            </p>
                                        </>
                                    )}
                                </div>
                                <button
                                    onClick={() => navigate('/')}
                                    className="w-full rounded-lg bg-[#1e63c7] hover:bg-[#0c3a73] text-white font-semibold py-2 transition-all"
                                >
                                    Vào trang chủ ngay
                                </button>
                            </div>
                        ) : (
                            <>
                                <form className="space-y-4" onSubmit={handleSubmit(onSubmit)}>
                                    {error && (
                                        <div className="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded text-sm">
                                            {error}
                                        </div>
                                    )}

                                    <div>
                                        <label className="block text-sm font-medium text-slate-700">Tài khoản</label>
                                        <div className="mt-1 relative">
                      <span className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                        <Mail className="h-4 w-4 text-slate-400" />
                      </span>
                                            <input
                                                {...register('identifier')}
                                                placeholder="email@domain.com hoặc username"
                                                autoComplete="username"
                                                className="w-full rounded-lg border border-slate-200 pl-9 pr-3 py-2 outline-none focus:ring-2 focus:ring-[#1e63c7] focus:border-transparent"
                                            />
                                        </div>
                                        {errors.identifier && <p className="text-red-600 text-sm mt-1">{errors.identifier.message}</p>}
                                    </div>

                                    <div>
                                        <label className="block text-sm font-medium text-slate-700">Mật khẩu</label>
                                        <div className="mt-1 relative">
                      <span className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                        <Lock className="h-4 w-4 text-slate-400" />
                      </span>
                                            <input
                                                type={showPwd ? 'text' : 'password'}
                                                placeholder="••••••••"
                                                className="w-full rounded-lg border border-slate-200 pl-9 pr-10 py-2 outline-none focus:ring-2 focus:ring-[#1e63c7] focus:border-transparent"
                                                {...register('password')}
                                            />
                                            <button
                                                type="button"
                                                onClick={() => setShowPwd((v) => !v)}
                                                className="absolute inset-y-0 right-0 pr-3 flex items-center text-slate-500 hover:text-slate-700"
                                            >
                                                {showPwd ? <EyeOff className="h-4 w-4" /> : <Eye className="h-4 w-4" />}
                                            </button>
                                        </div>
                                        {errors.password && <p className="text-red-600 text-sm mt-1">{errors.password.message}</p>}
                                    </div>

                                    <div className="flex justify-end">
                                        <button type="button" onClick={onShowPasswordReset} className="text-sm text-[#1e63c7] hover:underline">
                                            Quên mật khẩu?
                                        </button>
                                    </div>

                                    <button
                                        type="submit"
                                        disabled={loading}
                                        className="w-full rounded-lg bg-[#1e63c7] hover:bg-[#0c3a73] text-white font-semibold py-2 transition-all disabled:opacity-60 disabled:cursor-not-allowed"
                                    >
                                        {loading ? 'Đang đăng nhập...' : 'Đăng nhập'}
                                    </button>
                                </form>

                                <div className="relative my-6">
                                    <div className="border-t border-slate-200" />
                                    <div className="absolute inset-0 flex items-center justify-center">
                                        <span className="px-3 bg-white text-slate-500 text-sm">Hoặc</span>
                                    </div>
                                </div>

                                <button
                                    onClick={handleGoogleLogin}
                                    className="w-full flex items-center justify-center gap-2 rounded-lg border border-slate-200 bg-white hover:bg-slate-50 py-2 font-medium transition-colors"
                                >
                                    <FcGoogle size={18} />
                                    <span>Đăng nhập bằng Google</span>
                                </button>

                                <p className="mt-6 text-sm text-center text-slate-600">
                                    Chưa có tài khoản?{' '}
                                    <button onClick={onShowSignUp} className="text-[#1e63c7] hover:underline font-semibold">
                                        Đăng ký ngay
                                    </button>
                                </p>
                            </>
                        )}
                    </div>
                </div>
            </div>
        </div>
    );
}