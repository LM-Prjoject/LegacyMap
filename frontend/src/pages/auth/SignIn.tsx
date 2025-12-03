import { useForm } from 'react-hook-form';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { useState, useEffect } from 'react';
import { Eye, EyeOff, Mail, Lock, X, CheckCircle, AlertCircle} from 'lucide-react';
import { authApi } from '@/api/auth';
import { useNavigate } from 'react-router-dom';
import DragonsBackground from '@/components/visual/DragonsBackground';
import { FcGoogle } from "react-icons/fc";
import LockCountdownModal from '@/components/auth/LockCountdownModal';
import UnbanRequestModal from "@/components/auth/UnbanRequestModal";

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

    const [loginStatus, setLoginStatus] = useState<{
        failedAttempts?: number;
        isLocked?: boolean;
        lockSecondsLeft?: number;
        isBanned?: boolean;
        canRequestUnban?: boolean;
    } | null>(null);
    const [lockCountdown, setLockCountdown] = useState<number | null>(null);
    const [showLockModal, setShowLockModal] = useState(false);
    const [showUnbanModal, setShowUnbanModal] = useState(false);

    useEffect(() => {
        const urlParams = new URLSearchParams(window.location.search);

        const errorParam = urlParams.get('error');
        if (errorParam) {
            let errorMessage = '';

            switch (errorParam) {
                case 'banned':
                    errorMessage = 'Tài khoản của bạn đã bị khóa.';
                    setLoginStatus({
                        failedAttempts: 0,
                        isLocked: false,
                        lockSecondsLeft: 0,
                        isBanned: true,
                        canRequestUnban: true,
                    });
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

        const token = urlParams.get('token');
        if (token) handleEmailVerification(token);
    }, []);

    useEffect(() => {
        const urlParams = new URLSearchParams(window.location.search);
        const hasVerificationToken = urlParams.get('token');
        if (hasVerificationToken) return;
        const existingToken = localStorage.getItem('authToken');
        if (existingToken) {
            const redirect = urlParams.get('redirect');
            if (redirect && redirect.startsWith('/')) {
                navigate(redirect);
            } else {
                navigate('/');
            }
        }
    }, []);

    useEffect(() => {
        if (lockCountdown == null || lockCountdown <= 0) return;

        const interval = setInterval(() => {
            setLockCountdown((prev) => {
                if (prev == null) return prev;
                if (prev <= 1) {
                    setShowLockModal(false);
                    setLoginStatus((old) =>
                        old ? {...old, isLocked: false, lockSecondsLeft: 0} : old
                    );
                    return 0;
                }
                return prev - 1;
            });
        }, 1000);

        return () => clearInterval(interval);
    }, [lockCountdown]);

    const handlePasswordFocus = () => {
        if (loginStatus?.isLocked && lockCountdown && lockCountdown > 0) {
            setShowLockModal(true);
        }
    };

    const handleEmailVerification = async (token: string) => {
        try {
            setLoading(true);
            setError('');
            const response = await authApi.verifyEmail(token);

            if (response.success && response.result) {
                const {user, token: authToken} = response.result;
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

                            const redirectUrl = localStorage.getItem('redirectAfterLogin');

                            if (redirectUrl) {
                                localStorage.removeItem('redirectAfterLogin');
                                window.location.href = redirectUrl;
                            } else {
                                navigate('/');
                            }
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

    const {register, handleSubmit, formState: {errors}, watch} =
        useForm<SignInFormData>({
            resolver: zodResolver(signInSchema),
        });

    const identifierValue = watch('identifier') || '';

    const onSubmit = async (data: SignInFormData) => {
        try {
            setLoading(true);
            setError('');
            setLoginStatus(null);

            const response = await authApi.login({
                identifier: data.identifier,
                password: data.password,
            });

            if (response.result?.token) {
                const {token, user} = response.result;

                localStorage.setItem('authToken', token);
                localStorage.setItem('user', JSON.stringify(user));

                const redirectUrl = localStorage.getItem('redirectAfterLogin');

                if (redirectUrl) {
                    localStorage.removeItem('redirectAfterLogin');
                    window.location.href = redirectUrl;
                } else {
                    window.location.href = '/';
                }
            } else {
                throw new Error('No token received from server');
            }
        } catch (error: any) {
            let errorMessage = 'Tài khoản hoặc mật khẩu không đúng.';

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
                }
            } else if (error.message) {
                errorMessage = error.message;
            }
            try {
                const statusRes = await authApi.getLoginStatus(data.identifier);
                const s = statusRes.result;

                if (s) {
                    setLoginStatus({
                        failedAttempts: s.failedAttempts,
                        isLocked: s.isLocked,
                        lockSecondsLeft: s.lockSecondsLeft,
                        isBanned: s.isBanned,
                        canRequestUnban: s.canRequestUnban,
                    });

                    if (s.isBanned) {
                        errorMessage = 'Tài khoản của bạn đã bị khóa';
                    } else if (s.isLocked && s.lockSecondsLeft > 0) {
                        setLockCountdown(s.lockSecondsLeft);
                        setShowLockModal(true);
                    } else if (s.failedAttempts >= 1) {
                        errorMessage = `Tài khoản hoặc mật khẩu không đúng.`;
                    }
                }
            } catch (statusErr) {
                console.error('Không lấy được login status:', statusErr);
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
            <div className="fixed inset-0 z-50 overflow-y-auto scrollbar-hidden"
                 style={{background: 'rgba(42, 53, 72, 0.25)', backdropFilter: 'blur(8px)'}}>
                <div className="min-h-screen flex items-center justify-center px-4 py-24 relative">
                    <DragonsBackground
                        size={380}
                        showGrid
                        left={{enabled: true, flipX: true, delayMs: 0}}
                        right={{enabled: true, flipX: false, delayMs: 200}}
                    />

                    <div className="relative w-full max-w-md">
                        {!showUnbanModal && (
                        <div className="relative rounded-3xl shadow-2xl p-8" style={{
                            background: 'linear-gradient(135deg, rgba(255, 245, 220, 0.95) 0%, rgba(255, 235, 200, 0.9) 25%, rgba(255, 245, 220, 0.95) 50%, rgba(255, 235, 200, 0.9) 75%, rgba(255, 245, 220, 0.95) 100%)',
                            border: '3px solid rgba(255, 216, 155, 0.6)',
                            boxShadow: '0 20px 60px rgba(42, 53, 72, 0.3), inset 0 0 100px rgba(255, 255, 255, 0.5)'
                        }}>
                            <div className="flex justify-between items-center mb-6">
                                <h1 className="text-3xl font-black tracking-tight" style={{
                                    color: '#2a3548',
                                    textShadow: '0 3px 15px rgba(42, 53, 72, 0.3)'
                                }}>
                                    {verificationSuccess ? 'XÁC MINH THÀNH CÔNG!' : 'ĐĂNG NHẬP'}
                                </h1>
                                <button
                                    onClick={onClose}
                                    className="p-2 rounded-xl transition-all hover:scale-110"
                                    style={{
                                        background: 'linear-gradient(135deg, #2a3548 0%, #3d4a5f 100%)',
                                        border: '2px solid rgba(255, 216, 155, 0.5)'
                                    }}
                                >
                                    <X className="h-5 w-5" style={{color: 'rgb(255, 216, 155)'}}/>
                                </button>
                            </div>

                            {verificationSuccess ? (
                                <div className="text-center py-6">
                                    <div className="mb-6">
                                        <div
                                            className="mx-auto w-20 h-20 rounded-full flex items-center justify-center mb-4"
                                            style={{
                                                background: 'linear-gradient(135deg, rgba(255, 216, 155, 0.3) 0%, rgba(255, 230, 190, 0.2) 100%)',
                                                border: '3px solid rgba(255, 216, 155, 0.6)'
                                            }}>
                                            <CheckCircle className="h-10 w-10" style={{color: '#2a3548'}}/>
                                        </div>
                                        <p className="text-lg font-bold mb-2" style={{color: '#2a3548'}}>Email của bạn
                                            đã được xác minh thành công!</p>
                                        {autoLoginSuccess && (
                                            <>
                                                <p className="text-sm mb-4" style={{color: '#2a3548'}}>
                                                    Đăng nhập tự động thành công. Chào mừng{' '}
                                                    <span className="font-black"
                                                          style={{color: '#2a3548'}}>{userName}</span>!
                                                </p>
                                                <p className="text-sm" style={{color: '#2a3548'}}>
                                                    Chuyển đến trang chủ trong{' '}
                                                    <span className="font-black"
                                                          style={{color: '#2a3548'}}>{countdown}</span> giây...
                                                </p>
                                            </>
                                        )}
                                    </div>
                                    <button
                                        onClick={() => {
                                            const redirectUrl = localStorage.getItem('redirectAfterLogin');
                                            if (redirectUrl) {
                                                localStorage.removeItem('redirectAfterLogin');
                                                window.location.href = redirectUrl;
                                            } else {
                                                navigate('/');
                                            }
                                        }}
                                        className="w-full rounded-xl py-3 font-semibold transition-all shadow-lg hover:shadow-xl"
                                        style={{
                                            background: 'linear-gradient(135deg, #2a3548 0%, #3d4a5f 100%)',
                                            color: 'rgb(255, 216, 155)',
                                            border: '2px solid rgba(255, 216, 155, 0.5)'
                                        }}
                                    >
                                        VÀO TRANG CHỦ NGAY
                                    </button>
                                </div>
                            ) : (
                                <>
                                    {error && (
                                        <div className="mb-4 p-3 rounded-xl flex items-center gap-2 text-sm" style={{
                                            background: 'linear-gradient(135deg, rgba(239, 68, 68, 0.1) 0%, rgba(239, 68, 68, 0.05) 100%)',
                                            border: '1px solid rgba(239, 68, 68, 0.3)'
                                        }}>
                                            <AlertCircle className="w-4 h-4 flex-shrink-0" style={{color: '#dc2626'}}/>
                                            <span className="font-medium" style={{color: '#dc2626'}}>{error}</span>
                                        </div>
                                    )}

                                    {loginStatus && (
                                        <div className="mb-4 text-xs font-medium" style={{color: '#2a3548'}}>
                                            {loginStatus.isBanned && loginStatus.canRequestUnban && (
                                                <button
                                                    type="button"
                                                    onClick={() => {
                                                        setShowUnbanModal(true);
                                                    }}
                                                    className="text-xs font-semibold no-underline underline-offset-2"
                                                    style={{color: "#b45309"}}
                                                >
                                                    Gửi yêu cầu mở khóa tài khoản
                                                </button>
                                            )}
                                        </div>
                                    )}

                                    <form className="space-y-5" onSubmit={handleSubmit(onSubmit)}>
                                        {/* ... input fields giữ nguyên ... */}
                                        <div>
                                            <label className="block text-sm font-semibold mb-2"
                                                   style={{color: '#2a3548'}}>TÀI KHOẢN</label>
                                            <div className="relative">
                                            <span
                                                className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                                                <Mail className="h-5 w-5" style={{color: '#2a3548'}}/>
                                            </span>
                                                <input
                                                    {...register('identifier')}
                                                    placeholder="email@domain.com hoặc username"
                                                    autoComplete="username"
                                                    className="w-full rounded-xl border-2 px-10 py-3 outline-none focus:ring-2 focus:ring-[#2a3548] focus:border-transparent font-medium"
                                                    style={{
                                                        background: 'linear-gradient(135deg, rgba(255, 255, 255, 0.8) 0%, rgba(255, 255, 255, 0.6) 100%)',
                                                        borderColor: 'rgba(42, 53, 72, 0.3)',
                                                        color: '#2a3548'
                                                    }}
                                                />
                                            </div>
                                            {errors.identifier && <p className="text-red-600 text-sm mt-2 font-medium"
                                                                     style={{color: '#2a3548'}}>{errors.identifier.message}</p>}
                                        </div>

                                        <div>
                                            <label className="block text-sm font-semibold mb-2"
                                                   style={{color: '#2a3548'}}>MẬT KHẨU</label>
                                            <div className="relative">
                                            <span
                                                className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                                                <Lock className="h-5 w-5" style={{color: '#2a3548'}}/>
                                            </span>
                                                <input
                                                    type={showPwd ? 'text' : 'password'}
                                                    placeholder="••••••••"
                                                    className="w-full rounded-xl border-2 px-10 py-3 pr-10 outline-none focus:ring-2 focus:ring-[#2a3548] focus:border-transparent font-medium"
                                                    style={{
                                                        background: 'linear-gradient(135deg, rgba(255, 255, 255, 0.8) 0%, rgba(255, 255, 255, 0.6) 100%)',
                                                        borderColor: 'rgba(42, 53, 72, 0.3)',
                                                        color: '#2a3548'
                                                    }}
                                                    {...register('password')}
                                                    onFocus={handlePasswordFocus}
                                                />
                                                <button
                                                    type="button"
                                                    onClick={() => setShowPwd((v) => !v)}
                                                    className="absolute inset-y-0 right-0 pr-3 flex items-center"
                                                    style={{color: '#2a3548'}}
                                                >
                                                    {showPwd ? <EyeOff className="h-5 w-5"/> :
                                                        <Eye className="h-5 w-5"/>}
                                                </button>
                                            </div>
                                            {errors.password && <p className="text-red-600 text-sm mt-2 font-medium"
                                                                   style={{color: '#2a3548'}}>{errors.password.message}</p>}
                                        </div>

                                        <div className="flex justify-end">
                                            <button
                                                type="button"
                                                onClick={onShowPasswordReset}
                                                className="text-sm font-semibold hover:underline"
                                                style={{color: '#2a3548'}}
                                            >
                                                Quên mật khẩu?
                                            </button>
                                        </div>

                                        <button
                                            type="submit"
                                            disabled={loading}
                                            className="w-full rounded-xl py-3 font-semibold transition-all shadow-lg hover:shadow-xl disabled:opacity-60 disabled:cursor-not-allowed"
                                            style={{
                                                background: 'linear-gradient(135deg, #2a3548 0%, #3d4a5f 100%)',
                                                color: 'rgb(255, 216, 155)',
                                                border: '2px solid rgba(255, 216, 155, 0.5)'
                                            }}
                                        >
                                            {loading ? 'ĐANG ĐĂNG NHẬP...' : 'ĐĂNG NHẬP'}
                                        </button>
                                    </form>

                                    <div className="flex items-center gap-4 my-6">
                                        <div className="flex-1 border-t"
                                             style={{borderColor: 'rgba(42, 53, 72, 0.3)'}}/>
                                        <span className="text-m font-semibold" style={{color: '#2a3548'}}>Hoặc</span>
                                        <div className="flex-1 border-t"
                                             style={{borderColor: 'rgba(42, 53, 72, 0.3)'}}/>
                                    </div>

                                    <button
                                        onClick={handleGoogleLogin}
                                        className="w-full flex items-center justify-center gap-3 rounded-xl border-2 py-3 font-semibold transition-all hover:scale-[1.02] shadow-lg"
                                        style={{
                                            background: 'linear-gradient(135deg, rgba(255, 255, 255, 0.8) 0%, rgba(255, 255, 255, 0.6) 100%)',
                                            borderColor: 'rgba(42, 53, 72, 0.3)',
                                            color: '#2a3548'
                                        }}
                                    >
                                        <FcGoogle size={20}/>
                                        <span>Đăng nhập bằng Google</span>
                                    </button>

                                    <p className="mt-6 text-sm text-center font-semibold" style={{color: '#2a3548'}}>
                                        Chưa có tài khoản?{' '}
                                        <button
                                            onClick={onShowSignUp}
                                            className="hover:underline font-black"
                                            style={{color: '#2a3548'}}
                                        >
                                            ĐĂNG KÝ NGAY
                                        </button>
                                    </p>
                                </>
                            )}
                            <LockCountdownModal
                                open={showLockModal && !!lockCountdown && lockCountdown > 0}
                                secondsLeft={lockCountdown ?? 0}
                                onClose={() => setShowLockModal(false)}
                            />
                        </div>
                        )}
                    </div>
                </div>
                <UnbanRequestModal
                    open={showUnbanModal}
                    onClose={() => setShowUnbanModal(false)}
                    identifier={identifierValue}
                />
            </div>
        );
    }