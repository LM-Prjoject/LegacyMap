import { useForm } from 'react-hook-form'
import { z } from 'zod'
import { zodResolver } from '@hookform/resolvers/zod'
import { useState } from 'react'
import { Eye, EyeOff, Mail, Lock, X } from 'lucide-react'
import { authApi } from '@/api/auth'
import { Player } from '@lottiefiles/react-lottie-player'

const DRAGON_URL = '/lottie/Chinese_Dragon_Cartoon_Character2.json'

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

const signInSchema = z.object({
    email: z.string().email('Email không hợp lệ'),
    password: z.string().min(6, 'Mật khẩu tối thiểu 6 ký tự')
})

type SignInFormData = z.infer<typeof signInSchema>

interface SignInProps {
    onClose: () => void;
    onShowPasswordReset: () => void;
    onShowSignUp: () => void;
}

export default function SignIn({ onClose, onShowPasswordReset, onShowSignUp }: SignInProps) {
    const [showPwd, setShowPwd] = useState(false)
    const [loading, setLoading] = useState(false)
    const [error, setError] = useState('')

    const { register, handleSubmit, formState: { errors } } = useForm<SignInFormData>({
        resolver: zodResolver(signInSchema)
    })

    const onSubmit = async (data: SignInFormData) => {
        try {
            setLoading(true)
            setError('')
            console.log('🔐 Attempting backend login...')

            const response = await authApi.login({
                identifier: data.email,
                password: data.password
            })

            console.log('✅ Backend login successful:', response)

            if (response.result?.token) {
                // ✅ Lưu token với key NHẤT QUÁN
                localStorage.setItem('authToken', response.result.token)
                localStorage.setItem('user', JSON.stringify(response.result.user))

                console.log('✅ Token saved, redirecting to homepage')

                // ✅ Redirect về homepage sau khi đăng nhập thành công
                window.location.href = '/'
            } else {
                throw new Error('No token received from server')
            }
        } catch (error: any) {
            console.error('❌ Login error:', error)

            const errorMessage = error.response?.data?.message
                || error.message
                || 'Đăng nhập thất bại. Vui lòng thử lại.'

            setError(errorMessage)
        } finally {
            setLoading(false)
        }
    }

    const handleGoogleLogin = async () => {
        setError('Đăng nhập Google chưa được hỗ trợ. Vui lòng sử dụng email/password.')
    }

    return (
        <div className="fixed inset-0 z-50 overflow-y-auto">
            <div className="fixed inset-0 bg-black bg-opacity-50 transition-opacity"></div>

            <div className="flex min-h-full items-center justify-center p-4">
                <div className="relative w-full max-w-md">
                    <div className="relative rounded-2xl bg-white shadow-2xl p-6 md:p-8">
                        <div className="flex justify-between items-center mb-2">
                            <h1 className="text-2xl md:text-3xl font-bold text-[#0c3a73]">Đăng nhập</h1>
                            <button
                                onClick={onClose}
                                className="text-gray-400 hover:text-gray-600 transition-colors"
                            >
                                <X className="h-6 w-6" />
                            </button>
                        </div>

                        <p className="text-sm text-slate-600 mb-6">
                            Nhập email của bạn để đăng nhập
                        </p>

                        <form className="space-y-4" onSubmit={handleSubmit(onSubmit)}>
                            {error && (
                                <div className="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded text-sm">
                                    {error}
                                </div>
                            )}

                            <div>
                                <label className="block text-sm font-medium text-slate-700">
                                    Email
                                </label>
                                <div className="mt-1 relative">
                                    <span className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                                        <Mail className="h-4 w-4 text-slate-400" />
                                    </span>
                                    <input
                                        type="email"
                                        placeholder="email@domain.com"
                                        className="w-full rounded-lg border border-slate-200 pl-9 pr-3 py-2 outline-none focus:ring-2 focus:ring-[#1e63c7] focus:border-transparent"
                                        {...register('email')}
                                    />
                                </div>
                                {errors.email && (
                                    <p className="text-red-600 text-sm mt-1">{errors.email.message}</p>
                                )}
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
                                        onClick={() => setShowPwd(v => !v)}
                                        className="absolute inset-y-0 right-0 pr-3 flex items-center text-slate-500 hover:text-slate-700"
                                    >
                                        {showPwd ? <EyeOff className="h-4 w-4" /> : <Eye className="h-4 w-4" />}
                                    </button>
                                </div>
                                {errors.password && (
                                    <p className="text-red-600 text-sm mt-1">{errors.password.message}</p>
                                )}
                            </div>

                            <div className="flex justify-end">
                                <button
                                    type="button"
                                    onClick={onShowPasswordReset}
                                    className="text-sm text-[#1e63c7] hover:underline"
                                >
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
                            className="w-full rounded-lg border border-slate-200 bg-white hover:bg-slate-50 py-2 font-medium transition-colors"
                        >
                            Đăng nhập bằng Google
                        </button>

                        <p className="mt-6 text-sm text-center text-slate-600">
                            Chưa có tài khoản?{' '}
                            <button
                                onClick={onShowSignUp}
                                className="text-[#1e63c7] hover:underline font-semibold"
                            >
                                Đăng ký ngay
                            </button>
                        </p>
                    </div>
                </div>
            </div>

            <Dragons />
        </div>
    )
}