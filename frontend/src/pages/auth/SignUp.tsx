import React, { useState } from 'react';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { z } from 'zod';
import { X, CheckCircle, Mail, Clock, AlertCircle, Eye, EyeOff } from 'lucide-react';
import { authApi } from '@/api/auth';
import DragonsBackground from '@/components/visual/DragonsBackground';

interface SuccessModalProps {
    isOpen: boolean;
    onClose: () => void;
    email: string;
}

const SuccessModal: React.FC<SuccessModalProps> = ({ isOpen, onClose, email }) => {
    if (!isOpen) return null;

    return (
        <div className="fixed inset-0 z-[60] flex items-center justify-center p-4" style={{background: 'linear-gradient(135deg, #2a3548 0%, #3d4a5f 25%, #4a5970 50%, #3d4a5f 75%, #2a3548 100%)'}}>
            <div className="absolute inset-0 bg-black bg-opacity-50" onClick={onClose}></div>

            <div className="relative rounded-3xl shadow-2xl max-w-md w-full p-8 animate-scale-in" style={{
                background: 'linear-gradient(135deg, rgba(255, 245, 220, 0.95) 0%, rgba(255, 235, 200, 0.9) 25%, rgba(255, 245, 220, 0.95) 50%, rgba(255, 235, 200, 0.9) 75%, rgba(255, 245, 220, 0.95) 100%)',
                border: '3px solid rgba(255, 216, 155, 0.6)',
                boxShadow: '0 20px 60px rgba(42, 53, 72, 0.3), inset 0 0 100px rgba(255, 255, 255, 0.5)'
            }}>
                <div className="flex justify-center mb-4">
                    <div className="w-20 h-20 rounded-full flex items-center justify-center" style={{
                        background: 'linear-gradient(135deg, rgba(255, 216, 155, 0.3) 0%, rgba(255, 230, 190, 0.2) 100%)',
                        border: '3px solid rgba(255, 216, 155, 0.6)'
                    }}>
                        <CheckCircle className="w-12 h-12" style={{color: '#2a3548'}} />
                    </div>
                </div>

                <h2 className="text-2xl font-black text-center mb-4 tracking-tight" style={{
                    color: '#2a3548',
                    textShadow: '0 3px 15px rgba(42, 53, 72, 0.3)'
                }}>
                    ĐĂNG KÝ THÀNH CÔNG!
                </h2>

                <div className="text-center mb-6 space-y-3" style={{color: '#2a3548'}}>
                    <p className="flex items-center justify-center gap-2 font-semibold">
                        <Mail className="w-5 h-5" />
                        <span>Email xác minh đã được gửi đến:</span>
                    </p>
                    <p className="font-black break-all">{email}</p>

                    <div className="rounded-xl p-4 mt-4 text-left" style={{
                        background: 'linear-gradient(135deg, rgba(255, 216, 155, 0.2) 0%, rgba(255, 230, 190, 0.1) 100%)',
                        border: '2px solid rgba(255, 216, 155, 0.5)'
                    }}>
                        <div className="flex items-start gap-3 mb-2">
                            <Clock className="w-5 h-5 mt-0.5 flex-shrink-0" />
                            <div>
                                <p className="font-bold">VUI LÒNG KIỂM TRA EMAIL</p>
                                <p className="text-sm mt-1 font-medium">
                                    • Kiểm tra hộp thư đến và thư mục spam<br />
                                    • Link xác minh có hiệu lực trong 5 phút<br />
                                    • Xác minh email để bắt đầu sử dụng
                                </p>
                            </div>
                        </div>
                    </div>
                </div>

                <div className="flex gap-3">
                    <button
                        onClick={onClose}
                        className="flex-1 rounded-xl py-3 font-semibold transition-all shadow-lg hover:shadow-xl"
                        style={{
                            background: 'linear-gradient(135deg, #2a3548 0%, #3d4a5f 100%)',
                            color: 'rgb(255, 216, 155)',
                            border: '2px solid rgba(255, 216, 155, 0.5)'
                        }}
                    >
                        ĐÃ HIỂU
                    </button>
                    <button
                        onClick={() => window.open('https://mail.google.com', '_blank')}
                        className="flex-1 rounded-xl py-3 font-semibold transition-all shadow-lg hover:shadow-xl"
                        style={{
                            background: 'linear-gradient(135deg, rgba(255, 255, 255, 0.8) 0%, rgba(255, 255, 255, 0.6) 100%)',
                            border: '2px solid rgba(42, 53, 72, 0.3)',
                            color: '#2a3548'
                        }}
                    >
                        Mở GMAIL
                    </button>
                </div>
            </div>
        </div>
    );
};

const signupSchema = z.object({
    username: z.string().min(3, 'Username phải có ít nhất 3 ký tự').max(20, 'Username tối đa 20 ký tự'),
    email: z.string().email('Email không hợp lệ'),
    password: z
        .string()
        .min(8, 'Mật khẩu phải có ít nhất 8 ký tự')
        .regex(
            /^(?=.*[A-Za-z])(?=.*\d)(?=.*[!@#$%^&*()_+\-=\[\]{};':"\\|,.<>\/?~])[A-Za-z\d!@#$%^&*()_+\-=\[\]{};':"\\|,.<>\/?~]+$/,
            'Mật khẩu phải chứa ít nhất 1 chữ cái, 1 số và 1 ký tự đặc biệt'
        ),
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
    const [showPassword, setShowPassword] = useState(false);
    const [showConfirmPassword, setShowConfirmPassword] = useState(false);

    const {
        register,
        handleSubmit,
        formState: { errors, isSubmitting },
    } = useForm<SignupFormData>({ resolver: zodResolver(signupSchema) });

    const onSubmit = async (data: SignupFormData) => {
        setErrorMessage('');

        try {
            await authApi.register({
                username: data.username,
                email: data.email,
                password: data.password,
                fullName: data.fullName,
                gender: data.gender,
                phone: data.phone,
            });

            setRegisteredEmail(data.email);
            setShowSuccessModal(true);
        } catch (err: any) {
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
        onClose();
    };

    return (
        <>
            <div className="fixed inset-0 z-50 overflow-y-auto scrollbar-hidden" style={{background: 'rgba(42, 53, 72, 0.25)', backdropFilter: 'blur(8px)'}}>
                <div className="min-h-screen flex flex-col items-center justify-start px-4 py-24">
                    <div className="fixed inset-0 z-0 pointer-events-none flex items-center justify-center">
                        <DragonsBackground
                            size={360}
                            showGrid
                            left={{ enabled: true, flipX: true, delayMs: 0 }}
                            right={{ enabled: true, flipX: false, delayMs: 250 }}
                        />
                    </div>

                    <div className="relative w-full max-w-md z-10 pointer-events-auto">
                        <div className="relative rounded-3xl shadow-2xl p-8" style={{
                            background: 'linear-gradient(135deg, rgba(255, 245, 220, 0.95) 0%, rgba(255, 235, 200, 0.9) 25%, rgba(255, 245, 220, 0.95) 50%, rgba(255, 235, 200, 0.9) 75%, rgba(255, 245, 220, 0.95) 100%)',
                            border: '3px solid rgba(255, 216, 155, 0.6)',
                            boxShadow: '0 20px 60px rgba(42, 53, 72, 0.3), inset 0 0 100px rgba(255, 255, 255, 0.5)'
                        }}>
                            <div className="flex justify-between items-center mb-6">
                                <h1 className="text-3xl font-black tracking-tight" style={{
                                    color: '#2a3548',
                                    textShadow: '0 3px 15px rgba(42, 53, 72, 0.3)'
                                }}>ĐĂNG KÝ</h1>
                                <button
                                    onClick={onClose}
                                    disabled={isSubmitting}
                                    className="p-2 rounded-xl transition-all hover:scale-110 disabled:opacity-50"
                                    style={{
                                        background: 'linear-gradient(135deg, #2a3548 0%, #3d4a5f 100%)',
                                        border: '2px solid rgba(255, 216, 155, 0.5)'
                                    }}
                                >
                                    <X className="h-5 w-5" style={{color: 'rgb(255, 216, 155)'}} />
                                </button>
                            </div>

                            {errorMessage && (
                                <div className="mb-6 p-4 rounded-xl flex items-start gap-3" style={{
                                    background: 'linear-gradient(135deg, rgba(255, 216, 155, 0.2) 0%, rgba(255, 230, 190, 0.1) 100%)',
                                    border: '2px solid rgba(255, 216, 155, 0.5)'
                                }}>
                                    <AlertCircle className="w-5 h-5 mt-0.5 flex-shrink-0" style={{color: '#2a3548'}} />
                                    <div>
                                        <p className="font-bold" style={{color: '#2a3548'}}>CÓ LỖI XẢY RA</p>
                                        <p className="text-sm mt-1 font-medium" style={{color: '#2a3548'}}>{errorMessage}</p>
                                    </div>
                                </div>
                            )}

                            <form className="mt-6 space-y-5" onSubmit={handleSubmit(onSubmit)}>
                                <div>
                                    <label className="block text-sm font-semibold mb-2" style={{color: '#2a3548'}}>USERNAME</label>
                                    <input
                                        {...register('username')}
                                        type="text"
                                        disabled={isSubmitting}
                                        className="w-full rounded-xl border-2 px-4 py-3 outline-none focus:ring-2 focus:ring-[#2a3548] font-medium disabled:opacity-50"
                                        style={{
                                            background: 'linear-gradient(135deg, rgba(255, 255, 255, 0.8) 0%, rgba(255, 255, 255, 0.6) 100%)',
                                            borderColor: 'rgba(42, 53, 72, 0.3)',
                                            color: '#2a3548'
                                        }}
                                        placeholder="Nhập username"
                                    />
                                    {errors.username && <p className="text-red-600 text-sm mt-2 font-medium" style={{color: '#2a3548'}}>{errors.username.message}</p>}
                                </div>

                                <div>
                                    <label className="block text-sm font-semibold mb-2" style={{color: '#2a3548'}}>HỌ VÀ TÊN</label>
                                    <input
                                        {...register('fullName')}
                                        type="text"
                                        disabled={isSubmitting}
                                        className="w-full rounded-xl border-2 px-4 py-3 outline-none focus:ring-2 focus:ring-[#2a3548] font-medium disabled:opacity-50"
                                        style={{
                                            background: 'linear-gradient(135deg, rgba(255, 255, 255, 0.8) 0%, rgba(255, 255, 255, 0.6) 100%)',
                                            borderColor: 'rgba(42, 53, 72, 0.3)',
                                            color: '#2a3548'
                                        }}
                                        placeholder="Nhập họ và tên"
                                    />
                                    {errors.fullName && <p className="text-red-600 text-sm mt-2 font-medium" style={{color: '#2a3548'}}>{errors.fullName.message}</p>}
                                </div>

                                <div>
                                    <label className="block text-sm font-semibold mb-2" style={{color: '#2a3548'}}>EMAIL</label>
                                    <input
                                        {...register('email')}
                                        type="email"
                                        disabled={isSubmitting}
                                        className="w-full rounded-xl border-2 px-4 py-3 outline-none focus:ring-2 focus:ring-[#2a3548] font-medium disabled:opacity-50"
                                        style={{
                                            background: 'linear-gradient(135deg, rgba(255, 255, 255, 0.8) 0%, rgba(255, 255, 255, 0.6) 100%)',
                                            borderColor: 'rgba(42, 53, 72, 0.3)',
                                            color: '#2a3548'
                                        }}
                                        placeholder="Nhập email"
                                    />
                                    {errors.email && <p className="text-red-600 text-sm mt-2 font-medium" style={{color: '#2a3548'}}>{errors.email.message}</p>}
                                </div>

                                <div>
                                    <label className="block text-sm font-semibold mb-2" style={{color: '#2a3548'}}>MẬT KHẨU</label>
                                    <div className="relative">
                                        <input
                                            {...register('password')}
                                            type={showPassword ? "text" : "password"}
                                            disabled={isSubmitting}
                                            className="w-full rounded-xl border-2 px-4 py-3 pr-10 outline-none focus:ring-2 focus:ring-[#2a3548] font-medium disabled:opacity-50"
                                            style={{
                                                background: 'linear-gradient(135deg, rgba(255, 255, 255, 0.8) 0%, rgba(255, 255, 255, 0.6) 100%)',
                                                borderColor: 'rgba(42, 53, 72, 0.3)',
                                                color: '#2a3548'
                                            }}
                                            placeholder="Nhập mật khẩu"
                                        />
                                        <button
                                            type="button"
                                            onClick={() => setShowPassword(!showPassword)}
                                            disabled={isSubmitting}
                                            className="absolute inset-y-0 right-0 pr-3 flex items-center"
                                            style={{color: '#2a3548'}}
                                        >
                                            {showPassword ? <EyeOff className="h-5 w-5" /> : <Eye className="h-5 w-5" />}
                                        </button>
                                    </div>
                                    {errors.password && <p className="text-red-600 text-sm mt-2 font-medium" style={{color: '#2a3548'}}>{errors.password.message}</p>}
                                </div>

                                <div>
                                    <label className="block text-sm font-semibold mb-2" style={{color: '#2a3548'}}>XÁC NHẬN MẬT KHẨU</label>
                                    <div className="relative">
                                        <input
                                            {...register('confirmPassword')}
                                            type={showConfirmPassword ? "text" : "password"}
                                            disabled={isSubmitting}
                                            className="w-full rounded-xl border-2 px-4 py-3 pr-10 outline-none focus:ring-2 focus:ring-[#2a3548] font-medium disabled:opacity-50"
                                            style={{
                                                background: 'linear-gradient(135deg, rgba(255, 255, 255, 0.8) 0%, rgba(255, 255, 255, 0.6) 100%)',
                                                borderColor: 'rgba(42, 53, 72, 0.3)',
                                                color: '#2a3548'
                                            }}
                                            placeholder="Nhập lại mật khẩu"
                                        />
                                        <button
                                            type="button"
                                            onClick={() => setShowConfirmPassword(!showConfirmPassword)}
                                            disabled={isSubmitting}
                                            className="absolute inset-y-0 right-0 pr-3 flex items-center"
                                            style={{color: '#2a3548'}}
                                        >
                                            {showConfirmPassword ? <EyeOff className="h-5 w-5" /> : <Eye className="h-5 w-5" />}
                                        </button>
                                    </div>
                                    {errors.confirmPassword && (
                                        <p className="text-red-600 text-sm mt-2 font-medium" style={{color: '#2a3548'}}>{errors.confirmPassword.message}</p>
                                    )}
                                </div>

                                <div>
                                    <label className="block text-sm font-semibold mb-2" style={{color: '#2a3548'}}>GIỚI TÍNH</label>
                                    <select
                                        {...register('gender')}
                                        disabled={isSubmitting}
                                        className="w-full rounded-xl border-2 px-4 py-3 outline-none focus:ring-2 focus:ring-[#2a3548] font-medium disabled:opacity-50"
                                        style={{
                                            background: 'linear-gradient(135deg, rgba(255, 255, 255, 0.8) 0%, rgba(255, 255, 255, 0.6) 100%)',
                                            borderColor: 'rgba(42, 53, 72, 0.3)',
                                            color: '#2a3548'
                                        }}
                                        defaultValue=""
                                    >
                                        <option value="" disabled>Chọn giới tính</option>
                                        <option value="MALE">Nam</option>
                                        <option value="FEMALE">Nữ</option>
                                        <option value="OTHER">Khác</option>
                                    </select>
                                </div>

                                <div>
                                    <button
                                        type="submit"
                                        disabled={isSubmitting}
                                        className="w-full rounded-xl py-3 font-semibold transition-all shadow-lg hover:shadow-xl disabled:opacity-60 disabled:cursor-not-allowed"
                                        style={{
                                            background: 'linear-gradient(135deg, #2a3548 0%, #3d4a5f 100%)',
                                            color: 'rgb(255, 216, 155)',
                                            border: '2px solid rgba(255, 216, 155, 0.5)'
                                        }}
                                    >
                                        {isSubmitting ? (
                                            <span className="flex items-center justify-center gap-2">
                                                <svg className="animate-spin h-5 w-5" viewBox="0 0 24 24" style={{color: 'rgb(255, 216, 155)'}}>
                                                    <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4" fill="none" />
                                                    <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z" />
                                                </svg>
                                                ĐANG XỬ LÝ...
                                            </span>
                                        ) : 'ĐĂNG KÝ'}
                                    </button>
                                </div>

                                <div className="text-center">
                                    <p className="text-sm font-semibold" style={{color: '#2a3548'}}>
                                        Đã có tài khoản?{' '}
                                        <button
                                            type="button"
                                            onClick={onShowSignIn}
                                            disabled={isSubmitting}
                                            className="hover:underline font-black disabled:opacity-50"
                                            style={{color: '#2a3548'}}
                                        >
                                            ĐĂNG NHẬP
                                        </button>
                                    </p>
                                </div>
                            </form>
                        </div>
                    </div>
                </div>
            </div>

            <SuccessModal
                isOpen={showSuccessModal}
                onClose={handleSuccessModalClose}
                email={registeredEmail}
            />
        </>
    );
};

export default SignUp;