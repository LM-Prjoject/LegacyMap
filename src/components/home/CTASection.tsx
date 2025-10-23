// src/components/home/CTASection.tsx
import { Check } from 'lucide-react';

interface CTASectionProps {
    onLoginClick: () => void;
    onSignupClick: () => void;
}

const features = [
    'Miễn phí mãi mãi',
    'Không giới hạn thành viên',
    'Hỗ trợ 24/7'
];

export default function CTASection({ onLoginClick, onSignupClick }: CTASectionProps) {
    return (
        <section className="py-20 relative overflow-hidden">
            {/* Background với màu #f7eede */}
            <div className="absolute inset-0 bg-[#f7eede]">
                <div className="absolute inset-0 bg-[radial-gradient(circle_at_30%_50%,rgba(209,176,102,0.2),transparent_50%)] animate-pulse-slow" />
                <div className="absolute inset-0 bg-[radial-gradient(circle_at_70%_50%,rgba(244,216,138,0.15),transparent_50%)] animate-pulse-slow animation-delay-1000" />
            </div>

            {/* Floating particles */}
            <div className="absolute inset-0 overflow-hidden pointer-events-none">
                {[...Array(15)].map((_, i) => (
                    <div
                        key={i}
                        className="absolute w-2 h-2 bg-[#053D88]/20 rounded-full animate-float"
                        style={{
                            left: `${Math.random() * 100}%`,
                            top: `${Math.random() * 100}%`,
                            animationDelay: `${Math.random() * 5}s`,
                            animationDuration: `${8 + Math.random() * 12}s`
                        }}
                    />
                ))}
            </div>

            <div className="relative max-w-7xl mx-auto px-6">
                <div className="max-w-4xl mx-auto">
                    {/* Main content */}
                    <div className="text-center mb-12">
                        <h2 className="text-4xl lg:text-6xl font-extrabold text-[#053D88] mb-6 leading-tight">
                            Bắt đầu xây dựng cây gia phả ngay hôm nay
                        </h2>
                        <p className="text-xl text-[#074aa8] mb-8 leading-relaxed">
                            Miễn phí đăng ký và tạo cây gia phả đầu tiên. Không cần thẻ tín dụng.
                        </p>

                        {/* Features checklist */}
                        <div className="flex flex-wrap justify-center gap-6 mb-10">
                            {features.map((feature, index) => (
                                <div key={index} className="flex items-center gap-2 text-[#053D88]">
                                    <div className="w-6 h-6 rounded-full bg-gradient-to-br from-[#D1B066] to-[#f4d88a] flex items-center justify-center flex-shrink-0">
                                        <Check className="w-4 h-4 text-[#053D88]" />
                                    </div>
                                    <span className="font-medium">{feature}</span>
                                </div>
                            ))}
                        </div>

                        {/* CTA Buttons */}
                        <div className="flex flex-col sm:flex-row justify-center gap-4">
                            <button
                                onClick={onSignupClick}
                                className="group relative px-8 py-4 rounded-full bg-gradient-to-r from-[#D1B066] to-[#f4d88a] text-[#053D88] font-bold text-lg shadow-2xl hover:shadow-[#f4d88a]/50 transition-all duration-300 hover:scale-105 overflow-hidden"
                            >
                                <span className="relative z-10">Đăng ký miễn phí ngay</span>
                                <div className="absolute inset-0 bg-gradient-to-r from-[#f4d88a] to-[#D1B066] opacity-0 group-hover:opacity-100 transition-opacity duration-300" />
                            </button>

                            <button
                                onClick={onLoginClick}
                                className="px-8 py-4 rounded-full border-2 border-[#053D88]/30 text-[#053D88] font-semibold text-lg backdrop-blur-sm hover:bg-[#053D88]/10 transition-all duration-300 hover:scale-105"
                            >
                                Tìm hiểu thêm
                            </button>
                        </div>
                    </div>

                    {/* Stats */}
                    <div className="grid grid-cols-3 gap-8 pt-12 border-t border-[#053D88]/20">
                        <div className="text-center">
                            <div className="text-3xl lg:text-4xl font-bold text-transparent bg-clip-text bg-gradient-to-r from-[#D1B066] to-[#f4d88a] mb-2">1000+</div>
                            <div className="text-[#074aa8] text-sm lg:text-base">Gia phả đã tạo</div>
                        </div>
                        <div className="text-center">
                            <div className="text-3xl lg:text-4xl font-bold text-transparent bg-clip-text bg-gradient-to-r from-[#D1B066] to-[#f4d88a] mb-2">10K+</div>
                            <div className="text-[#074aa8] text-sm lg:text-base">Người dùng tin tưởng</div>
                        </div>
                        <div className="text-center">
                            <div className="text-3xl lg:text-4xl font-bold text-transparent bg-clip-text bg-gradient-to-r from-[#D1B066] to-[#f4d88a] mb-2">50K+</div>
                            <div className="text-[#074aa8] text-sm lg:text-base">Thành viên đã thêm</div>
                        </div>
                    </div>
                </div>
            </div>

            <style>{`
                @keyframes float {
                    0%, 100% { transform: translateY(0) translateX(0); opacity: 0; }
                    10% { opacity: 1; }
                    90% { opacity: 1; }
                    100% { transform: translateY(-100vh) translateX(30px); opacity: 0; }
                }
                @keyframes pulse-slow {
                    0%, 100% { opacity: 0.3; }
                    50% { opacity: 0.5; }
                }
                .animate-float { animation: float linear infinite; }
                .animate-pulse-slow { animation: pulse-slow 4s ease-in-out infinite; }
                .animation-delay-1000 { animation-delay: 1s; }
            `}</style>
        </section>
    );
}