// src/components/home/HeroSection.tsx
import { useEffect, useState } from 'react';
import Button from '@/components/layout/Button';
import RongPhuong from '@/assets/Rong_Phuong.jpg';

interface HeroSectionProps {
    onSignupClick: () => void;
}

export default function HeroSection({ onSignupClick }: HeroSectionProps) {
    const [isVisible, setIsVisible] = useState(false);

    useEffect(() => {
        setIsVisible(true);
    }, []);

    return (
        <header className="relative bg-gradient-to-br from-[#053D88] via-[#074aa8] to-[#053D88] text-white overflow-hidden">
            {/* Animated background pattern */}
            <div className="absolute inset-0 opacity-10">
                <div className="absolute inset-0 bg-[radial-gradient(circle_at_50%_50%,rgba(209,176,102,0.15),transparent_50%)]" />
                <div className="absolute inset-0 bg-[linear-gradient(to_right,transparent_0%,rgba(244,216,138,0.08)_50%,transparent_100%)] animate-shimmer" />
            </div>

            {/* Floating particles */}
            <div className="absolute inset-0 overflow-hidden pointer-events-none">
                {[...Array(20)].map((_, i) => (
                    <div
                        key={i}
                        className="absolute w-1 h-1 bg-[#f4d88a]/30 rounded-full animate-float"
                        style={{
                            left: `${Math.random() * 100}%`,
                            top: `${Math.random() * 100}%`,
                            animationDelay: `${Math.random() * 5}s`,
                            animationDuration: `${5 + Math.random() * 10}s`
                        }}
                    />
                ))}
            </div>

            <div className="relative max-w-7xl mx-auto px-6 py-20 lg:py-28">
                <div className="grid lg:grid-cols-2 gap-10 items-center">
                    {/* Left: Text */}
                    <div className={`transition-all duration-1000 ${isVisible ? 'opacity-100 translate-y-0' : 'opacity-0 translate-y-10'}`}>

                        <h1 className="mt-6 text-4xl lg:text-6xl font-extrabold leading-tight">
                            <span className="block animate-fade-in-up">Xây dựng</span>
                            <span className="block text-transparent bg-clip-text bg-gradient-to-r from-[#f4d88a] to-[#D1B066] animate-fade-in-up animation-delay-200">
                                Cây Gia Phả
                            </span>
                            <span className="block animate-fade-in-up animation-delay-400">trực quan, kết nối</span>
                            <span className="block text-transparent bg-clip-text bg-gradient-to-r from-[#f4d88a] to-[#D1B066] animate-fade-in-up animation-delay-600">
                                Con Rồng Cháu Tiên
                            </span>
                        </h1>

                        <p className="mt-6 text-blue-100 text-lg leading-relaxed animate-fade-in-up animation-delay-800">
                            Hệ thống quản lý cây gia phả hiện đại, giúp bạn ghi nhận, lưu trữ và chia sẻ lịch sử gia đình qua nhiều thế hệ.
                        </p>

                        <div className="mt-8 flex flex-wrap gap-4 animate-fade-in-up animation-delay-1000">
                            <Button
                                variant="primary"
                                size="lg"
                                className="bg-gradient-to-r from-[#D1B066] to-[#f4d88a] text-[#053D88] hover:from-[#f4d88a] hover:to-[#D1B066] font-semibold shadow-lg shadow-[#D1B066]/50 transition-all hover:scale-105 hover:shadow-xl hover:shadow-[#f4d88a]/50"
                                onClick={onSignupClick}
                            >
                                Bắt đầu miễn phí
                            </Button>
                            <a href="#features">
                                <Button
                                    variant="outline"
                                    size="lg"
                                    className="border-2 border-white/30 text-white hover:bg-white/10 backdrop-blur-sm transition-all hover:scale-105"
                                >
                                    Xem tính năng
                                </Button>
                            </a>
                        </div>
                    </div>

                    {/* Right: Image card with parallax effect */}
                    <div className={`transition-all duration-1000 delay-300 ${isVisible ? 'opacity-100 translate-x-0' : 'opacity-0 translate-x-10'}`}>
                        <div className="relative group">
                            {/* Glow effect */}
                            <div className="absolute -inset-1 bg-gradient-to-r from-[#D1B066] to-[#f4d88a] rounded-3xl blur-2xl opacity-30 group-hover:opacity-50 transition-opacity" />

                            <div className="relative bg-white/95 backdrop-blur-sm p-6 rounded-3xl shadow-2xl transform group-hover:scale-[1.02] transition-transform duration-500">
                                <div className="relative rounded-2xl overflow-hidden ring-1 ring-gray-200">
                                    <img
                                        src={RongPhuong}
                                        alt="Rồng - Phượng (Rồng phượng tương giao)"
                                        className="w-full h-full object-cover transform group-hover:scale-110 transition-transform duration-700"
                                        loading="lazy"
                                    />
                                    {/* Overlay gradient */}
                                    <div className="absolute inset-0 bg-gradient-to-t from-[#053D88]/20 to-transparent opacity-0 group-hover:opacity-100 transition-opacity duration-500" />
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>

            <style>{`
                @keyframes shimmer {
                    0% { transform: translateX(-100%); }
                    100% { transform: translateX(100%); }
                }
                @keyframes float {
                    0%, 100% { transform: translateY(0) translateX(0); opacity: 0; }
                    10% { opacity: 1; }
                    90% { opacity: 1; }
                    100% { transform: translateY(-100vh) translateX(20px); opacity: 0; }
                }
                @keyframes fade-in-up {
                    from { opacity: 0; transform: translateY(20px); }
                    to { opacity: 1; transform: translateY(0); }
                }
                @keyframes pulse-slow {
                    0%, 100% { opacity: 1; }
                    50% { opacity: 0.8; }
                }
                .animate-shimmer { animation: shimmer 8s infinite; }
                .animate-float { animation: float linear infinite; }
                .animate-fade-in-up { animation: fade-in-up 0.8s ease-out forwards; }
                .animate-pulse-slow { animation: pulse-slow 3s ease-in-out infinite; }
                .animation-delay-200 { animation-delay: 0.2s; }
                .animation-delay-400 { animation-delay: 0.4s; }
                .animation-delay-600 { animation-delay: 0.6s; }
                .animation-delay-800 { animation-delay: 0.8s; }
                .animation-delay-1000 { animation-delay: 1s; }
            `}</style>
        </header>
    );
}