// src/components/home/HeroSection.tsx
import { useEffect, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import Button from '@/components/layout/Button';
import RongPhuong from '@/assets/Rong_Phuong.jpg';

interface HeroSectionProps {
    onSignupClick: () => void;
}

export default function HeroSection({ onSignupClick }: HeroSectionProps) {
    const [isVisible, setIsVisible] = useState(false);
    const [mousePos, setMousePos] = useState({ x: 0, y: 0 });
    const [isAuthenticated, setIsAuthenticated] = useState(false);
    const [user, setUser] = useState<any>(null);
    const navigate = useNavigate();

    useEffect(() => {
        setIsVisible(true);

        // Check authentication status
        const checkAuth = () => {
            const token = localStorage.getItem('authToken');
            const userStr = localStorage.getItem('user');
            const isAuth = !!(token && userStr);
            setIsAuthenticated(isAuth);

            if (isAuth && userStr) {
                try {
                    const userData = JSON.parse(userStr);
                    setUser(userData);
                } catch {
                    setUser(null);
                }
            } else {
                setUser(null);
            }
        };

        checkAuth();
        window.addEventListener('storage', checkAuth);

        const handleMouseMove = (e: MouseEvent) => {
            setMousePos({
                x: (e.clientX / window.innerWidth) * 100,
                y: (e.clientY / window.innerHeight) * 100
            });
        };

        window.addEventListener('mousemove', handleMouseMove);
        return () => {
            window.removeEventListener('mousemove', handleMouseMove);
            window.removeEventListener('storage', checkAuth);
        };
    }, []);

    const isAdmin = () => {
        if (!user) return false;
        return user.roleName === 'admin' || user.role === 'admin';
    };

    const getDashboardUrl = () => {
        return isAdmin() ? '/admin/dashboard' : '/dashboard';
    };

    const handlePrimaryClick = () => {
        if (isAuthenticated) {
            navigate(getDashboardUrl());
        } else {
            onSignupClick();
        }
    };

    const getButtonText = () => {
        if (!isAuthenticated) return 'Bắt đầu miễn phí';
        return isAdmin() ? 'Quản lý hệ thống' : 'Bắt đầu';
    };

    return (
        <header className="relative overflow-hidden bg-gradient-to-br from-[#0f1419] via-[#1e2a3a] to-[#0f1419] text-white min-h-screen flex items-center">
            {/* Animated gradient mesh background */}
            <div
                className="absolute inset-0 opacity-30"
                style={{
                    background: `radial-gradient(circle at ${mousePos.x}% ${mousePos.y}%, #ffd89b33 0%, transparent 50%)`
                }}
            />

            {/* Enhanced noise texture */}
            <div className="absolute inset-0 opacity-[0.025] pointer-events-none mix-blend-overlay bg-[url('data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIzMDAiIGhlaWdodD0iMzAwIj48ZmlsdGVyIGlkPSJhIj48ZmVUdXJidWxlbmNlIGJhc2VGcmVxdWVuY3k9Ii43NSIgc3RpdGNoVGlsZXM9InN0aXRjaCIgdHlwZT0iZnJhY3RhbE5vaXNlIi8+PGZlQ29sb3JNYXRyaXggdHlwZT0ic2F0dXJhdGUiIHZhbHVlcz0iMCIvPjwvZmlsdGVyPjxyZWN0IHdpZHRoPSIxMDAlIiBoZWlnaHQ9IjEwMCUiIGZpbHRlcj0idXJsKCNhKSIvPjwvc3ZnPg==')]" />

            {/* Multi-layered ambient glows with better composition - reduced intensity */}
            <div className="absolute top-0 left-1/2 -translate-x-1/2 w-[1400px] h-[700px] bg-gradient-radial from-[#ffd89b]/15 via-[#d4af7a]/8 to-transparent blur-[140px] rounded-full pointer-events-none animate-pulse-slow" />
            <div className="absolute top-1/3 right-0 w-[1000px] h-[600px] bg-gradient-radial from-[#f5e6d3]/12 via-[#d4af7a]/6 to-transparent blur-[120px] rounded-full pointer-events-none animate-pulse-slow animation-delay-1500" />
            <div className="absolute bottom-0 left-0 w-[900px] h-[550px] bg-gradient-radial from-[#ffd89b]/15 via-[#d4af7a]/5 to-transparent blur-[130px] rounded-full pointer-events-none animate-pulse-slow animation-delay-3000" />

            {/* Spotlight effect from top - reduced */}
            <div className="absolute top-0 left-1/2 -translate-x-1/2 w-full h-[800px] bg-gradient-to-b from-[#ffd89b]/5 via-[#ffd89b]/2 to-transparent pointer-events-none" />

            {/* Dynamic light rays */}
            {[...Array(5)].map((_, i) => (
                <div
                    key={i}
                    className="absolute top-0 w-[1px] h-full opacity-20 pointer-events-none animate-shimmer"
                    style={{
                        left: `${20 + i * 15}%`,
                        background: `linear-gradient(to bottom, #ffd89b ${Math.random() * 20}%, transparent ${50 + Math.random() * 30}%)`,
                        animationDelay: `${i * 0.5}s`,
                    }}
                />
            ))}

            {/* Enhanced floating particles with depth */}
            <div className="absolute inset-0 overflow-hidden pointer-events-none">
                {[...Array(30)].map((_, i) => {
                    const size = Math.random() * 3 + 1;
                    const depth = Math.random();
                    return (
                        <div
                            key={i}
                            className="absolute rounded-full animate-float"
                            style={{
                                width: `${size}px`,
                                height: `${size}px`,
                                left: `${Math.random() * 100}%`,
                                top: `${Math.random() * 100}%`,
                                background: i % 3 === 0 ? '#ffd89b' : i % 3 === 1 ? '#d4af7a' : '#f5e6d3',
                                opacity: depth * 0.5 + 0.1,
                                boxShadow: `0 0 ${size * 2}px currentColor`,
                                animationDelay: `${Math.random() * 10}s`,
                                animationDuration: `${15 + Math.random() * 20}s`,
                                filter: `blur(${depth * 1.5}px)`,
                            }}
                        />
                    );
                })}
            </div>

            {/* Decorative dragon-phoenix inspired patterns */}
            <div className="absolute top-8 left-8 w-80 h-80 opacity-[0.08] pointer-events-none">
                <svg viewBox="0 0 200 200" className="w-full h-full animate-spin-slow">
                    <circle cx="100" cy="100" r="80" stroke="url(#gold-gradient)" strokeWidth="0.5" fill="none" strokeDasharray="5,5"/>
                    <circle cx="100" cy="100" r="60" stroke="url(#gold-gradient)" strokeWidth="0.3" fill="none" strokeDasharray="3,3"/>
                    <circle cx="100" cy="100" r="40" stroke="url(#gold-gradient)" strokeWidth="0.5" fill="none"/>
                    <defs>
                        <linearGradient id="gold-gradient" x1="0%" y1="0%" x2="100%" y2="100%">
                            <stop offset="0%" stopColor="#ffd89b"/>
                            <stop offset="50%" stopColor="#f5e6d3"/>
                            <stop offset="100%" stopColor="#d4af7a"/>
                        </linearGradient>
                    </defs>
                </svg>
            </div>

            <div className="absolute bottom-8 right-8 w-80 h-80 opacity-[0.08] pointer-events-none">
                <svg viewBox="0 0 200 200" className="w-full h-full animate-spin-reverse">
                    <path d="M100,20 Q120,50 100,80 T100,140 Q120,170 100,180" stroke="url(#gold-gradient2)" strokeWidth="0.5" fill="none"/>
                    <path d="M80,20 Q60,50 80,80 T80,140 Q60,170 80,180" stroke="url(#gold-gradient2)" strokeWidth="0.5" fill="none"/>
                    <defs>
                        <linearGradient id="gold-gradient2" x1="0%" y1="0%" x2="0%" y2="100%">
                            <stop offset="0%" stopColor="#ffd89b"/>
                            <stop offset="100%" stopColor="#d4af7a"/>
                        </linearGradient>
                    </defs>
                </svg>
            </div>

            {/* Content */}
            <div className="relative max-w-7xl mx-auto px-6 py-20 lg:py-24 w-full">
                <div className="grid lg:grid-cols-2 gap-16 items-center">
                    {/* LEFT: TEXT */}
                    <div
                        className={`transition-all duration-1200 ${
                            isVisible
                                ? 'opacity-100 translate-y-0'
                                : 'opacity-0 translate-y-12'
                        }`}
                    >
                        <h1 className="text-4xl lg:text-5xl xl:text-6xl font-black leading-[1.3] tracking-tight">
                            <span className="block animate-fade-in-up text-white mb-2" style={{ fontWeight: 900 }}>Xây dựng</span>
                            <span
                                className="block animate-fade-in-up animation-delay-200 mb-2 relative"
                                style={{ fontWeight: 900 }}
                            >
                                <span className="absolute inset-0 blur-lg opacity-20" style={{
                                    background: 'linear-gradient(to right, #ffd89b, #f5e6d3, #d4af7a)',
                                    WebkitBackgroundClip: 'text',
                                    WebkitTextFillColor: 'transparent',
                                    backgroundClip: 'text'
                                }}>
                                    Cây Gia Phả
                                </span>
                                <span className="relative" style={{
                                    color: '#ffd89b',
                                    textShadow: '0 0 15px rgba(255,216,155,0.3), 0 0 8px rgba(255,216,155,0.25), 0 2px 4px rgba(0,0,0,0.5)'
                                }}>
                                    Cây Gia Phả
                                </span>
                            </span>
                            <span className="block animate-fade-in-up animation-delay-400 text-white mb-2" style={{ fontWeight: 900 }}>
                                trực quan, kết nối
                            </span>
                            <span
                                className="block animate-fade-in-up animation-delay-600 relative"
                                style={{ fontWeight: 900 }}
                            >
                                <span className="absolute inset-0 blur-lg opacity-20" style={{
                                    background: 'linear-gradient(to right, #ffd89b, #f5e6d3, #d4af7a)',
                                    WebkitBackgroundClip: 'text',
                                    WebkitTextFillColor: 'transparent',
                                    backgroundClip: 'text'
                                }}>
                                    Con Rồng Cháu Tiên
                                </span>
                                <span className="relative" style={{
                                    color: '#ffd89b',
                                    textShadow: '0 0 15px rgba(255,216,155,0.3), 0 0 8px rgba(255,216,155,0.25), 0 2px 4px rgba(0,0,0,0.5)'
                                }}>
                                    Con Rồng Cháu Tiên
                                </span>
                            </span>
                        </h1>

                        <p className="mt-8 text-gray-200 text-lg lg:text-xl leading-relaxed animate-fade-in-up animation-delay-800 max-w-2xl font-medium"
                           style={{
                               textShadow: '0 1px 4px rgba(0,0,0,0.4)',
                               fontWeight: 500
                           }}>
                            Hệ thống quản lý cây gia phả hiện đại, giúp bạn ghi nhận, lưu trữ
                            và chia sẻ lịch sử gia đình qua nhiều thế hệ.
                        </p>

                        <div className="mt-10 flex flex-wrap gap-5 animate-fade-in-up animation-delay-900">
                            <Button
                                variant="primary"
                                size="lg"
                                onClick={handlePrimaryClick}
                                className="relative overflow-hidden bg-gradient-to-r from-[#d4af7a] via-[#ffd89b] to-[#d4af7a] bg-[length:200%_100%] text-[#0f1419] font-bold shadow-[0_20px_50px_rgba(255,216,155,0.5),0_0_80px_rgba(212,175,122,0.4),inset_0_1px_0_rgba(255,255,255,0.3)] hover:shadow-[0_25px_60px_rgba(255,216,155,0.7),0_0_100px_rgba(212,175,122,0.5)] transition-all duration-500 hover:scale-110 active:scale-105 border-2 border-[#ffd89b]/50 group px-10 py-4 text-lg"
                            >
                                <span className="relative z-10 flex items-center gap-2">
                                    {getButtonText()}
                                    <svg className="w-5 h-5 group-hover:translate-x-1 transition-transform" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                                        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M13 7l5 5m0 0l-5 5m5-5H6" />
                                    </svg>
                                </span>
                                <div className="absolute inset-0 -translate-x-full group-hover:translate-x-full transition-transform duration-1000 bg-gradient-to-r from-transparent via-white/40 to-transparent skew-x-12" />
                                <div className="absolute inset-0 opacity-0 group-hover:opacity-100 transition-opacity duration-500 bg-gradient-to-r from-[#ffd89b] to-[#d4af7a] blur-xl" />
                            </Button>

                            <a href="#features">
                                <Button
                                    variant="outline"
                                    size="lg"
                                    className="border-2 border-[#ffd89b]/60 hover:bg-[#ffd89b]/10 hover:border-[#ffd89b] backdrop-blur-xl bg-white/5 transition-all duration-500 hover:scale-110 active:scale-105 shadow-[0_8px_30px_rgba(255,216,155,0.15)] hover:shadow-[0_12px_40px_rgba(255,216,155,0.3)] px-10 py-4 text-lg group [&]:!text-[#ffd89b] [&_*]:!text-[#ffd89b]"
                                >
                                    <span className="flex items-center gap-2">
                                        Xem tính năng
                                        <svg className="w-5 h-5 group-hover:translate-y-1 transition-transform" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                                            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M19 9l-7 7-7-7" />
                                        </svg>
                                    </span>
                                </Button>
                            </a>
                        </div>
                    </div>

                    {/* RIGHT: IMAGE */}
                    <div
                        className={`transition-all duration-1200 delay-300 ${
                            isVisible
                                ? 'opacity-100 translate-x-0'
                                : 'opacity-0 translate-x-12'
                        }`}
                    >
                        <div className="relative group perspective-1000">
                            {/* Soft glow effect around image */}
                            <div className="absolute -inset-6 bg-gradient-to-r from-[#d4af7a] via-[#ffd89b] to-[#d4af7a] rounded-3xl blur-[40px] opacity-30 animate-pulse-slow" />

                            <div className="relative bg-gradient-to-br from-white/15 via-white/10 to-white/5 backdrop-blur-2xl p-10 rounded-[2rem] shadow-[0_50px_100px_rgba(0,0,0,0.5)] border-2 border-white/20">

                                {/* Image container with enhanced effects */}
                                <div className="relative rounded-2xl overflow-hidden ring-1 ring-[#ffd89b]/35 shadow-[0_0_25px_rgba(255,216,155,0.2),inset_0_0_18px_rgba(255,216,155,0.12)]">
                                    <img
                                        src={RongPhuong}
                                        alt="Rồng - Phượng (Rồng phượng tương giao)"
                                        className="w-full h-full object-cover brightness-110 contrast-110 saturate-110"
                                        loading="lazy"
                                    />
                                    {/* Sophisticated gradient overlays */}
                                    <div className="absolute inset-0 bg-gradient-to-t from-[#0f1419]/70 via-transparent to-[#ffd89b]/15 opacity-60" />
                                    <div className="absolute inset-0 bg-gradient-to-br from-[#ffd89b]/10 via-transparent to-[#d4af7a]/10 opacity-40" />
                                    {/* Enhanced vignette */}
                                    <div className="absolute inset-0 shadow-[inset_0_0_120px_rgba(0,0,0,0.5)]" />
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>

            {/* Enhanced Animations */}
            <style>{`
                .perspective-1000 {
                    perspective: 1000px;
                }
                
                @keyframes float {
                    0%, 100% { 
                        transform: translateY(0) translateX(0) scale(1) rotate(0deg); 
                        opacity: 0; 
                    }
                    10% { opacity: 1; }
                    50% { 
                        transform: translateY(-50vh) translateX(15px) scale(1.3) rotate(180deg);
                        opacity: 0.9;
                    }
                    90% { opacity: 0.5; }
                    100% { 
                        transform: translateY(-100vh) translateX(30px) scale(0.7) rotate(360deg); 
                        opacity: 0; 
                    }
                }
                
                @keyframes float-slow {
                    0%, 100% { 
                        transform: translateY(0) scale(1); 
                        opacity: 0.6; 
                    }
                    50% { 
                        transform: translateY(-20px) scale(1.2);
                        opacity: 1;
                    }
                }
                
                @keyframes fade-in-up {
                    from { 
                        opacity: 0; 
                        transform: translateY(40px); 
                    }
                    to { 
                        opacity: 1; 
                        transform: translateY(0); 
                    }
                }
                
                @keyframes pulse-slow {
                    0%, 100% { 
                        opacity: 0.4;
                        transform: scale(1);
                    }
                    50% { 
                        opacity: 0.6;
                        transform: scale(1.08);
                    }
                }
                
                @keyframes pulse-fast {
                    0%, 100% { 
                        opacity: 1;
                        transform: scale(1);
                    }
                    50% { 
                        opacity: 0.5;
                        transform: scale(0.9);
                    }
                }
                
                @keyframes shimmer {
                    0% { 
                        opacity: 0.1;
                        transform: translateY(0);
                    }
                    50% {
                        opacity: 0.3;
                        transform: translateY(-20px);
                    }
                    100% { 
                        opacity: 0.1;
                        transform: translateY(0);
                    }
                }
                
                @keyframes spin-slow {
                    from { transform: rotate(0deg); }
                    to { transform: rotate(360deg); }
                }
                
                @keyframes spin-reverse {
                    from { transform: rotate(360deg); }
                    to { transform: rotate(0deg); }
                }
                
                @keyframes shine {
                    0% { 
                        transform: translateX(-100%) translateY(-100%) rotate(45deg); 
                    }
                    100% { 
                        transform: translateX(100%) translateY(100%) rotate(45deg); 
                    }
                }
                
                .animate-float { 
                    animation: float linear infinite; 
                }
                .animate-float-slow { 
                    animation: float-slow 4s ease-in-out infinite; 
                }
                .animate-fade-in-up { 
                    animation: fade-in-up 1.2s cubic-bezier(0.16, 1, 0.3, 1) forwards; 
                    opacity: 0;
                }
                .animate-pulse-slow {
                    animation: pulse-slow 5s ease-in-out infinite;
                }
                .animate-pulse-fast {
                    animation: pulse-fast 2s ease-in-out infinite;
                }
                .animate-shimmer {
                    animation: shimmer 3s ease-in-out infinite;
                }
                .animate-spin-slow {
                    animation: spin-slow 30s linear infinite;
                }
                .animate-spin-reverse {
                    animation: spin-reverse 25s linear infinite;
                }
                .animate-shine {
                    animation: shine 2s ease-in-out;
                }
                
                .animation-delay-200 { animation-delay: 0.2s; }
                .animation-delay-400 { animation-delay: 0.4s; }
                .animation-delay-600 { animation-delay: 0.6s; }
                .animation-delay-800 { animation-delay: 0.8s; }
                .animation-delay-900 { animation-delay: 0.9s; }
                .animation-delay-1000 { animation-delay: 1s; }
                .animation-delay-1500 { animation-delay: 1.5s; }
                .animation-delay-2000 { animation-delay: 2s; }
                .animation-delay-3000 { animation-delay: 3s; }
                .bg-gradient-radial {
                    background: radial-gradient(circle, var(--tw-gradient-stops));
                }
            `}</style>
        </header>
    );
}