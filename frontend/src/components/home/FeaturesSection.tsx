// src/components/home/FeaturesSection.tsx
import { Lightbulb, Users, ShieldCheck, Database } from 'lucide-react';

const features = [
    {
        icon: Lightbulb,
        title: 'Dễ sử dụng',
        description:
            'Giao diện trực quan, giúp bạn dễ dàng tạo và quản lý cây gia phả chỉ với vài thao tác đơn giản.',
    },
    {
        icon: Users,
        title: 'Kết nối gia đình',
        description:
            'Kết nối các thành viên trong họ hàng, chia sẻ thông tin và kỷ niệm một cách sinh động và ý nghĩa.',
    },
    {
        icon: ShieldCheck,
        title: 'Bảo mật thông tin',
        description:
            'Dữ liệu của bạn được bảo vệ bằng các công nghệ mã hóa tiên tiến và tuân thủ tiêu chuẩn bảo mật cao nhất.',
    },
    {
        icon: Database,
        title: 'Lưu trữ lâu dài',
        description:
            'Dữ liệu được lưu trữ ổn định, đảm bảo lịch sử gia đình bạn được giữ vững qua nhiều thế hệ.',
    },
];

export default function FeaturesSection() {
    return (
        <section id="features" className="py-24 bg-gradient-to-b from-[#2a3548] via-[#1a2332] to-[#2a3548] relative overflow-hidden">
            {/* Ambient glow effects */}
            <div className="absolute top-0 left-1/2 -translate-x-1/2 w-[1000px] h-[500px] bg-gradient-radial from-[#ffd89b]/10 via-[#d4af7a]/5 to-transparent blur-[100px] rounded-full pointer-events-none animate-pulse-slow" />

            {/* Noise texture */}
            <div className="absolute inset-0 opacity-[0.02] pointer-events-none mix-blend-overlay bg-[url('data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIzMDAiIGhlaWdodD0iMzAwIj48ZmlsdGVyIGlkPSJhIj48ZmVUdXJidWxlbmNlIGJhc2VGcmVxdWVuY3k9Ii43NSIgc3RpdGNoVGlsZXM9InN0aXRjaCIgdHlwZT0iZnJhY3RhbE5vaXNlIi8+PGZlQ29sb3JNYXRyaXggdHlwZT0ic2F0dXJhdGUiIHZhbHVlcz0iMCIvPjwvZmlsdGVyPjxyZWN0IHdpZHRoPSIxMDAlIiBoZWlnaHQ9IjEwMCUiIGZpbHRlcj0idXJsKCNhKSIvPjwvc3ZnPg==')]" />

            {/* Enhanced floating particles with depth */}
            <div className="absolute inset-0 overflow-hidden pointer-events-none">
                {[...Array(25)].map((_, i) => {
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

            <div className="max-w-7xl mx-auto px-6 text-center relative z-10">
                <h2 className="text-3xl md:text-4xl lg:text-5xl font-black mb-6 relative inline-block">
                    <span className="absolute inset-0 blur-lg opacity-20" style={{
                        background: 'linear-gradient(to right, #ffd89b, #f5e6d3, #d4af7a)',
                        WebkitBackgroundClip: 'text',
                        WebkitTextFillColor: 'transparent',
                        backgroundClip: 'text'
                    }}>
                        Tính năng nổi bật
                    </span>
                    <span className="relative" style={{
                        color: '#ffd89b',
                        textShadow: '0 0 15px rgba(255,216,155,0.3), 0 0 8px rgba(255,216,155,0.25), 0 2px 4px rgba(0,0,0,0.5)'
                    }}>
                        Tính năng nổi bật
                    </span>
                </h2>

                <p className="text-gray-200 max-w-2xl mx-auto mb-16 text-lg leading-relaxed" style={{
                    textShadow: '0 1px 4px rgba(0,0,0,0.4)'
                }}>
                    Khám phá những chức năng mạnh mẽ giúp bạn quản lý, lưu trữ và kết nối
                    thông tin gia đình hiệu quả hơn.
                </p>

                <div className="grid md:grid-cols-2 lg:grid-cols-4 gap-8">
                    {features.map((f, i) => {
                        const Icon = f.icon;
                        return (
                            <div
                                key={i}
                                className="group relative p-8 rounded-2xl bg-gradient-to-br from-white/10 via-white/5 to-white/[0.02] backdrop-blur-xl border border-white/10 shadow-[0_8px_30px_rgba(0,0,0,0.4)] hover:shadow-[0_12px_40px_rgba(255,216,155,0.2)] transition-all duration-500 hover:-translate-y-2 hover:border-[#ffd89b]/30"
                            >
                                {/* Gradient glow on hover */}
                                <div className="absolute inset-0 bg-gradient-to-br from-[#ffd89b]/5 to-[#d4af7a]/5 opacity-0 group-hover:opacity-100 rounded-2xl transition-opacity duration-500" />

                                {/* Icon container */}
                                <div className="relative flex justify-center mb-6">
                                    <div className="relative">
                                        {/* Glow effect behind icon */}
                                        <div className="absolute inset-0 bg-gradient-to-br from-[#ffd89b] to-[#d4af7a] rounded-full blur-xl opacity-40 group-hover:opacity-60 transition-opacity" />

                                        <div className="relative w-16 h-16 rounded-full bg-gradient-to-br from-[#d4af7a] to-[#ffd89b] flex items-center justify-center shadow-[0_8px_20px_rgba(255,216,155,0.3)] group-hover:scale-110 transition-transform duration-500">
                                            <Icon className="w-8 h-8 text-[#0f1419]" strokeWidth={2.5} />
                                        </div>
                                    </div>
                                </div>

                                <h3 className="relative text-xl font-bold mb-3 text-[#ffd89b] group-hover:text-[#f5e6d3] transition-colors">
                                    {f.title}
                                </h3>
                                <p className="relative text-gray-300 leading-relaxed text-[15px]">
                                    {f.description}
                                </p>
                            </div>
                        );
                    })}
                </div>
            </div>

            {/* Bottom glow effect */}
            <div className="absolute bottom-0 left-1/2 -translate-x-1/2 w-[1000px] h-[400px] bg-gradient-radial from-[#ffd89b]/10 via-[#d4af7a]/5 to-transparent blur-[100px] rounded-full pointer-events-none animate-pulse-slow animation-delay-3000" />

            <style>{`
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
                
                .animate-float { 
                    animation: float linear infinite; 
                }
                
                .animate-pulse-slow {
                    animation: pulse-slow 5s ease-in-out infinite;
                }
                
                .animation-delay-3000 {
                    animation-delay: 3s;
                }
                
                .bg-gradient-radial {
                    background: radial-gradient(circle, var(--tw-gradient-stops));
                }
            `}</style>
        </section>
    );
}