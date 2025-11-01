// src/components/home/TestimonialsSection.tsx
import { Quote } from 'lucide-react';

const testimonials = [
    {
        name: 'Nguyễn Văn Minh',
        role: 'Hà Nội',
        content:
            'LegacyMap giúp gia đình tôi kết nối lại với nhiều người họ hàng xa. Một trải nghiệm thật ý nghĩa và xúc động.',
    },
    {
        name: 'Trần Thị Lan',
        role: 'TP. Hồ Chí Minh',
        content:
            'Giao diện rất thân thiện, dễ dùng. Tôi có thể dễ dàng tạo cây gia phả cho cả họ nội và họ ngoại.',
    },
    {
        name: 'Phạm Quốc Anh',
        role: 'Đà Nẵng',
        content:
            'Rất thích tính năng chia sẻ và lưu giữ hình ảnh, như một album kỹ thuật số của gia đình.',
    },
];

export default function TestimonialsSection() {
    return (
        <section className="relative py-24 bg-gradient-to-b from-[#2a3548] via-[#1a2332] to-[#2a3548] text-white overflow-hidden">
            {/* Ambient glow effects */}
            <div className="absolute top-0 left-1/2 -translate-x-1/2 w-[1000px] h-[500px] bg-gradient-radial from-[#ffd89b]/10 via-[#d4af7a]/5 to-transparent blur-[100px] rounded-full pointer-events-none animate-pulse-slow" />
            <div className="absolute bottom-0 left-1/4 w-[800px] h-[400px] bg-gradient-radial from-[#f5e6d3]/8 via-[#d4af7a]/4 to-transparent blur-[120px] rounded-full pointer-events-none animate-pulse-slow animation-delay-2000" />

            {/* Noise texture */}
            <div className="absolute inset-0 opacity-[0.02] pointer-events-none mix-blend-overlay bg-[url('data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIzMDAiIGhlaWdodD0iMzAwIj48ZmlsdGVyIGlkPSJhIj48ZmVUdXJidWxlbmNlIGJhc2VGcmVxdWVuY3k9Ii43NSIgc3RpdGNoVGlsZXM9InN0aXRjaCIgdHlwZT0iZnJhY3RhbE5vaXNlIi8+PGZlQ29sb3JNYXRyaXggdHlwZT0ic2F0dXJhdGUiIHZhbHVlcz0iMCIvPjwvZmlsdGVyPjxyZWN0IHdpZHRoPSIxMDAlIiBoZWlnaHQ9IjEwMCUiIGZpbHRlcj0idXJsKCNhKSIvPjwvc3ZnPg==')]" />

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

            <div className="relative max-w-7xl mx-auto px-6 z-10">
                {/* Header */}
                <div className="text-center mb-16">
                    <h2 className="text-3xl md:text-4xl lg:text-5xl font-black mb-6 relative inline-block">
                        <span className="absolute inset-0 blur-lg opacity-20" style={{
                            background: 'linear-gradient(to right, #ffd89b, #f5e6d3, #d4af7a)',
                            WebkitBackgroundClip: 'text',
                            WebkitTextFillColor: 'transparent',
                            backgroundClip: 'text'
                        }}>
                            Hành trình lưu giữ ký ức
                        </span>
                        <span className="relative" style={{
                            color: '#ffd89b',
                            textShadow: '0 0 15px rgba(255,216,155,0.3), 0 0 8px rgba(255,216,155,0.25), 0 2px 4px rgba(0,0,0,0.5)'
                        }}>
                            Hành trình lưu giữ ký ức
                        </span>
                    </h2>

                    <p className="text-gray-200 max-w-2xl mx-auto mb-8 text-lg leading-relaxed" style={{
                        textShadow: '0 1px 4px rgba(0,0,0,0.4)'
                    }}>
                        Những chia sẻ chân thành từ cộng đồng người dùng LegacyMap trên khắp Việt Nam.
                    </p>
                </div>

                {/* Testimonials grid */}
                <div className="grid md:grid-cols-3 gap-8">
                    {testimonials.map((t, i) => (
                        <div
                            key={i}
                            className="group relative p-8 rounded-2xl bg-gradient-to-br from-white/10 via-white/5 to-white/[0.02] backdrop-blur-xl border border-white/10 shadow-[0_8px_30px_rgba(0,0,0,0.4)] hover:shadow-[0_12px_40px_rgba(255,216,155,0.2)] transition-all duration-500 hover:-translate-y-2 hover:border-[#ffd89b]/30"
                        >
                            {/* Gradient glow on hover */}
                            <div className="absolute inset-0 bg-gradient-to-br from-[#ffd89b]/5 to-[#d4af7a]/5 opacity-0 group-hover:opacity-100 rounded-2xl transition-opacity duration-500" />

                            {/* Quote icon with glow */}
                            <div className="relative flex justify-center mb-6">
                                <div className="relative">
                                    {/* Glow effect behind icon */}
                                    <div className="absolute inset-0 bg-gradient-to-br from-[#ffd89b] to-[#d4af7a] rounded-full blur-xl opacity-40 group-hover:opacity-60 transition-opacity" />

                                    <div className="relative w-16 h-16 rounded-full bg-gradient-to-br from-[#d4af7a] to-[#ffd89b] flex items-center justify-center shadow-[0_8px_20px_rgba(255,216,155,0.3)] group-hover:scale-110 transition-transform duration-500">
                                        <Quote className="w-8 h-8 text-[#0f1419]" strokeWidth={2.5} />
                                    </div>
                                </div>
                            </div>

                            {/* Content */}
                            <p className="relative text-gray-300 mb-6 leading-relaxed italic text-[15px] text-center">
                                "{t.content}"
                            </p>

                            {/* Author info */}
                            <div className="relative text-center">
                                <div className="font-bold text-[#ffd89b] group-hover:text-[#f5e6d3] transition-colors text-lg">
                                    {t.name}
                                </div>
                                <div className="text-sm text-gray-400 mt-1">
                                    {t.role}
                                </div>
                            </div>
                        </div>
                    ))}
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
                
                .animation-delay-2000 {
                    animation-delay: 2s;
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