import { Facebook, Youtube, Mail, Phone, MapPin, Heart } from 'lucide-react';
import logo from '@/assets/logo.png';

const productLinks = [
    { label: 'Tính năng', href: '#features' },
    { label: 'Bảng giá', href: '#pricing' },
    { label: 'Dashboard', href: '/dashboard' },
];

const supportLinks = [
    { label: 'Trợ giúp', href: '#help' },
    { label: 'Chính sách bảo mật', href: '#privacy' },
    { label: 'Điều khoản sử dụng', href: '#terms' },
    { label: 'Liên hệ', href: '#contact' }
];

export default function Footer() {
    return (
        <footer id="contact" className="relative scroll-mt-16 bg-gradient-to-b from-[#2a3548] via-[#1a2332] to-[#2a3548] text-white overflow-hidden border-t border-[#f4d88a]/20">
            {/* Ambient glow effects */}
            <div className="absolute top-0 left-1/2 -translate-x-1/2 w-[1000px] h-[500px] bg-gradient-radial from-[#f4d88a]/10 via-[#D1B066]/5 to-transparent blur-[100px] rounded-full pointer-events-none animate-pulse-slow" />
            <div className="absolute bottom-0 left-1/4 w-[800px] h-[400px] bg-gradient-radial from-[#f4d88a]/8 via-[#D1B066]/4 to-transparent blur-[120px] rounded-full pointer-events-none animate-pulse-slow animation-delay-2000" />

            {/* Noise texture */}
            <div className="absolute inset-0 opacity-[0.02] pointer-events-none mix-blend-overlay bg-[url('data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIzMDAiIGhlaWdodD0iMzAwIj48ZmlsdGVyIGlkPSJhIj48ZmVUdXJidWxlbmNlIGJhc2VGcmVxdWVuY3k9Ii43NSIgc3RpdGNoVGlsZXM9InN0aXRjaCIgdHlwZT0iZnJhY3RhbE5vaXNlIi8+PGZlQ29sb3JNYXRyaXggdHlwZT0ic2F0dXJhdGUiIHZhbHVlcz0iMCIvPjwvZmlsdGVyPjxyZWN0IHdpZHRoPSIxMDAlIiBoZWlnaHQ9IjEwMCUiIGZpbHRlcj0idXJsKCNhKSIvPjwvc3ZnPg==')]" />

            {/* Enhanced floating particles with depth */}
            <div className="absolute inset-0 overflow-hidden pointer-events-none">
                {[...Array(20)].map((_, i) => {
                    const size = Math.random() * 2 + 1;
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
                                background: i % 3 === 0 ? '#f4d88a' : i % 3 === 1 ? '#D1B066' : '#ffffff',
                                opacity: depth * 0.3 + 0.1,
                                boxShadow: `0 0 ${size * 2}px currentColor`,
                                animationDelay: `${Math.random() * 10}s`,
                                animationDuration: `${20 + Math.random() * 25}s`,
                                filter: `blur(${depth * 1}px)`,
                            }}
                        />
                    );
                })}
            </div>

            <div className="relative max-w-7xl mx-auto px-6 py-10 z-10">
                <div className="grid md:grid-cols-2 lg:grid-cols-4 gap-8 mb-8 items-start">
                    {/* Brand */}
                    <div>
                        <div className="flex items-center gap-2 mb-4">
                            <div className="relative w-12 h-12 flex items-center justify-center rounded-full bg-gradient-to-br from-white/20 to-white/10 backdrop-blur-sm overflow-hidden border border-white/10 shadow-[0_8px_20px_rgba(0,0,0,0.3)]">
                                {/* Reduced glow effect behind logo */}
                                <div className="absolute inset-0 bg-gradient-to-br from-[#f4d88a] to-[#D1B066] rounded-full blur-md opacity-20" />
                                <img
                                    src={logo}
                                    alt="Logo"
                                    className="relative w-10 h-10 object-cover z-10 filter brightness-90 contrast-110"
                                    style={{
                                        filter: 'brightness(0.9) contrast(1.1) saturate(0.8)'
                                    }}
                                />
                            </div>
                            <div>
                                <div className="font-bold text-xl text-white">Cây Gia Phả</div>
                                <div className="text-sm text-gray-300">Con Rồng Cháu Tiên</div>
                            </div>
                        </div>
                        <p className="text-gray-300 leading-relaxed mb-6" style={{
                            textShadow: '0 1px 2px rgba(0,0,0,0.3)'
                        }}>
                            Hệ thống quản lý cây gia phả hiện đại, giúp lưu giữ và tôn vinh truyền thống gia đình Việt Nam qua nhiều thế hệ.
                        </p>
                        <div className="flex gap-3">
                            {[
                                { icon: Facebook, href: '#' },
                                { icon: Youtube, href: '#' },
                                { icon: Mail, href: '#' },
                            ].map(({ icon: Icon, href }, i) => (
                                <a
                                    key={i}
                                    href={href}
                                    className="relative w-10 h-10 rounded-full bg-white/10 backdrop-blur-sm border border-white/10 hover:bg-[#f4d88a]/20 flex items-center justify-center transition-all duration-300 hover:scale-110 hover:shadow-[0_0_20px_rgba(244,216,138,0.3)] hover:border-[#f4d88a]/30 group"
                                >
                                    <Icon className="w-5 h-5 text-gray-300 group-hover:text-[#f4d88a] group-hover:scale-110 transition-transform" />
                                </a>
                            ))}
                        </div>
                    </div>

                    {/* Products */}
                    <div>
                        <h3 className="font-bold text-lg leading-tight mt-0 mb-4 text-[#f4d88a] relative inline-block">
                            <span className="relative" style={{
                                textShadow: '0 0 10px rgba(244,216,138,0.3), 0 2px 4px rgba(0,0,0,0.5)'
                            }}>
                                Sản phẩm
                            </span>
                        </h3>
                        <ul className="space-y-3 list-none pl-0">
                            {productLinks.map((link) => (
                                <li key={link.label}>
                                    <a href={link.href} className="text-gray-300 hover:text-[#f4d88a] transition-all duration-300 no-underline hover:translate-x-1 hover:underline underline-offset-4 block" style={{
                                        textShadow: '0 1px 2px rgba(0,0,0,0.3)'
                                    }}>
                                        {link.label}
                                    </a>
                                </li>
                            ))}
                        </ul>
                    </div>

                    {/* Support */}
                    <div>
                        <h3 className="font-bold text-lg leading-tight mt-0 mb-4 text-[#f4d88a] relative inline-block">
                            <span className="relative" style={{
                                textShadow: '0 0 10px rgba(244,216,138,0.3), 0 2px 4px rgba(0,0,0,0.5)'
                            }}>
                                Hỗ trợ
                            </span>
                        </h3>
                        <ul className="space-y-3 list-none pl-0">
                            {supportLinks.map((link) => (
                                <li key={link.label}>
                                    <a href={link.href} className="text-gray-300 hover:text-[#f4d88a] transition-all duration-300 no-underline hover:translate-x-1 hover:underline underline-offset-4 block" style={{
                                        textShadow: '0 1px 2px rgba(0,0,0,0.3)'
                                    }}>
                                        {link.label}
                                    </a>
                                </li>
                            ))}
                        </ul>
                    </div>

                    {/* Contact */}
                    <div>
                        <h3 className="font-bold text-lg leading-tight mt-0 mb-4 text-[#f4d88a] relative inline-block">
                            <span className="relative" style={{
                                textShadow: '0 0 10px rgba(244,216,138,0.3), 0 2px 4px rgba(0,0,0,0.5)'
                            }}>
                                Liên hệ
                            </span>
                        </h3>
                        <ul className="space-y-3 list-none pl-0">
                            <li className="flex items-start gap-3 text-gray-300 group hover:text-[#f4d88a] transition-colors duration-300" style={{
                                textShadow: '0 1px 2px rgba(0,0,0,0.3)'
                            }}>
                                <MapPin className="w-5 h-5 flex-shrink-0 mt-0.5 text-[#D1B066] group-hover:scale-110 transition-transform" />
                                <span>Đà Nẵng, Việt Nam</span>
                            </li>
                            <li className="flex items-center gap-3 text-gray-300 group hover:text-[#f4d88a] transition-colors duration-300" style={{
                                textShadow: '0 1px 2px rgba(0,0,0,0.3)'
                            }}>
                                <Phone className="w-5 h-5 flex-shrink-0 text-[#D1B066] group-hover:scale-110 transition-transform" />
                                <span>1508</span>
                            </li>
                            <li className="flex items-center gap-3 text-gray-300 group hover:text-[#f4d88a] transition-colors duration-300" style={{
                                textShadow: '0 1px 2px rgba(0,0,0,0.3)'
                            }}>
                                <Mail className="w-5 h-5 flex-shrink-0 text-[#D1B066] group-hover:scale-110 transition-transform" />
                                <span>legacymap180@gmail.com</span>
                            </li>
                        </ul>
                    </div>
                </div>

                {/* Bottom bar */}
                <div className="relative pt-8 border-t border-white/10 z-10">
                    <div className="flex flex-col items-center text-center gap-3">
                        <div className="text-gray-300 text-sm" style={{
                            textShadow: '0 1px 2px rgba(0,0,0,0.3)'
                        }}>
                            © {new Date().getFullYear()} Cây Gia Phả. Tất cả quyền được bảo lưu.
                        </div>
                        <div className="flex items-center gap-1 text-gray-300 text-sm group hover:text-[#f4d88a] transition-colors duration-300" style={{
                            textShadow: '0 1px 2px rgba(0,0,0,0.3)'
                        }}>
                            <span>Được phát triển bởi team sinh viên thực tập năm 3 tại Việt Nam</span>
                            <Heart className="w-4 h-4 text-[#D1B066] fill-[#D1B066] animate-pulse group-hover:scale-110 transition-transform" />
                        </div>
                    </div>
                </div>
            </div>

            {/* Bottom glow effect */}
            <div className="absolute bottom-0 left-1/2 -translate-x-1/2 w-[1000px] h-[400px] bg-gradient-radial from-[#f4d88a]/10 via-[#D1B066]/5 to-transparent blur-[100px] rounded-full pointer-events-none animate-pulse-slow animation-delay-3000" />

            <style>{`
                @keyframes float {
                    0%, 100% { 
                        transform: translateY(0) translateX(0) scale(1) rotate(0deg); 
                        opacity: 0; 
                    }
                    10% { opacity: 1; }
                    50% { 
                        transform: translateY(-30vh) translateX(10px) scale(1.2) rotate(180deg);
                        opacity: 0.7;
                    }
                    90% { opacity: 0.3; }
                    100% { 
                        transform: translateY(-60vh) translateX(20px) scale(0.8) rotate(360deg); 
                        opacity: 0; 
                    }
                }
                
                @keyframes pulse-slow {
                    0%, 100% { 
                        opacity: 0.3;
                        transform: scale(1);
                    }
                    50% { 
                        opacity: 0.5;
                        transform: scale(1.05);
                    }
                }
                
                .animate-float { 
                    animation: float linear infinite; 
                }
                
                .animate-pulse-slow {
                    animation: pulse-slow 6s ease-in-out infinite;
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
        </footer>
    );
}