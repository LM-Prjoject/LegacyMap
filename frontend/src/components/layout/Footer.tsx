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
        <footer className="bg-gradient-to-b from-[#053D88] to-[#032859] text-white">
            <div className="max-w-7xl mx-auto px-6 py-16">
                <div className="grid md:grid-cols-2 lg:grid-cols-4 gap-12 mb-12 items-start">
                    {/* Brand */}
                    <div>
                        <div className="flex items-center gap-2 mb-4">
                            <div className="w-12 h-12 flex items-center justify-center rounded-full bg-gradient-to-br overflow-hidden">
                                <img src={logo} alt="Logo" className="w-full h-full object-cover" />
                            </div>
                            <div>
                                <div className="font-bold text-xl">Cây Gia Phả</div>
                                <div className="text-sm text-blue-200">Con Rồng Châu Tiên</div>
                            </div>
                        </div>
                        <p className="text-blue-200 leading-relaxed mb-6">
                            Hệ thống quản lý cây gia phả hiện đại, giúp lưu giữ và tôn vinh truyền thống gia đình Việt Nam qua nhiều thế hệ.
                        </p>
                        <div className="flex gap-3">
                            <a href="#" className="w-10 h-10 rounded-full bg-[#074aa8] hover:bg-[#D1B066] flex items-center justify-center transition-colors">
                                <Facebook className="w-5 h-5" />
                            </a>
                            <a href="#" className="w-10 h-10 rounded-full bg-[#074aa8] hover:bg-[#D1B066] flex items-center justify-center transition-colors">
                                <Youtube className="w-5 h-5" />
                            </a>
                            <a href="#" className="w-10 h-10 rounded-full bg-[#074aa8] hover:bg-[#D1B066] flex items-center justify-center transition-colors">
                                <Mail className="w-5 h-5" />
                            </a>
                        </div>
                    </div>

                    {/* Products */}
                    <div>
                        <h3 className="font-bold text-lg leading-tight mt-0 mb-4 text-[#f4d88a]">Sản phẩm</h3>
                        <ul className="space-y-3">
                            {productLinks.map((link) => (
                                <li key={link.label}>
                                    <a href={link.href} className="text-blue-200 hover:text-[#f4d88a] transition-colors no-underline">
                                        {link.label}
                                    </a>
                                </li>
                            ))}
                        </ul>
                    </div>

                    {/* Support */}
                    <div>
                        <h3 className="font-bold text-lg leading-tight mt-0 mb-4 text-[#f4d88a]">Hỗ trợ</h3>
                        <ul className="space-y-3">
                            {supportLinks.map((link) => (
                                <li key={link.label}>
                                    <a href={link.href} className="text-blue-200 hover:text-[#f4d88a] transition-colors no-underline">
                                        {link.label}
                                    </a>
                                </li>
                            ))}
                        </ul>
                    </div>

                    {/* Contact */}
                    <div>
                        <h3 className="font-bold text-lg leading-tight mt-0 mb-4 text-[#f4d88a]">Liên hệ</h3>
                        <ul className="space-y-3">
                            <li className="flex items-start gap-3 text-blue-200">
                                <MapPin className="w-5 h-5 flex-shrink-0 mt-0.5 text-[#D1B066]" />
                                <span>Đà Nẵng, Việt Nam</span>
                            </li>
                            <li className="flex items-center gap-3 text-blue-200">
                                <Phone className="w-5 h-5 flex-shrink-0 text-[#D1B066]" />
                                <span>1508</span>
                            </li>
                            <li className="flex items-center gap-3 text-blue-200">
                                <Mail className="w-5 h-5 flex-shrink-0 text-[#D1B066]" />
                                <span>legacymap180@gmail.com</span>
                            </li>
                        </ul>
                    </div>
                </div>

                {/* Bottom bar */}
                <div className="pt-8 border-t border-[#074aa8]">
                    <div className="flex flex-col items-center text-center gap-3">
                        <div className="text-blue-200 text-sm">
                            © {new Date().getFullYear()} Cây Gia Phả. Tất cả quyền được bảo lưu.
                        </div>
                        <div className="flex items-center gap-1 text-blue-200 text-sm">
                            <span>Được phát triển bởi team sinh viên thực tập năm 3 tại Việt Nam</span>
                            <Heart className="w-4 h-4 text-[#D1B066] fill-[#D1B066] animate-pulse" />
                        </div>
                    </div>
                </div>
            </div>
        </footer>
    );
}