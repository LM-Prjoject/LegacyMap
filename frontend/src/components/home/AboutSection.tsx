// src/components/home/AboutSection.tsx
import AboutImage from '@/assets/logo.png';

export default function AboutSection() {
    return (
        <section className="relative py-32 bg-gradient-to-b from-[#fffaf3] via-[#f8f4ec] to-[#f2e7d6] overflow-hidden">
            {/* Hiệu ứng nền ánh vàng mờ */}
            <div className="absolute inset-0 bg-[radial-gradient(circle_at_center,rgba(209,185,138,0.15),transparent_70%)] pointer-events-none" />

            {/* Decorative elements */}
            <div className="absolute top-20 left-10 w-32 h-32 bg-[#d1b98a]/10 rounded-full blur-3xl" />
            <div className="absolute bottom-20 right-10 w-40 h-40 bg-[#d1b98a]/10 rounded-full blur-3xl" />

            <div className="max-w-7xl mx-auto px-6 grid lg:grid-cols-2 gap-16 items-center relative z-10">
                {/* Hình minh họa */}
                <div className="relative group">
                    {/* Viền ánh vàng glow với animation */}
                    <div className="absolute -inset-4 bg-gradient-to-br from-[#d1b98a]/30 via-[#d1b98a]/20 to-transparent rounded-3xl blur-2xl group-hover:blur-3xl transition-all duration-500" />
                    <div className="absolute -inset-1 bg-gradient-to-br from-[#d1b98a]/50 to-transparent rounded-3xl opacity-0 group-hover:opacity-100 transition-all duration-500" />

                    <div className="relative bg-white/40 backdrop-blur-sm rounded-3xl p-6 shadow-2xl flex items-center justify-center">
                        <img
                            src={AboutImage}
                            alt="Rồng Phượng – Con Rồng Cháu Tiên"
                            className="relative rounded-2xl w-3/4 object-contain transform group-hover:scale-105 transition-transform duration-500"
                        />
                    </div>

                    {/* Decorative corner accents */}
                    <div className="absolute -top-3 -left-3 w-6 h-6 border-t-2 border-l-2 border-[#d1b98a]/60 rounded-tl-lg" />
                    <div className="absolute -bottom-3 -right-3 w-6 h-6 border-b-2 border-r-2 border-[#d1b98a]/60 rounded-br-lg" />
                </div>

                {/* Nội dung mô tả */}
                <div className="space-y-8">
                    {/* Tiêu đề với accent line */}
                    <div>
                        <div className="flex items-center gap-3 mb-4">
                            <div className="w-12 h-1 bg-gradient-to-r from-[#d1b98a] to-transparent rounded-full" />
                            <span className="text-sm font-semibold text-[#b49e7b] tracking-wider uppercase">Giới thiệu</span>
                        </div>
                        <h2 className="text-5xl font-bold mb-3 text-[#20283d] leading-tight">
                            Về <span className="text-transparent bg-clip-text bg-gradient-to-r from-[#b49e7b] to-[#d1b98a]">LegacyMap</span>
                        </h2>
                        <div className="w-24 h-1 bg-gradient-to-r from-[#d1b98a] to-transparent rounded-full" />
                    </div>

                    {/* Content với improved spacing */}
                    <div className="space-y-6">
                        <p className="text-lg text-[#2e3a57] leading-relaxed">
                            <strong className="font-bold text-[#b49e7b]">LegacyMap</strong> là nền tảng giúp bạn dễ dàng xây dựng cây gia phả, lưu trữ và
                            kết nối các thế hệ trong gia đình, tôn vinh nguồn gốc <span className="text-[#b49e7b] font-semibold">"Con Rồng Cháu Tiên"</span>.
                        </p>

                        <p className="text-lg text-[#2e3a57] leading-relaxed">
                            Với giao diện trực quan, công nghệ bảo mật hiện đại và tính năng chia sẻ thân thiện,
                            chúng tôi mang đến trải nghiệm lưu giữ ký ức gia đình một cách trang trọng và ý nghĩa.
                        </p>

                        <p className="text-lg text-[#2e3a57] leading-relaxed italic">
                            Mỗi gia đình là một câu chuyện — và <span className="text-[#b49e7b] font-semibold not-italic">LegacyMap</span>
                            {' '}là nơi giúp bạn kể lại câu chuyện ấy bằng hình ảnh, kết nối và tự hào.
                        </p>
                    </div>
                </div>
            </div>

            {/* Ánh sáng mờ phía dưới tạo chiều sâu */}
            <div className="absolute bottom-0 left-1/2 -translate-x-1/2 w-[1000px] h-[400px] bg-[#d1b98a]/10 blur-3xl rounded-full pointer-events-none" />
        </section>
    );
}