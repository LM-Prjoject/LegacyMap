// src/components/home/CTASection.tsx
import Button from '@/components/layout/Button';

interface CTASectionProps {
    onSignupClick: () => void;
    onLoginClick?: () => void;
}

export default function CTASection({ onSignupClick }: CTASectionProps) {
    return (
        <section className="relative py-24 bg-gradient-to-br from-[#f0e6d2] via-[#f8f4ec] to-[#fffdf8] text-center overflow-hidden">
            <div className="absolute inset-0 bg-[radial-gradient(circle_at_center,rgba(180,158,123,0.15),transparent_70%)]" />
            <div className="relative z-10 max-w-3xl mx-auto px-6">
                <h2 className="text-3xl md:text-4xl font-bold text-[#20283d] mb-6">
                    Sẵn sàng xây dựng cây gia phả của riêng bạn?
                </h2>
                <p className="text-lg text-[#2e3a57] mb-10">
                    Hãy bắt đầu hành trình lưu giữ truyền thống và kết nối gia đình bạn ngay hôm nay —
                    chỉ với vài bước đơn giản.
                </p>

                <div className="flex flex-wrap justify-center gap-4">
                    <Button
                        variant="primary"
                        size="lg"
                        onClick={onSignupClick}
                        className="bg-gradient-to-r from-[#d1b98a] to-[#b49e7b] text-[#20283d] hover:scale-105 hover:shadow-lg hover:shadow-[#d1b98a]/40 transition-all font-semibold"
                    >
                        Bắt đầu miễn phí
                    </Button>
                    <a href="#features">
                        <Button
                            variant="outline"
                            size="lg"
                            className="border-2 border-[#b49e7b] text-[#b49e7b] hover:bg-[#b49e7b]/10 hover:scale-105 transition-all hover:text-[#b49e7b] !text-[#b49e7b]"
                        >
                            Tìm hiểu thêm
                        </Button>
                    </a>
                </div>
            </div>
        </section>
    );
}