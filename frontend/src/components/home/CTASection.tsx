// src/components/home/CTASection.tsx
import Button from '@/components/layout/Button';

interface CTASectionProps {
    onLoginClick: () => void;
    onSignupClick: () => void;
}

export default function CTASection({ onLoginClick, onSignupClick }: CTASectionProps) {
    return (
        <section className="py-16 bg-gradient-to-r from-amber-800/80 via-slate-700 to-blue-900 text-white">
            <div className="max-w-7xl mx-auto px-6 text-center">
                <h2 className="text-3xl lg:text-4xl font-extrabold">Sẵn sàng bắt đầu?</h2>
                <p className="mt-3 text-primary-foreground/90">
                    Tạo gia phả đầu tiên của bạn chỉ trong vài phút — miễn phí.
                </p>
                <div className="mt-8 flex justify-center gap-4">
                    <Button
                        onClick={onSignupClick}
                        variant="primary"
                        size="lg"
                        className="bg-accent text-accent-foreground hover:opacity-90"
                    >
                        Đăng ký ngay
                    </Button>
                    <Button
                        onClick={onLoginClick}
                        variant="outline"
                        size="lg"
                        className="border-primary-foreground text-primary-foreground hover:bg-primary-foreground hover:text-primary"
                    >
                        Tìm hiểu thêm
                    </Button>
                </div>
            </div>
        </section>
    );
}