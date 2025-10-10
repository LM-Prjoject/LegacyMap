// src/components/home/HeroSection.tsx
import { Link } from 'react-router-dom';
import Button from '@/components/layout/Button';
import RongPhuong from '@/assets/Rong_Phuong.jpg';

export default function HeroSection() {
    return (
        <header className="relative bg-gradient-to-br from-royal via-azure to-royal text-ivory">
            <div className="absolute inset-0 bg-white/0 [mask-image:radial-gradient(70%_70%_at_60%_40%,#000_40%,transparent_80%)]" />
            <div className="relative max-w-7xl mx-auto px-6 py-20 lg:py-28">
                <div className="grid lg:grid-cols-2 gap-10 items-center">
                    {/* Left: Text */}
                    <div>
            <span className="inline-block rounded-full bg-gold/20 text-golddark px-3 py-1 text-sm font-semibold">
              Con Rồng Cháu Tiên
            </span>
                        <h1 className="mt-4 text-4xl lg:text-5xl font-extrabold leading-tight drop-shadow">
                            Xây dựng <span className="text-gold">Cây Gia Phả</span> trực quan,
                            kết nối <span className="text-gold">Con Rồng Cháu Tiên</span>
                        </h1>
                        <p className="mt-4 text-ivory/90 text-lg">
                            Thêm thành viên, định nghĩa quan hệ, chia sẻ và in ấn chỉ với vài cú click.
                        </p>

                        <div className="mt-8 flex flex-wrap gap-4">
                            <Link to="/signup">
                                <Button variant="primary" size="lg" className="bg-gold text-ink hover:opacity-90">
                                    Bắt đầu miễn phí
                                </Button>
                            </Link>
                            <a href="#features">
                                <Button
                                    variant="outline"
                                    size="lg"
                                    className="border-ivory text-ivory hover:bg-ivory hover:text-ink"
                                >
                                    Xem tính năng
                                </Button>
                            </a>
                        </div>
                    </div>

                    {/* Right: Image card */}
                    <div className="card p-4 lg:p-6 bg-white/90">
                        <div className="relative rounded-2xl overflow-hidden ring-1 ring-cloud/60">
                            <img
                                src={RongPhuong}
                                alt="Rồng – Phượng (Rồng phượng tương giao)"
                                className="w-full h-full object-cover"
                                loading="lazy"
                            />
                            {/* subtle inner border */}
                            <div className="pointer-events-none absolute inset-0 rounded-2xl ring-1 ring-white/40" />
                        </div>
                        <p className="mt-3 text-ink/70 text-sm">
                            Tạo sơ đồ phả hệ đẹp mắt, gọn gàng và dễ chia sẻ.
                        </p>
                    </div>
                </div>
            </div>
        </header>
    );
}
