// src/components/home/FeaturesSection.tsx
import { Link } from 'react-router-dom';
import { Users, Share2, Printer, Shield } from 'lucide-react';
import type { LucideIcon } from 'lucide-react';

interface Feature {
    icon: LucideIcon;
    title: string;
    desc: string;
}

const features: Feature[] = [
    { icon: Users, title: 'Quan hệ rõ ràng', desc: 'Cha/mẹ/con, vợ/chồng — thể hiện trực quan, dễ theo dõi.' },
    { icon: Share2, title: 'Chia sẻ nhanh', desc: 'Tạo link public hoặc giới hạn quyền xem theo ý bạn.' },
    { icon: Printer, title: 'In & PDF', desc: 'Xuất biểu đồ ra file PDF để in ấn & trưng bày.' },
    { icon: Shield, title: 'Bảo mật', desc: 'Kiểm soát truy cập, chỉ người được mời mới xem/chỉnh sửa.' },
];

export default function FeaturesSection() {
    return (
        <section className="py-16 bg-background">
            <div className="max-w-7xl mx-auto px-6">
                <div className="text-center mb-12">
                    <h2 className="text-3xl lg:text-4xl font-bold text-foreground">Tính năng nổi bật</h2>
                    <p className="mt-3 text-muted-foreground">Đủ sâu để quản lý phức tạp, đủ đơn giản để ai cũng dùng được.</p>
                </div>

                <div className="grid sm:grid-cols-2 lg:grid-cols-4 gap-6">
                    {features.map((f) => {
                        const IconComponent = f.icon;
                        return (
                            <div key={f.title} className="card p-6">
                                <IconComponent className="w-8 h-8 text-accent mb-4" />
                                <h3 className="font-semibold text-xl text-card-foreground">{f.title}</h3>
                                <p className="mt-2 text-muted-foreground">{f.desc}</p>
                            </div>
                        );
                    })}
                </div>

                <div className="text-center mt-10">
                    <Link to="/signup" className="btn-primary">Dùng thử ngay</Link>
                </div>
            </div>
        </section>
    );
}