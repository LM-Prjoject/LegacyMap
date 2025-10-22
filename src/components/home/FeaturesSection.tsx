// src/components/home/FeaturesSection.tsx
import { Users, Share2, FileText, Shield, History, GitBranch } from 'lucide-react';
import type { LucideIcon } from 'lucide-react';

interface Feature {
    icon: LucideIcon;
    title: string;
    desc: string;
    color: string;
}

const features: Feature[] = [
    {
        icon: Users,
        title: 'Quản lý thành viên',
        desc: 'Thêm và quản lý thông tin chi tiết: tên, ngày sinh, giới tính, ảnh, tiểu sử và quan hệ gia đình.',
        color: 'from-[#053D88] to-[#074aa8]'
    },
    {
        icon: GitBranch,
        title: 'Cây gia phả trực quan',
        desc: 'Xem sơ đồ gia phả đẹp mắt với khả năng zoom, di chuyển và tương tác dễ dàng.',
        color: 'from-[#053D88] to-[#074aa8]'
    },
    {
        icon: Share2,
        title: 'Chia sẻ an toàn',
        desc: 'Tạo link chia sẻ với quyền chi xem, kiểm soát ai có thể truy cập thông tin gia đình.',
        color: 'from-[#D1B066] to-[#f4d88a]'
    },
    {
        icon: FileText,
        title: 'Xuất PDF',
        desc: 'Xuất cây gia phả ra file PDF chất lượng cao để in ấn hoặc lưu trữ offline.',
        color: 'from-[#D1B066] to-[#f4d88a]'
    },
    {
        icon: Shield,
        title: 'Bảo mật tuyệt đối',
        desc: 'Dữ liệu được mã hóa và bảo vệ. Chỉ bạn và người được chia sẻ mới có quyền truy cập.',
        color: 'from-[#053D88] to-[#074aa8]'
    },
    {
        icon: History,
        title: 'Lịch sử thay đổi',
        desc: 'Theo dõi mọi chỉnh sửa với audit log. Biết ai đã thay đổi gì và khi nào.',
        color: 'from-[#D1B066] to-[#f4d88a]'
    }
];

export default function FeaturesSection() {
    return (
        <section className="py-20 bg-[#f7eede] relative overflow-hidden">
            {/* Background decorations */}
            <div className="absolute inset-0 bg-[radial-gradient(circle_at_30%_20%,rgba(209,176,102,0.1),transparent_50%)]" />
            <div className="absolute inset-0 bg-[radial-gradient(circle_at_70%_80%,rgba(5,61,136,0.08),transparent_50%)]" />

            <div className="relative max-w-7xl mx-auto px-6">
                {/* Header */}
                <div className="text-center mb-16">
                    <span className="inline-block px-4 py-1.5 rounded-full bg-[#053D88]/10 text-[#053D88] font-semibold text-sm mb-4 border border-[#053D88]/20">
                        Tính năng
                    </span>
                    <h2 className="text-4xl lg:text-5xl font-extrabold text-gray-900 mb-4">
                        Quản lý gia phả chuyên nghiệp
                    </h2>
                    <p className="text-xl text-gray-600 max-w-2xl mx-auto">
                        Công cụ hiện đại với đầy đủ tính năng để lưu giữ và chia sẻ lịch sử gia đình
                    </p>
                </div>

                {/* Features grid */}
                <div className="grid md:grid-cols-2 lg:grid-cols-3 gap-8">
                    {features.map((feature, index) => {
                        const Icon = feature.icon;
                        return (
                            <div
                                key={index}
                                className="group relative bg-white rounded-2xl p-8 shadow-md hover:shadow-xl transition-all duration-500 hover:-translate-y-2 border border-gray-100"
                                style={{ animationDelay: `${index * 100}ms` }}
                            >
                                {/* Gradient border on hover */}
                                <div className={`absolute inset-0 rounded-2xl bg-gradient-to-br ${feature.color} opacity-0 group-hover:opacity-100 transition-opacity duration-500 -z-10 blur-xl`} />

                                {/* Icon container */}
                                <div className="relative mb-6">
                                    <div className={`w-14 h-14 rounded-xl bg-gradient-to-br ${feature.color} flex items-center justify-center shadow-lg group-hover:scale-110 group-hover:rotate-6 transition-all duration-500`}>
                                        <Icon className="w-7 h-7 text-white" />
                                    </div>
                                </div>

                                {/* Content */}
                                <h3 className="text-xl font-bold text-gray-900 mb-3 group-hover:text-transparent group-hover:bg-clip-text group-hover:bg-gradient-to-br group-hover:from-[#053D88] group-hover:to-[#074aa8] transition-all duration-300">
                                    {feature.title}
                                </h3>
                                <p className="text-gray-600 leading-relaxed">
                                    {feature.desc}
                                </p>

                                {/* Hover arrow */}
                                <div className="mt-4 flex items-center text-transparent bg-clip-text bg-gradient-to-r from-[#053D88] to-[#074aa8] opacity-0 group-hover:opacity-100 transition-opacity duration-300">
                                    <span className="text-sm font-semibold">Tìm hiểu thêm</span>
                                    <svg className="w-4 h-4 ml-2 group-hover:translate-x-2 transition-transform duration-300" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                                        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 5l7 7-7 7" />
                                    </svg>
                                </div>
                            </div>
                        );
                    })}
                </div>

                {/* CTA */}
                <div className="mt-16 text-center">
                    <a href="#cta" className="inline-flex items-center gap-2 px-8 py-4 rounded-full bg-gradient-to-r from-[#053D88] to-[#074aa8] text-white font-semibold shadow-lg hover:shadow-xl hover:scale-105 transition-all duration-300">
                        <span>Dùng thử miễn phí</span>
                        <svg className="w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M13 7l5 5m0 0l-5 5m5-5H6" />
                        </svg>
                    </a>
                </div>
            </div>
        </section>
    );
}