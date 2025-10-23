import { Heart, Target, Users, Shield } from 'lucide-react';

const values = [
    {
        icon: Heart,
        title: 'Tôn trọng truyền thống',
        desc: 'Giữ gìn và phát huy giá trị văn hóa gia đình Việt Nam'
    },
    {
        icon: Target,
        title: 'Đổi mới công nghệ',
        desc: 'Ứng dụng công nghệ hiện đại vào quản lý gia phả'
    },
    {
        icon: Users,
        title: 'Kết nối thế hệ',
        desc: 'Tạo cầu nối giữa các thế hệ trong gia đình'
    },
    {
        icon: Shield,
        title: 'Bảo mật tuyệt đối',
        desc: 'Cam kết bảo vệ thông tin gia đình của bạn'
    }
];

export default function AboutSection() {
    return (
        <section className="py-20 bg-[#f7eede] relative overflow-hidden">
            {/* Background decoration */}
            <div className="absolute inset-0 bg-[radial-gradient(circle_at_20%_30%,rgba(209,176,102,0.1),transparent_50%)]" />
            <div className="absolute inset-0 bg-[radial-gradient(circle_at_80%_70%,rgba(5,61,136,0.05),transparent_50%)]" />

            <div className="relative max-w-7xl mx-auto px-6">
                {/* Header */}
                <div className="text-center mb-16">
                    <span className="inline-block px-4 py-1.5 rounded-full bg-[#053D88]/10 text-[#053D88] font-semibold text-sm mb-4 border border-[#053D88]/20">
                        Về chúng tôi
                    </span>
                    <h2 className="text-4xl lg:text-5xl font-extrabold text-gray-900 mb-4">
                        Lưu giữ dòng họ Việt Nam
                    </h2>
                    <p className="text-xl text-gray-600 max-w-3xl mx-auto">
                        Hệ thống quản lý cây gia phả hiện đại, giúp lưu giữ và tôn vinh truyền thống gia đình Việt Nam qua nhiều thế hệ.
                    </p>
                </div>

                {/* Main content */}
                <div className="grid lg:grid-cols-2 gap-12 items-center mb-20">
                    {/* Left: Image */}
                    <div className="relative group order-2 lg:order-1">
                        <div className="absolute -inset-4 bg-gradient-to-r from-[#053D88] to-[#074aa8] rounded-3xl blur-2xl opacity-20 group-hover:opacity-30 transition-opacity" />
                        <div className="relative rounded-3xl overflow-hidden shadow-2xl">
                            <img
                                src="https://images.unsplash.com/photo-1511632765486-a01980e01a18?w=800&q=80"
                                alt="Family gathering"
                                className="w-full h-[400px] object-cover transform group-hover:scale-105 transition-transform duration-700"
                                loading="lazy"
                            />
                            <div className="absolute inset-0 bg-gradient-to-t from-[#053D88]/50 to-transparent" />
                        </div>
                    </div>

                    {/* Right: Text */}
                    <div className="order-1 lg:order-2">
                        <h3 className="text-3xl font-bold text-gray-900 mb-6">
                            Sứ mệnh của chúng tôi
                        </h3>
                        <p className="text-lg text-gray-600 mb-6 leading-relaxed">
                            Chúng tôi tin rằng mỗi gia đình đều có một câu chuyện độc đáo cần được ghi nhận và lưu truyền.
                            Cây Gia Phả ra đời với sứ mệnh giúp các gia đình Việt Nam dễ dàng quản lý, lưu giữ và
                            chia sẻ lịch sử gia đình của mình.
                        </p>
                        <ul className="space-y-4">
                            <li className="flex items-start gap-3">
                                <div className="w-6 h-6 rounded-full bg-gradient-to-br from-[#D1B066] to-[#f4d88a] flex items-center justify-center flex-shrink-0 mt-1">
                                    <svg className="w-4 h-4 text-white" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                                        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M5 13l4 4L19 7" />
                                    </svg>
                                </div>
                                <span className="text-gray-700">Giao diện thân thiện, dễ sử dụng cho mọi lứa tuổi</span>
                            </li>
                            <li className="flex items-start gap-3">
                                <div className="w-6 h-6 rounded-full bg-gradient-to-br from-[#D1B066] to-[#f4d88a] flex items-center justify-center flex-shrink-0 mt-1">
                                    <svg className="w-4 h-4 text-white" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                                        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M5 13l4 4L19 7" />
                                    </svg>
                                </div>
                                <span className="text-gray-700">Hỗ trợ đa nền tảng: Web, Mobile, Desktop</span>
                            </li>
                            <li className="flex items-start gap-3">
                                <div className="w-6 h-6 rounded-full bg-gradient-to-br from-[#D1B066] to-[#f4d88a] flex items-center justify-center flex-shrink-0 mt-1">
                                    <svg className="w-4 h-4 text-white" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                                        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M5 13l4 4L19 7" />
                                    </svg>
                                </div>
                                <span className="text-gray-700">Cập nhật tính năng thường xuyên theo yêu cầu người dùng</span>
                            </li>
                        </ul>
                    </div>
                </div>

                {/* Values */}
                <div className="grid md:grid-cols-2 lg:grid-cols-4 gap-6">
                    {values.map((value, index) => {
                        const Icon = value.icon;
                        return (
                            <div
                                key={index}
                                className="group bg-white rounded-2xl p-6 shadow-lg hover:shadow-xl transition-all duration-500 hover:-translate-y-2"
                            >
                                <div className="w-12 h-12 rounded-xl bg-gradient-to-br from-[#053D88] to-[#074aa8] flex items-center justify-center mb-4 group-hover:scale-110 group-hover:rotate-6 transition-all duration-500">
                                    <Icon className="w-6 h-6 text-white" />
                                </div>
                                <h4 className="font-bold text-gray-900 mb-2">{value.title}</h4>
                                <p className="text-gray-600 text-sm">{value.desc}</p>
                            </div>
                        );
                    })}
                </div>
            </div>
        </section>
    );
}