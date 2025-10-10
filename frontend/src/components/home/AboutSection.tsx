// src/components/home/AboutSection.tsx

export default function AboutSection() {
    return (
        <section className="py-16 bg-muted/40">
            <div className="max-w-7xl mx-auto px-6 grid lg:grid-cols-2 gap-10 items-center">
                <div className="order-2 lg:order-1">
                    <h2 className="text-3xl lg:text-4xl font-bold text-foreground">Về GPGay Gia Phả</h2>
                    <p className="mt-4 text-muted-foreground">
                        Ứng dụng giúp bạn lưu giữ lịch sử gia đình, kết nối thế hệ, và tôn vinh nguồn cội.
                        Từ quản lý thành viên đến xuất bản phả hệ — tất cả trong một nền tảng.
                    </p>
                    <ul className="mt-4 list-disc pl-5 text-muted-foreground space-y-2">
                        <li>UI hiện đại, dễ sử dụng</li>
                        <li>Hỗ trợ chia sẻ công khai hoặc riêng tư</li>
                        <li>Tùy biến biểu đồ phá hệ</li>
                    </ul>
                </div>

                <div className="order-1 lg:order-2 card overflow-hidden">
                    <img
                        src="https://images.unsplash.com/photo-1541534401786-2077eed87a72?q=80&w=1600&auto=format&fit=crop"
                        alt="About"
                        className="w-full h-full object-cover"
                        loading="lazy"
                    />
                </div>
            </div>
        </section>
    );
}