// src/components/home/TestimonialsSection.tsx

const items = [
    { name: 'Minh T.', role: 'Hà Nội', quote: 'Gia đình mình đã kết nối lại nhờ biểu đồ phả hệ rõ ràng và dễ đọc.' },
    { name: 'Lan N.', role: 'Đà Nẵng', quote: 'Xuất PDF để in treo dịp giỗ Tổ—anh em ai cũng thích.' },
    { name: 'Quang P.', role: 'TP.HCM', quote: 'Chia sẻ đường link cho cả họ xem ngay trên điện thoại.' },
];

export default function TestimonialsSection() {
    return (
        <section className="py-16 bg-muted">
            <div className="max-w-7xl mx-auto px-6">
                <div className="text-center mb-10">
                    <h2 className="text-3xl lg:text-4xl font-bold text-foreground">Mọi người nói gì?</h2>
                    <p className="mt-3 text-muted-foreground">Một vài phản hồi từ người dùng đầu tiên.</p>
                </div>

                <div className="grid md:grid-cols-3 gap-6">
                    {items.map((t) => (
                        <figure key={t.name} className="card p-6">
                            <blockquote className="text-card-foreground">{t.quote}"</blockquote>
                            <figcaption className="mt-4 font-semibold text-card-foreground">{t.name}</figcaption>
                            <div className="text-muted-foreground text-sm">{t.role}</div>
                        </figure>
                    ))}
                </div>
            </div>
        </section>
    );
}