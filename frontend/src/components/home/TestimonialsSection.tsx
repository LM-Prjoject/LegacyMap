// src/components/home/TestimonialsSection.tsx
import { useState, useEffect } from 'react';
import { ChevronLeft, ChevronRight, Star, Quote } from 'lucide-react';

interface Testimonial {
    name: string;
    role: string;
    location: string;
    quote: string;
    rating: number;
    avatar: string;
}

const testimonials: Testimonial[] = [
    {
        name: 'Nguyễn Văn Thành',
        role: 'Trưởng họ',
        location: 'Hà Nội',
        quote: 'Ứng dụng rất dễ sử dụng và chuyên nghiệp. Tôi đã tạo được cây gia phả cho cả dòng họ với hơn 100 người. Rất hài lòng!',
        rating: 5,
        avatar: 'NT'
    },
    {
        name: 'Trần Thị Hương',
        role: 'Người dùng',
        location: 'TP. Hồ Chí Minh',
        quote: 'Giao diện đẹp, tính năng đầy đủ. Đặc biệt thích tính năng xuất PDF để in ra cho gia đình. Rất tuyệt vời!',
        rating: 5,
        avatar: 'TH'
    },
    {
        name: 'Lê Minh Đức',
        role: 'Giáo viên',
        location: 'Đà Nẵng',
        quote: 'Hệ thống bảo mật tốt, dữ liệu an toàn. Tôi yên tâm lưu trữ thông tin gia đình ở đây. Cảm ơn đội ngũ phát triển!',
        rating: 5,
        avatar: 'LM'
    },
    {
        name: 'Phạm Thu Hà',
        role: 'Kế toán',
        location: 'Hải Phòng',
        quote: 'Tính năng chia sẻ rất tiện lợi. Cả gia đình đều có thể xem và cập nhật thông tin dễ dàng. Đáng giá từng đồng!',
        rating: 5,
        avatar: 'PH'
    },
    {
        name: 'Hoàng Văn Nam',
        role: 'Doanh nhân',
        location: 'Cần Thơ',
        quote: 'Đã thử nhiều ứng dụng khác nhưng đây là tốt nhất. Giao diện đẹp, dễ dùng và có đầy đủ tính năng cần thiết.',
        rating: 5,
        avatar: 'HN'
    }
];

export default function TestimonialsSection() {
    const [currentIndex, setCurrentIndex] = useState(0);
    const [isAutoPlaying, setIsAutoPlaying] = useState(true); // ✅ Thêm setter

    // Auto slide
    useEffect(() => {
        if (!isAutoPlaying) return;

        const interval = setInterval(() => {
            setCurrentIndex((prev) => (prev + 1) % testimonials.length);
        }, 5000);

        return () => clearInterval(interval);
    }, [isAutoPlaying]);

    const goToPrevious = () => {
        setCurrentIndex((prev) => (prev - 1 + testimonials.length) % testimonials.length);
        setIsAutoPlaying(false);
    };

    const goToNext = () => {
        setCurrentIndex((prev) => (prev + 1) % testimonials.length);
        setIsAutoPlaying(false);
    };

    const goToSlide = (index: number) => {
        setCurrentIndex(index);
        setIsAutoPlaying(false);
    };

    return (
        <section className="py-20 bg-[#f7eede] relative overflow-hidden">
            {/* Background decoration */}
            <div className="absolute inset-0 bg-[radial-gradient(circle_at_50%_50%,rgba(209,176,102,0.08),transparent_70%)]" />

            <div className="relative max-w-7xl mx-auto px-6">
                {/* Header */}
                <div className="text-center mb-16">
                    <span className="inline-block px-4 py-1.5 rounded-full bg-yellow-100 text-yellow-700 font-semibold text-sm mb-4">
                        Đánh giá
                    </span>
                    <h2 className="text-4xl lg:text-5xl font-extrabold text-gray-900 mb-4">
                        Người dùng nói gì về chúng tôi
                    </h2>
                    <p className="text-xl text-gray-600">
                        Hơn 10,000 gia đình đã tin tưởng sử dụng
                    </p>
                </div>

                {/* Carousel */}
                <div className="relative max-w-4xl mx-auto">
                    {/* Main card */}
                    <div className="relative bg-white rounded-3xl shadow-2xl p-12 overflow-hidden">
                        {/* Quote icon */}
                        <Quote className="absolute top-8 left-8 w-16 h-16 text-blue-100" />

                        {/* Content */}
                        <div className="relative z-10">
                            {/* Stars */}
                            <div className="flex justify-center gap-1 mb-6">
                                {[...Array(testimonials[currentIndex].rating)].map((_, i) => (
                                    <Star key={i} className="w-6 h-6 fill-yellow-400 text-yellow-400" />
                                ))}
                            </div>

                            {/* Quote */}
                            <blockquote className="text-center text-2xl text-gray-700 leading-relaxed mb-8 min-h-[120px] flex items-center justify-center">
                                "{testimonials[currentIndex].quote}"
                            </blockquote>

                            {/* Avatar and name */}
                            <div className="flex items-center justify-center gap-4">
                                <div className="w-16 h-16 rounded-full bg-gradient-to-br from-blue-500 to-purple-500 flex items-center justify-center text-white font-bold text-xl shadow-lg">
                                    {testimonials[currentIndex].avatar}
                                </div>
                                <div className="text-left">
                                    <div className="font-bold text-lg text-gray-900">
                                        {testimonials[currentIndex].name}
                                    </div>
                                    <div className="text-sm text-gray-600">
                                        {testimonials[currentIndex].role} • {testimonials[currentIndex].location}
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>

                    {/* Navigation buttons */}
                    <button
                        onClick={goToPrevious}
                        className="absolute left-0 top-1/2 -translate-y-1/2 -translate-x-6 w-12 h-12 rounded-full bg-white shadow-lg hover:shadow-xl flex items-center justify-center transition-all hover:scale-110 group"
                    >
                        <ChevronLeft className="w-6 h-6 text-gray-600 group-hover:text-blue-600 transition-colors" />
                    </button>

                    <button
                        onClick={goToNext}
                        className="absolute right-0 top-1/2 -translate-y-1/2 translate-x-6 w-12 h-12 rounded-full bg-white shadow-lg hover:shadow-xl flex items-center justify-center transition-all hover:scale-110 group"
                    >
                        <ChevronRight className="w-6 h-6 text-gray-600 group-hover:text-blue-600 transition-colors" />
                    </button>

                    {/* Dots indicator */}
                    <div className="flex justify-center gap-2 mt-8">
                        {testimonials.map((_, index) => (
                            <button
                                key={index}
                                onClick={() => goToSlide(index)}
                                className={`h-2 rounded-full transition-all duration-300 ${
                                    index === currentIndex
                                        ? 'w-8 bg-blue-600'
                                        : 'w-2 bg-gray-300 hover:bg-gray-400'
                                }`}
                            />
                        ))}
                    </div>
                </div>

                {/* Stats */}
                <div className="grid grid-cols-3 gap-8 max-w-3xl mx-auto mt-16">
                    <div className="text-center">
                        <div className="text-4xl font-bold text-blue-600 mb-2">98%</div>
                        <div className="text-gray-600">Hài lòng</div>
                    </div>
                    <div className="text-center">
                        <div className="text-4xl font-bold text-purple-600 mb-2">4.9/5</div>
                        <div className="text-gray-600">Đánh giá</div>
                    </div>
                    <div className="text-center">
                        <div className="text-4xl font-bold text-yellow-600 mb-2">10K+</div>
                        <div className="text-gray-600">Người dùng</div>
                    </div>
                </div>
            </div>
        </section>
    );
}