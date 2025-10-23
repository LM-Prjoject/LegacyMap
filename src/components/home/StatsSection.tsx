// src/components/home/StatsSection.tsx
import { useEffect, useState, useRef } from 'react';
import { Users, GitBranch, TrendingUp } from 'lucide-react';

interface Stat {
    icon: typeof Users;
    number: number;
    suffix: string;
    label: string;
    color: string;
}

const stats: Stat[] = [
    { icon: GitBranch, number: 1000, suffix: '+', label: 'Gia phả', color: 'from-[#053D88] to-[#074aa8]' },
    { icon: Users, number: 10000, suffix: '+', label: 'Người dùng', color: 'from-[#053D88] to-[#074aa8]' },
    { icon: TrendingUp, number: 50000, suffix: '+', label: 'Thành viên', color: 'from-[#D1B066] to-[#f4d88a]' }
];

function Counter({ target, suffix }: { target: number; suffix: string }) {
    const [count, setCount] = useState(0);
    const elementRef = useRef<HTMLDivElement>(null);
    const [hasAnimated, setHasAnimated] = useState(false);

    useEffect(() => {
        const observer = new IntersectionObserver(
            ([entry]) => {
                if (entry.isIntersecting && !hasAnimated) {
                    setHasAnimated(true);

                    const duration = 2000;
                    const steps = 60;
                    const increment = target / steps;
                    let current = 0;

                    const timer = setInterval(() => {
                        current += increment;
                        if (current >= target) {
                            setCount(target);
                            clearInterval(timer);
                        } else {
                            setCount(Math.floor(current));
                        }
                    }, duration / steps);

                    return () => clearInterval(timer);
                }
            },
            { threshold: 0.5 }
        );

        if (elementRef.current) {
            observer.observe(elementRef.current);
        }

        return () => observer.disconnect();
    }, [target, hasAnimated]);

    return (
        <div ref={elementRef} className="text-5xl font-extrabold">
            {count.toLocaleString()}{suffix}
        </div>
    );
}

export default function StatsSection() {
    return (
        <section className="py-20 bg-[#f7eede]">
            <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
                <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
                    {stats.map((stat, index) => {
                        const Icon = stat.icon;
                        return (
                            <div
                                key={index}
                                className="group relative bg-white rounded-2xl p-8 shadow-lg hover:shadow-2xl transition-all duration-500 hover:-translate-y-2"
                                style={{ animationDelay: `${index * 150}ms` }}
                            >
                                {/* Background gradient on hover */}
                                <div className={`absolute inset-0 bg-gradient-to-br ${stat.color} opacity-0 group-hover:opacity-5 rounded-2xl transition-opacity duration-500`} />

                                {/* Icon */}
                                <div className="relative flex justify-center mb-4">
                                    <div className={`w-16 h-16 rounded-full bg-gradient-to-br ${stat.color} flex items-center justify-center shadow-lg group-hover:scale-110 transition-transform duration-500`}>
                                        <Icon className="w-8 h-8 text-white" />
                                    </div>
                                </div>

                                {/* Counter */}
                                <div className={`relative text-center mb-2 bg-gradient-to-br ${stat.color} bg-clip-text text-transparent`}>
                                    <Counter target={stat.number} suffix={stat.suffix} />
                                </div>

                                {/* Label */}
                                <div className="relative text-center text-xl font-semibold text-gray-700">
                                    {stat.label}
                                </div>

                                {/* Decorative line */}
                                <div className={`mt-4 h-1 w-0 group-hover:w-full bg-gradient-to-r ${stat.color} rounded-full transition-all duration-500 mx-auto`} />
                            </div>
                        );
                    })}
                </div>

                {/* Additional info */}
                <div className="mt-12 text-center">
                    <p className="text-gray-600 text-lg">
                        Được tin tưởng bởi hàng ngàn gia đình trên khắp Việt Nam 🇻🇳
                    </p>
                </div>
            </div>
        </section>
    );
}