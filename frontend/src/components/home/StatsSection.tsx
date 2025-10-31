// src/components/home/StatsSection.tsx
import { useEffect, useState, useRef } from 'react';
import { Users, GitBranch, TrendingUp } from 'lucide-react';

interface Stat {
    icon: typeof Users;
    number: number;
    suffix: string;
    label: string;
}

const stats: Stat[] = [
    { icon: GitBranch, number: 1000, suffix: '+', label: 'Cây gia phả' },
    { icon: Users, number: 10000, suffix: '+', label: 'Người dùng' },
    { icon: TrendingUp, number: 50000, suffix: '+', label: 'Thành viên' },
];

function Counter({ target, suffix }: { target: number; suffix: string }) {
    const [count, setCount] = useState(0);
    const ref = useRef<HTMLDivElement>(null);
    const [started, setStarted] = useState(false);

    useEffect(() => {
        const observer = new IntersectionObserver(([entry]) => {
            if (entry.isIntersecting && !started) {
                setStarted(true);
                const duration = 2000;
                const steps = 60;
                const increment = target / steps;
                let current = 0;
                const timer = setInterval(() => {
                    current += increment;
                    if (current >= target) {
                        setCount(target);
                        clearInterval(timer);
                    } else setCount(Math.floor(current));
                }, duration / steps);
            }
        }, { threshold: 0.5 });

        if (ref.current) observer.observe(ref.current);
        return () => observer.disconnect();
    }, [target, started]);

    return (
        <div ref={ref} className="text-5xl font-extrabold text-[#b49e7b]">
            {count.toLocaleString()}{suffix}
        </div>
    );
}

export default function StatsSection() {
    return (
        <section className="py-24 bg-gradient-to-b from-[#f8f4ec] to-[#f0e6d2] text-center relative overflow-hidden">
            <div className="absolute inset-0 bg-[radial-gradient(circle_at_center,rgba(180,158,123,0.15),transparent_70%)] pointer-events-none" />
            <div className="max-w-6xl mx-auto px-6 relative z-10">
                <h2 className="text-3xl md:text-4xl font-bold mb-12 text-[#20283d]">
                    Những con số ấn tượng
                </h2>

                <div className="grid grid-cols-1 md:grid-cols-3 gap-10">
                    {stats.map(({ icon: Icon, number, suffix, label }, i) => (
                        <div
                            key={i}
                            className="group bg-white/80 backdrop-blur-md p-8 rounded-2xl border border-[#b49e7b]/30 shadow-md hover:shadow-lg hover:-translate-y-2 transition-all duration-500"
                        >
                            <div className="flex justify-center mb-4">
                                <div className="w-16 h-16 rounded-full bg-gradient-to-br from-[#b49e7b] to-[#d1b98a] flex items-center justify-center shadow-lg group-hover:scale-110 transition-transform">
                                    <Icon className="w-8 h-8 text-white" />
                                </div>
                            </div>

                            <Counter target={number} suffix={suffix} />
                            <div className="mt-2 text-lg font-semibold text-[#20283d]">{label}</div>
                        </div>
                    ))}
                </div>
            </div>
        </section>
    );
}
