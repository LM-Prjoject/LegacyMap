// src/components/common/Particles.tsx
import { useEffect } from 'react';

interface ParticlesProps {
    color?: string;
    quantity?: number; // vẫn giữ để tương thích với HeroSection, nhưng không cần dùng
    className?: string;
    ease?: number;
    refresh?: boolean;
}

export default function Particles({
                                      color = '#ffffff',
                                      className = '',
                                  }: ParticlesProps) {
    useEffect(() => {
        // Placeholder effect — giữ cho tương thích, không thực hiện gì
    }, []);

    return (
        <div
            className={`absolute inset-0 ${className}`}
            style={{
                background: `radial-gradient(circle at center, ${color}10 0%, transparent 80%)`,
                pointerEvents: 'none',
            }}
        />
    );
}
