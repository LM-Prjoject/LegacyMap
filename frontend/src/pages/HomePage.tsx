// src/pages/dashboard/HomePage.tsx
import { useState, useEffect } from 'react';
import Navbar from '@/components/layout/Navbar';
import HeroSection from '@/components/home/HeroSection';
import StatsSection from '@/components/home/StatsSection';
import FeaturesSection from '@/components/home/FeaturesSection';
import AboutSection from '@/components/home/AboutSection';
import TestimonialsSection from '@/components/home/TestimonialsSection';
import CTASection from '@/components/home/CTASection';
import Footer from '@/components/layout/Footer';
import SignIn from '@/pages/auth/SignIn';
import SignUp from '@/pages/auth/SignUp';
import PasswordReset from '@/pages/auth/password-reset';

export default function HomePage() {
    const [showSignIn, setShowSignIn] = useState(false);
    const [showSignUp, setShowSignUp] = useState(false);
    const [showPasswordReset, setShowPasswordReset] = useState(false);
    const [resetToken, setResetToken] = useState<string | null>(null);

    // Tự động scroll mượt khi click anchor
    useEffect(() => {
        const handleAnchorClick = (e: MouseEvent) => {
            const target = e.target as HTMLAnchorElement;
            if (target.tagName === 'A' && target.hash) {
                const href = target.getAttribute('href');
                if (href?.startsWith('#')) {
                    e.preventDefault();
                    const element = document.querySelector(href);
                    if (element) {
                        element.scrollIntoView({ behavior: 'smooth', block: 'start' });
                    }
                }
            }
        };
        document.addEventListener('click', handleAnchorClick);
        return () => document.removeEventListener('click', handleAnchorClick);
    }, []);

    // Xử lý token reset mật khẩu
    useEffect(() => {
        const params = new URLSearchParams(window.location.search);
        const token = params.get('token');
        if (token) {
            setShowPasswordReset(true);
            setResetToken(token);
            const url = new URL(window.location.href);
            url.searchParams.delete('token');
            window.history.replaceState({}, '', url.toString());
        }
    }, []);

    // Nếu có ?showLogin=1 → mở modal đăng nhập
    useEffect(() => {
        const params = new URLSearchParams(window.location.search);
        if (params.get('showLogin') === '1') {
            setShowSignIn(true);
            const url = new URL(window.location.href);
            url.searchParams.delete('showLogin');
            window.history.replaceState({}, '', url.toString());
        }
    }, []);

    // Nếu có lỗi Google OAuth → mở modal đăng nhập
    useEffect(() => {
        const params = new URLSearchParams(window.location.search);
        const error = params.get('error');
        if (error) setShowSignIn(true);
    }, []);

    return (
        <div className="min-h-screen bg-[#fffaf3] text-[#20283d]">
            {/* Modals */}
            {showSignIn && (
                <SignIn
                    onClose={() => {
                        setShowSignIn(false);
                        const params = new URLSearchParams(window.location.search);
                        if (params.has('error')) {
                            params.delete('error');
                            const url = new URL(window.location.href);
                            url.search = params.toString();
                            window.history.replaceState({}, '', url.toString());
                        }
                    }}
                    onShowPasswordReset={() => {
                        setShowSignIn(false);
                        setShowPasswordReset(true);
                    }}
                    onShowSignUp={() => {
                        setShowSignIn(false);
                        setShowSignUp(true);
                    }}
                />
            )}

            {showSignUp && (
                <SignUp
                    onClose={() => setShowSignUp(false)}
                    onShowSignIn={() => {
                        setShowSignUp(false);
                        setShowSignIn(true);
                    }}
                />
            )}

            {showPasswordReset && (
                <PasswordReset
                    onClose={() => setShowPasswordReset(false)}
                    onShowSignIn={() => {
                        setShowPasswordReset(false);
                        setShowSignIn(true);
                    }}
                    token={resetToken || undefined}
                />
            )}

            {/* Navbar */}
            <Navbar onLoginClick={() => setShowSignIn(true)} onSignupClick={() => setShowSignUp(true)} />

            {/* Nội dung chính */}
            <main>
                <section id="hero"><HeroSection onSignupClick={() => setShowSignUp(true)} /></section>
                <section id="stats"><StatsSection /></section>
                <section id="features" className="scroll-mt-24"><FeaturesSection /></section>
                <section id="about" className="scroll-mt-24"><AboutSection /></section>
                <section id="testimonials" className="scroll-mt-24"><TestimonialsSection /></section>
                <section id="cta" className="scroll-mt-24">
                    <CTASection
                        onLoginClick={() => setShowSignIn(true)}
                        onSignupClick={() => setShowSignUp(true)}
                    />
                </section>
            </main>

            <Footer />
        </div>
    );
}
