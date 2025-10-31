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

    // Nếu URL có ?token=... (từ email), tự mở modal đặt lại mật khẩu
    useEffect(() => {
        const params = new URLSearchParams(window.location.search);
        const token = params.get('token');
        if (token) {
            setShowPasswordReset(true);
            setResetToken(token);
            // Xóa token khỏi URL để tránh mở lại khi refresh
            const url = new URL(window.location.href);
            url.searchParams.delete('token');
            window.history.replaceState({}, '', url.toString());
        }
    }, []);

    // Nếu URL có ?showLogin=1 (sau verify email), tự mở modal đăng nhập
    useEffect(() => {
        const params = new URLSearchParams(window.location.search);
        const showLogin = params.get('showLogin');
        if (showLogin === '1') {
            setShowSignIn(true);
            const url = new URL(window.location.href);
            url.searchParams.delete('showLogin');
            window.history.replaceState({}, '', url.toString());
        }
    }, []);

    // NEW: Tự động mở modal SignIn khi có error từ Google OAuth
    useEffect(() => {
        const params = new URLSearchParams(window.location.search);
        const error = params.get('error');

        if (error) {
            // Mở modal SignIn để hiển thị error message
            setShowSignIn(true);

            // NOTE: Không xóa error param ở đây
            // Để SignIn component tự xử lý và xóa sau khi hiển thị message
        }
    }, []);

    return (
        <div className="min-h-screen bg-white">

            {showSignIn && (
                <SignIn
                    onClose={() => {
                        setShowSignIn(false);

                        // Xóa error param khi đóng modal
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

            {/* Modal đăng ký */}
            {showSignUp && (
                <SignUp
                    onClose={() => setShowSignUp(false)}
                    onShowSignIn={() => {
                        setShowSignUp(false);
                        setShowSignIn(true);
                    }}
                />
            )}

            {/* Modal quên mật khẩu */}
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

            {/* ========== NAVBAR ========== */}
            <Navbar
                onLoginClick={() => setShowSignIn(true)}
                onSignupClick={() => setShowSignUp(true)}
            />

            {/* ========== MAIN CONTENT ========== */}
            <main>
                <section id="hero">
                    <HeroSection
                        onSignupClick={() => setShowSignUp(true)}
                    />
                </section>

                <section id="stats">
                    <StatsSection />
                </section>

                <section id="features" className="scroll-mt-24">
                    <FeaturesSection />
                </section>

                <section id="about" className="scroll-mt-24">
                    <AboutSection />
                </section>

                <section id="testimonials" className="scroll-mt-24">
                    <TestimonialsSection />
                </section>

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