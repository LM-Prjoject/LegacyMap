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

type ModalType = 'signin' | 'signup' | 'password-reset' | null;

export default function HomePage() {
    const [activeModal, setActiveModal] = useState<ModalType>(null);
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
            setActiveModal('password-reset');
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
            setActiveModal('signin');
            const url = new URL(window.location.href);
            url.searchParams.delete('showLogin');
            window.history.replaceState({}, '', url.toString());
        }
    }, []);

    // Nếu có lỗi Google OAuth → mở modal đăng nhập
    useEffect(() => {
        const params = new URLSearchParams(window.location.search);
        const error = params.get('error');
        if (error) setActiveModal('signin');
    }, []);

    // Hàm đóng modal và clear URL params nếu có error
    const handleCloseModal = () => {
        setActiveModal(null);
        const params = new URLSearchParams(window.location.search);
        if (params.has('error')) {
            params.delete('error');
            const url = new URL(window.location.href);
            url.search = params.toString();
            window.history.replaceState({}, '', url.toString());
        }
    };

    return (
        <div className="min-h-screen bg-[#fffaf3] text-[#20283d]">
            {/* MODALS - Chỉ render modal đang active */}
            {activeModal === 'signin' && (
                <SignIn
                    onClose={handleCloseModal}
                    onShowPasswordReset={() => setActiveModal('password-reset')}
                    onShowSignUp={() => setActiveModal('signup')}
                />
            )}

            {activeModal === 'signup' && (
                <SignUp
                    onClose={handleCloseModal}
                    onShowSignIn={() => setActiveModal('signin')}
                />
            )}

            {activeModal === 'password-reset' && (
                <PasswordReset
                    onClose={handleCloseModal}
                    onShowSignIn={() => setActiveModal('signin')}
                    token={resetToken || undefined}
                />
            )}

            {/* Navbar */}
            <Navbar
                onLoginClick={() => setActiveModal('signin')}
                onSignupClick={() => setActiveModal('signup')}
            />

            {/* Nội dung chính */}
            <main>
                <section id="hero">
                    <HeroSection onSignupClick={() => setActiveModal('signup')} />
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
                        onLoginClick={() => setActiveModal('signin')}
                        onSignupClick={() => setActiveModal('signup')}
                    />
                </section>
            </main>

            <Footer />
        </div>
    );
}