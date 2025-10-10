// src/pages/HomePage.tsx
import { useState } from 'react';
import Navbar from '@/components/layout/Navbar';
import HeroSection from '@/components/home/HeroSection';
import StatsSection from '@/components/home/StatsSection';
import FeaturesSection from '@/components/home/FeaturesSection';
import TestimonialsSection from '@/components/home/TestimonialsSection';
import CTASection from '@/components/home/CTASection';
import Footer from '@/components/home/Footer';
import SignIn from '@/pages/auth/SignIn';
import SignUp from '@/pages/auth/SignUp';
import PasswordReset from '@/pages/auth/password-reset';

export default function HomePage() {
    const [showSignIn, setShowSignIn] = useState(false);
    const [showSignUp, setShowSignUp] = useState(false);
    const [showPasswordReset, setShowPasswordReset] = useState(false);

    return (
        <div className="min-h-screen bg-white">
            {/* Modal đăng nhập */}
            {showSignIn && (
                <SignIn
                    onClose={() => setShowSignIn(false)}
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
                />
            )}

            <Navbar
                onLoginClick={() => setShowSignIn(true)}
                onSignupClick={() => setShowSignUp(true)}
            />
            <main>
                <section id="hero">
                    <HeroSection
                        onLoginClick={() => setShowSignIn(true)}
                        onSignupClick={() => setShowSignUp(true)}
                    />
                </section>
                <section id="stats" className="scroll-mt-24">
                    <StatsSection />
                </section>
                <section id="features" className="scroll-mt-24">
                    <FeaturesSection />
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