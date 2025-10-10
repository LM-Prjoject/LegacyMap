// src/components/layout/Navbar.tsx
import React from 'react';
import { Link } from 'react-router-dom';
import Button from './Button';

interface NavbarProps {
    onLoginClick?: () => void;
    onSignupClick?: () => void;
}

const Navbar: React.FC<NavbarProps> = ({ onLoginClick, onSignupClick }) => {
    const isHomePage = !!onLoginClick && !!onSignupClick;

    return (
        <nav className="bg-background/95 backdrop-blur shadow-sm py-4 sticky top-0 z-40 border-b border-border">
            <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
                <div className="flex justify-between items-center">
                    {/* Logo */}
                    <Link to="/" className="flex items-baseline gap-2">
                        <div className="text-2xl font-extrabold text-primary tracking-tight">GPGay Gia Phả</div>
                        <div className="text-sm text-muted-foreground italic">Con Rồng Cháu Tiên</div>
                    </Link>

                    {/* Navigation Links */}
                    <div className="hidden md:flex items-center gap-8">
                        <a href="#features" className="text-foreground hover:text-primary transition-colors">Tính năng</a>
                        <a href="#about" className="text-foreground hover:text-primary transition-colors">Về chúng tôi</a>
                        <a href="#testimonials" className="text-foreground hover:text-primary transition-colors">Khách hàng</a>
                        <a href="#cta" className="text-foreground hover:text-primary transition-colors">Liên hệ</a>
                    </div>

                    {/* Auth Buttons */}
                    <div className="flex items-center gap-3">
                        {isHomePage ? (
                            // Trang chủ - hiển thị nút đăng nhập/đăng ký
                            <>
                                <Button
                                    variant="outline"
                                    size="sm"
                                    onClick={onLoginClick}
                                >
                                    Đăng nhập
                                </Button>
                                <Button
                                    variant="primary"
                                    size="sm"
                                    onClick={onSignupClick}
                                >
                                    Đăng ký
                                </Button>
                            </>
                        ) : (
                            // Trang dashboard/tree - hiển thị nút quay về trang chủ
                            <Link to="/">
                                <Button variant="outline" size="sm">Trang chủ</Button>
                            </Link>
                        )}
                    </div>
                </div>
            </div>
        </nav>
    );
};

export default Navbar;