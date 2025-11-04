// src/components/layout/Navbar.tsx
import React, { useState, useEffect } from 'react';
import { Link, useNavigate, useLocation } from 'react-router-dom';
import { LogOut, User, Album, TreePine, Menu, CalendarFold, Bell } from 'lucide-react';
import Button from './Button';
import logoImg from '@/assets/logo.png';
import { useAutoLogout } from '@/hooks/useAutoLogout';

interface NavbarProps {
    onLoginClick?: () => void;
    onSignupClick?: () => void;
}

const Navbar: React.FC<NavbarProps> = ({ onLoginClick, onSignupClick }) => {
    const navigate = useNavigate();
    const isHomePage = !!onLoginClick && !!onSignupClick;

    const [isAuthenticated, setIsAuthenticated] = useState(false);
    const [user, setUser] = useState<any>(null);
    const [showDropdown, setShowDropdown] = useState(false);
    const [isOpen, setIsOpen] = useState(false);
    const location = useLocation();

    useAutoLogout(30);

    const isDashboard =
        location.pathname.startsWith('/dashboard') ||
        location.pathname.startsWith('/trees') ||
        location.pathname.startsWith('/admin');

    const linkColor = isDashboard
        ? 'text-white hover:text-[#d1b98a]'
        : 'text-white hover:text-[#d1b98a]';

    useEffect(() => {
        const checkAuth = () => {
            const token = localStorage.getItem('authToken');
            const userStr = localStorage.getItem('user');
            if (token && userStr) {
                try {
                    const userData = JSON.parse(userStr);
                    setUser(userData);
                    setIsAuthenticated(true);
                } catch {
                    setIsAuthenticated(false);
                }
            } else {
                setIsAuthenticated(false);
                setUser(null);
            }
        };

        checkAuth();
        window.addEventListener('storage', checkAuth);
        return () => window.removeEventListener('storage', checkAuth);
    }, []);

    const handleLogout = () => {
        localStorage.removeItem('authToken');
        localStorage.removeItem('user');
        setIsAuthenticated(false);
        setUser(null);
        setShowDropdown(false);
        window.location.href = '/';
    };

    const getDisplayName = () => {
        if (!user) return 'User';
        const profile = user.profile || user.userProfile;
        const fullName = user.fullName || profile?.fullName;
        return fullName || user.username || user.email?.split('@')[0] || 'User';
    };

    const getAvatarUrl = () => {
        if (!user) return null as string | null;
        const profile = user.profile || user.userProfile;
        return user.avatarUrl || profile?.avatarUrl || null;
    };

    const getInitials = () => {
        const name = getDisplayName();
        return name.charAt(0).toUpperCase();
    };

    const isAdmin = () => {
        if (!user) return false;
        return user.roleName === 'admin' || user.role === 'admin';
    };

    const getDashboardUrl = () => {
        return isAdmin() ? '/admin/dashboard' : '/dashboard';
    };

    return (
        <nav
            className="fixed top-0 left-0 right-0 z-[100] border-b transition-colors duration-300 backdrop-blur-xl backdrop-saturate-150 shadow-[0_8px_24px_rgba(0,0,0,0.25)]"
            style={{
                background:
                    'linear-gradient(180deg, rgba(32,40,61,0.95) 0%, rgba(46,58,87,0.90) 100%)',
                borderBottom: '1px solid rgba(212, 185, 138, 0.25)'
            }}
        >
            <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
                {/* Row */}
                <div className="flex justify-between items-center h-16">
                    {/* Logo */}
                    <Link
                        to="/"
                        className="flex items-center gap-3 no-underline hover:opacity-90 transition-opacity"
                    >
                        <img
                            src={logoImg}
                            alt="Cây Gia Phả"
                            style={{ width: '56px', height: '56px' }}
                            className="rounded-lg"
                        />
                        <div className="leading-tight">
                            <div className="text-white font-semibold text-base">
                                Con Rồng Cháu Tiên
                            </div>
                            <div className="text-[12px] tracking-wide text-[#d1b98a]/80">
                                LegacyMap
                            </div>
                        </div>
                    </Link>

                    {/* Desktop menu */}
                    <div className="hidden md:flex items-center gap-10">
                        <a href="#features" className={`${linkColor} no-underline`}>
                            Tính năng
                        </a>
                        <a href="#about" className={`${linkColor} no-underline`}>
                            Về chúng tôi
                        </a>
                        <a href="#testimonials" className={`${linkColor} no-underline`}>
                            Khách hàng
                        </a>
                        <a href="#cta" className={`${linkColor} no-underline`}>
                            Liên hệ
                        </a>
                    </div>

                    {/* Actions */}
                    <div className="hidden md:flex items-center gap-3">
                        {isAuthenticated ? (
                            <div className="relative">
                                <button
                                    onClick={() => setShowDropdown(!showDropdown)}
                                    className="flex items-center gap-2 hover:opacity-90 transition-opacity"
                                >
                                    {getAvatarUrl() ? (
                                        <img
                                            src={getAvatarUrl() as string}
                                            alt={getDisplayName()}
                                            className="w-10 h-10 rounded-full object-cover shadow-lg ring-1 ring-[rgba(209,185,138,0.35)]"
                                            referrerPolicy="no-referrer"
                                        />
                                    ) : (
                                        <div
                                            className={`w-10 h-10 rounded-full ${
                                                isAdmin()
                                                    ? 'bg-gradient-to-br from-[#b49e7b] to-[#d1b98a]'
                                                    : 'bg-gradient-to-br from-[#2e3a57] to-[#20283d]'
                                            } flex items-center justify-center text-white font-semibold shadow-lg ring-1 ring-[rgba(209,185,138,0.35)]`}
                                        >
                                            {getInitials()}
                                        </div>
                                    )}
                                    <span className="hidden md:block text-sm font-medium text-white">
                                        {getDisplayName()}
                                    </span>
                                </button>

                                {showDropdown && (
                                    <>
                                        <div
                                            className="fixed inset-0 z-10"
                                            onClick={() => setShowDropdown(false)}
                                        />
                                        <div
                                            className="absolute right-0 mt-2 w-56 rounded-lg shadow-xl py-2 z-20 border"
                                            style={{
                                                background:
                                                    'linear-gradient(180deg, rgba(26,31,46,0.95) 0%, rgba(32,40,61,0.95) 100%)',
                                                borderColor: 'rgba(209,185,138,0.25)'
                                            }}
                                        >
                                            <div className="px-4 py-3 border-b border-[rgba(209,185,138,0.2)]">
                                                <p className="text-sm font-semibold text-white flex items-center gap-2">
                                                    {getDisplayName()}
                                                    {isAdmin() && (
                                                        <span className="px-2 py-0.5 bg-[#b49e7b]/20 text-[#d1b98a] text-xs font-bold rounded">
                                                            ADMIN
                                                        </span>
                                                    )}
                                                </p>
                                                <p className="text-xs text-white/70 truncate">
                                                    {user?.email}
                                                </p>
                                            </div>

                                            <button
                                                onClick={() => {
                                                    setShowDropdown(false);
                                                    navigate(getDashboardUrl());
                                                }}
                                                className={`w-full px-4 py-2 text-left text-sm hover:bg-white/5 transition flex items-center gap-2 ${
                                                    isAdmin() ? 'text-[#d1b98a] font-medium' : 'text-white'
                                                }`}
                                            >
                                                {isAdmin() ? (
                                                    <>
                                                        <Album className="h-4 w-4" />
                                                        Admin Dashboard
                                                    </>
                                                ) : (
                                                    <>
                                                        <TreePine className="h-4 w-4" />
                                                        Tạo cây gia phả
                                                    </>
                                                )}
                                            </button>

                                            <button
                                                onClick={() => {
                                                    setShowDropdown(false);
                                                    navigate('/profile');
                                                }}
                                                className="w-full px-4 py-2 text-left text-sm text-white hover:bg-white/5 transition flex items-center gap-2"
                                            >
                                                <User className="h-4 w-4" />
                                                Trang cá nhân
                                            </button>

                                            <button
                                                onClick={() => {
                                                    setShowDropdown(false);
                                                    navigate('/events');
                                                }}
                                                className="w-full px-4 py-2 text-left text-sm text-white hover:bg-white/5 transition flex items-center gap-2"
                                            >
                                                <CalendarFold className="h-4 w-4" />
                                                Sự kiện
                                            </button>

                                            <button
                                                onClick={() => {
                                                    setShowDropdown(false);
                                                    navigate('/notifications');
                                                }}
                                                className="w-full px-4 py-2 text-left text-sm text-white hover:bg-white/5 transition flex items-center gap-2"
                                            >
                                                <Bell className="h-4 w-4" />
                                                Thông báo
                                            </button>

                                            <hr className="my-2 border-[rgba(209,185,138,0.25)]" />

                                            <button
                                                onClick={handleLogout}
                                                className="w-full px-4 py-2 text-left text-sm text-red-300 hover:bg-red-500/10 transition flex items-center gap-2"
                                            >
                                                <LogOut className="h-4 w-4" />
                                                Đăng xuất
                                            </button>
                                        </div>
                                    </>
                                )}
                            </div>
                        ) : isHomePage ? (
                            <>
                                <Button
                                    variant="outline"
                                    size="sm"
                                    onClick={onLoginClick}
                                    className="border-[rgba(209,185,138,0.4)] text-white hover:bg-white/10"
                                >
                                    Đăng nhập
                                </Button>
                                <Button
                                    variant="primary"
                                    size="sm"
                                    onClick={onSignupClick}
                                    className="font-semibold text-[#20283d] bg-gradient-to-br from-[#b49e7b] to-[#d1b98a] shadow-[0_6px_20px_rgba(209,185,138,0.35)] hover:brightness-110 transition-all"
                                >
                                    Đăng ký
                                </Button>
                            </>
                        ) : (
                            <Link to="/">
                                <Button
                                    variant="outline"
                                    size="sm"
                                    className="border-[rgba(209,185,138,0.4)] text-white hover:bg-white/10"
                                >
                                    Trang chủ
                                </Button>
                            </Link>
                        )}
                    </div>

                    {/* Mobile menu button */}
                    <button
                        className="md:hidden text-[#d1b98a]"
                        onClick={() => setIsOpen((v) => !v)}
                        aria-label="Toggle Menu"
                    >
                        <Menu size={22} />
                    </button>
                </div>

                {/* Mobile menu */}
                {isOpen && (
                    <div className="md:hidden pb-4 space-y-2">
                        <a href="#features" className="block text-white hover:text-[#d1b98a] py-2">
                            Tính năng
                        </a>
                        <a href="#about" className="block text-white hover:text-[#d1b98a] py-2">
                            Về chúng tôi
                        </a>
                        <a href="#testimonials" className="block text-white hover:text-[#d1b98a] py-2">
                            Khách hàng
                        </a>
                        <a href="#cta" className="block text-white hover:text-[#d1b98a] py-2">
                            Liên hệ
                        </a>

                        {!isAuthenticated && isHomePage && (
                            <div className="flex gap-2 pt-2">
                                <Button
                                    variant="outline"
                                    className="flex-1 text-white border-[rgba(209,185,138,0.4)] hover:bg-white/10"
                                    onClick={onLoginClick}
                                >
                                    Đăng nhập
                                </Button>
                                <Button
                                    className="flex-1 font-semibold text-[#20283d] bg-gradient-to-br from-[#b49e7b] to-[#d1b98a] shadow-[0_6px_20px_rgba(209,185,138,0.35)] hover:brightness-110 transition-all"
                                    onClick={onSignupClick}
                                >
                                    Đăng ký
                                </Button>
                            </div>
                        )}
                    </div>
                )}
            </div>
        </nav>
    );
};

export default Navbar;