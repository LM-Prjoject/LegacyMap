// src/components/layout/Navbar.tsx
import React, { useState, useEffect, useRef } from 'react';
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

    // Thêm useRef để theo dõi dropdown
    const dropdownRef = useRef<HTMLDivElement>(null);

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

    // Thêm useEffect để xử lý click outside
    useEffect(() => {
        const handleClickOutside = (event: MouseEvent) => {
            if (dropdownRef.current && !dropdownRef.current.contains(event.target as Node)) {
                setShowDropdown(false);
            }
        };

        if (showDropdown) {
            document.addEventListener('mousedown', handleClickOutside);
        }

        return () => {
            document.removeEventListener('mousedown', handleClickOutside);
        };
    }, [showDropdown]);

    const handleLogout = () => {
        localStorage.removeItem('authToken');
        localStorage.removeItem('user');
        setIsAuthenticated(false);
        setUser(null);
        setShowDropdown(false);
        window.location.href = '/';
    };

    // Xử lý click logo - về homepage + scroll to top
    const handleLogoClick = (e: React.MouseEvent) => {
        e.preventDefault();
        navigate('/');
        window.scrollTo({ top: 0, behavior: 'smooth' });
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
            className="w-full border-b transition-colors duration-300 backdrop-blur-xl backdrop-saturate-150 shadow-[0_8px_24px_rgba(0,0,0,0.25)]"
            style={{
                position: 'fixed',
                top: 0,
                left: 0,
                right: 0,
                zIndex: 99999,
                background: 'linear-gradient(180deg, rgba(32,40,61,0.95) 0%, rgba(46,58,87,0.90) 100%)',
                borderBottom: '1px solid rgba(212, 185, 138, 0.25)'
            }}
        >
            <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
                {/* Row - Tăng height từ h-16 lên h-20 */}
                <div className="flex justify-between items-center h-20">
                    {/* Logo - Tăng kích thước */}
                    <a
                        href="/"
                        onClick={handleLogoClick}
                        className="flex items-center gap-3 no-underline hover:opacity-90 transition-opacity cursor-pointer"
                        style={{ position: 'relative', zIndex: 100000 }}
                    >
                        <img
                            src={logoImg}
                            alt="Cây Gia Phả"
                            style={{ width: '64px', height: '64px' }}
                            className="rounded-lg"
                        />
                        <div className="leading-tight">
                            <div className="text-white font-semibold text-lg">
                                Con Rồng Cháu Tiên
                            </div>
                            <div className="text-[13px] tracking-wide text-[#d1b98a]/80">
                                LegacyMap
                            </div>
                        </div>
                    </a>

                    {/* Desktop menu - Tăng font size và gap */}
                    <div className="hidden md:flex items-center gap-12" style={{ position: 'relative', zIndex: 100000 }}>
                        <a href="#features" className={`${linkColor} no-underline text-[15px] font-medium`}>
                            Tính năng
                        </a>
                        <a href="#about" className={`${linkColor} no-underline text-[15px] font-medium`}>
                            Về chúng tôi
                        </a>
                        <a href="#testimonials" className={`${linkColor} no-underline text-[15px] font-medium`}>
                            Khách hàng
                        </a>
                        <a href="#cta" className={`${linkColor} no-underline text-[15px] font-medium`}>
                            Liên hệ
                        </a>
                    </div>

                    {/* Actions - Tăng kích thước avatar và button */}
                    <div className="hidden md:flex items-center gap-3" style={{ position: 'relative', zIndex: 100000 }}>
                        {isAuthenticated ? (
                            <div style={{ position: 'relative' }} ref={dropdownRef}>
                                <button
                                    onClick={() => setShowDropdown(!showDropdown)}
                                    className="flex items-center gap-3 hover:opacity-90 transition-opacity"
                                >
                                    {getAvatarUrl() ? (
                                        <img
                                            src={getAvatarUrl() as string}
                                            alt={getDisplayName()}
                                            className="w-12 h-12 rounded-full object-cover shadow-lg ring-2 ring-[rgba(209,185,138,0.35)]"
                                            referrerPolicy="no-referrer"
                                        />
                                    ) : (
                                        <div
                                            className={`w-12 h-12 rounded-full ${
                                                isAdmin()
                                                    ? 'bg-gradient-to-br from-[#b49e7b] to-[#d1b98a]'
                                                    : 'bg-gradient-to-br from-[#2e3a57] to-[#20283d]'
                                            } flex items-center justify-center text-white font-semibold text-lg shadow-lg ring-2 ring-[rgba(209,185,138,0.35)]`}
                                        >
                                            {getInitials()}
                                        </div>
                                    )}
                                    <span className="hidden md:block text-[15px] font-medium text-white">
                                        {getDisplayName()}
                                    </span>
                                </button>

                                {showDropdown && (
                                    <div
                                        className="rounded-lg shadow-xl py-2 border"
                                        style={{
                                            position: 'absolute',
                                            right: 0,
                                            marginTop: '0.75rem',
                                            width: '15rem',
                                            zIndex: 100000,
                                            background: 'linear-gradient(180deg, rgba(26,31,46,0.95) 0%, rgba(32,40,61,0.95) 100%)',
                                            borderColor: 'rgba(209,185,138,0.25)'
                                        }}
                                    >
                                        <div className="px-4 py-3 border-b border-[rgba(209,185,138,0.2)]">
                                            <p className="text-[15px] font-semibold text-white flex items-center gap-2">
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
                                            className={`w-full px-4 py-2.5 text-left text-[14px] hover:bg-white/5 transition flex items-center gap-2 ${
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
                                            className="w-full px-4 py-2.5 text-left text-[14px] text-white hover:bg-white/5 transition flex items-center gap-2"
                                        >
                                            <User className="h-4 w-4" />
                                            Trang cá nhân
                                        </button>

                                        <button
                                            onClick={() => {
                                                setShowDropdown(false);
                                                navigate('/events');
                                            }}
                                            className="w-full px-4 py-2.5 text-left text-[14px] text-white hover:bg-white/5 transition flex items-center gap-2"
                                        >
                                            <CalendarFold className="h-4 w-4" />
                                            Sự kiện
                                        </button>

                                        <button
                                            onClick={() => {
                                                setShowDropdown(false);
                                                navigate('/notifications');
                                            }}
                                            className="w-full px-4 py-2.5 text-left text-[14px] text-white hover:bg-white/5 transition flex items-center gap-2"
                                        >
                                            <Bell className="h-4 w-4" />
                                            Thông báo
                                        </button>

                                        <hr className="my-2 border-[rgba(209,185,138,0.25)]" />

                                        <button
                                            onClick={handleLogout}
                                            className="w-full px-4 py-2.5 text-left text-[14px] text-red-300 hover:bg-red-500/10 transition flex items-center gap-2"
                                        >
                                            <LogOut className="h-4 w-4" />
                                            Đăng xuất
                                        </button>
                                    </div>
                                )}
                            </div>
                        ) : isHomePage ? (
                            <>
                                <Button
                                    variant="outline"
                                    size="sm"
                                    onClick={onLoginClick}
                                    className="border-[rgba(209,185,138,0.4)] text-white hover:bg-white/10 h-11 px-5 text-[15px]"
                                >
                                    Đăng nhập
                                </Button>
                                <Button
                                    variant="primary"
                                    size="sm"
                                    onClick={onSignupClick}
                                    className="font-semibold text-[#20283d] bg-gradient-to-br from-[#b49e7b] to-[#d1b98a] shadow-[0_6px_20px_rgba(209,185,138,0.35)] hover:brightness-110 transition-all h-11 px-5 text-[15px]"
                                >
                                    Đăng ký
                                </Button>
                            </>
                        ) : (
                            <Link to="/">
                                <Button
                                    variant="outline"
                                    size="sm"
                                    className="border-[rgba(209,185,138,0.4)] text-white hover:bg-white/10 h-11 px-5 text-[15px]"
                                >
                                    Trang chủ
                                </Button>
                            </Link>
                        )}
                    </div>

                    {/* Mobile menu button - Tăng kích thước icon */}
                    <button
                        className="md:hidden text-[#d1b98a]"
                        onClick={() => setIsOpen((v) => !v)}
                        aria-label="Toggle Menu"
                        style={{ position: 'relative', zIndex: 100000 }}
                    >
                        <Menu size={26} />
                    </button>
                </div>

                {/* Mobile menu - Tăng padding và font size */}
                {isOpen && (
                    <div className="md:hidden pb-4 space-y-2">
                        <a href="#features" className="block text-white hover:text-[#d1b98a] py-3 text-[15px]">
                            Tính năng
                        </a>
                        <a href="#about" className="block text-white hover:text-[#d1b98a] py-3 text-[15px]">
                            Về chúng tôi
                        </a>
                        <a href="#testimonials" className="block text-white hover:text-[#d1b98a] py-3 text-[15px]">
                            Khách hàng
                        </a>
                        <a href="#cta" className="block text-white hover:text-[#d1b98a] py-3 text-[15px]">
                            Liên hệ
                        </a>

                        {!isAuthenticated && isHomePage && (
                            <div className="flex gap-2 pt-2">
                                <Button
                                    variant="outline"
                                    className="flex-1 text-white border-[rgba(209,185,138,0.4)] hover:bg-white/10 h-11 text-[15px]"
                                    onClick={onLoginClick}
                                >
                                    Đăng nhập
                                </Button>
                                <Button
                                    className="flex-1 font-semibold text-[#20283d] bg-gradient-to-br from-[#b49e7b] to-[#d1b98a] shadow-[0_6px_20px_rgba(209,185,138,0.35)] hover:brightness-110 transition-all h-11 text-[15px]"
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