import React, { useState, useEffect } from 'react';
import { Link, useNavigate, useLocation } from 'react-router-dom';
import { LogOut, User, Album} from 'lucide-react';
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
    const location = useLocation();

    useAutoLogout(30);

    const isDashboard =
        location.pathname.startsWith('/dashboard') ||
        location.pathname.startsWith('/trees') ||
        location.pathname.startsWith('/admin');

    const linkColor = isDashboard
        ? 'text-white hover:text-amber-400'
        : 'text-slate-900 hover:text-amber-600';

    useEffect(() => {
        const checkAuth = () => {
            const token = localStorage.getItem('authToken');
            const userStr = localStorage.getItem('user');

            if (token && userStr) {
                try {
                    const userData = JSON.parse(userStr);
                    setUser(userData);
                    setIsAuthenticated(true);
                } catch (error) {
                    console.error('Error parsing user data:', error);
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

    // ✅ NEW: Check if user is admin
    const isAdmin = () => {
        if (!user) return false;
        return user.roleName === 'admin' || user.role === 'admin';
    };

    // ✅ NEW: Get dashboard URL based on role
    const getDashboardUrl = () => {
        return isAdmin() ? '/admin' : '/dashboard';
    };

    return (
        <nav
            className={`${
                isDashboard ? 'bg-transparent border-b shadow-none' : 'bg-background/95 border-b border-border shadow-sm'
            } backdrop-blur py-4 sticky top-0 z-40 transition-colors duration-300`}
        >
            <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
                <div className="flex justify-between items-center">
                    <Link
                        to="/"
                        className={`flex items-center gap-4 no-underline hover:no-underline ${
                            isDashboard ? 'text-white/80' : 'text-slate-600'
                        }`}
                    >
                        <img
                            src={logoImg}
                            alt="Cây Gia Phả"
                            style={{ width: '75px', height: '75px' }}
                        />
                        <div className="text-base italic">
                            Con Rồng Cháu Tiên
                        </div>
                    </Link>

                    <div className="hidden md:flex items-center gap-16">
                        <a
                            href="#features"
                            className={`${linkColor} no-underline hover:no-underline transition-colors`}
                        >
                            Tính năng
                        </a>
                        <a
                            href="#about"
                            className={`${linkColor} no-underline hover:no-underline transition-colors`}
                        >
                            Về chúng tôi
                        </a>
                        <a
                            href="#testimonials"
                            className={`${linkColor} no-underline hover:no-underline transition-colors`}
                        >
                            Khách hàng
                        </a>
                        <a
                            href="#cta"
                            className={`${linkColor} no-underline hover:no-underline transition-colors`}
                        >
                            Liên hệ
                        </a>
                    </div>

                    <div className="flex items-center gap-3">
                        {isAuthenticated ? (
                            <div className="relative">
                                <button
                                    onClick={() => setShowDropdown(!showDropdown)}
                                    className="flex items-center gap-2 hover:opacity-80 transition-opacity"
                                >
                                    {getAvatarUrl() ? (
                                        <img
                                            src={getAvatarUrl() as string}
                                            alt={getDisplayName()}
                                            className="w-10 h-10 rounded-full object-cover shadow-lg"
                                            referrerPolicy="no-referrer"
                                        />
                                    ) : (
                                        <div className={`w-10 h-10 rounded-full ${
                                            isAdmin()
                                                ? 'bg-gradient-to-br from-amber-500 to-amber-700'
                                                : 'bg-gradient-to-br from-blue-500 to-blue-700'
                                        } flex items-center justify-center text-white font-semibold shadow-lg`}>
                                            {getInitials()}
                                        </div>
                                    )}
                                    <span
                                        className={`hidden md:block text-sm font-medium ${
                                            isDashboard ? 'text-white' : 'text-gray-700'
                                        }`}
                                    >
                                        {getDisplayName()}
                                    </span>
                                </button>

                                {showDropdown && (
                                    <>
                                        <div
                                            className="fixed inset-0 z-10"
                                            onClick={() => setShowDropdown(false)}
                                        />

                                        <div className="absolute right-0 mt-2 w-56 bg-white rounded-lg shadow-xl border border-gray-200 py-2 z-20">
                                            <div className="px-4 py-3 border-b border-gray-100">
                                                <p className="text-sm font-semibold text-gray-900 flex items-center gap-2">
                                                    {getDisplayName()}
                                                    {isAdmin() && (
                                                        <span className="px-2 py-0.5 bg-amber-100 text-amber-700 text-xs font-bold rounded">
                                                            ADMIN
                                                        </span>
                                                    )}
                                                </p>
                                                <p className="text-xs text-gray-500 truncate">
                                                    {user?.email}
                                                </p>
                                            </div>

                                            {/* ✅ DASHBOARD BUTTON - Redirects based on role */}
                                            <button
                                                onClick={() => {
                                                    setShowDropdown(false);
                                                    navigate(getDashboardUrl());
                                                }}
                                                className={`w-full px-4 py-2 text-left text-sm hover:bg-gray-100 flex items-center gap-2 ${
                                                    isAdmin() ? 'text-amber-700 font-medium' : 'text-gray-700'
                                                }`}
                                            >
                                                {isAdmin() ? (
                                                    <Album className="h-4 w-4" />
                                                ) : (
                                                    <Album className="h-4 w-4" />
                                                )}
                                                {isAdmin() ? 'Admin Dashboard' : 'Dashboard'}
                                            </button>

                                            <button
                                                onClick={() => {
                                                    setShowDropdown(false);
                                                    navigate('/profile');
                                                }}
                                                className="w-full px-4 py-2 text-left text-sm text-gray-700 hover:bg-gray-100 flex items-center gap-2"
                                            >
                                                <User className="h-4 w-4" />
                                                Trang cá nhân
                                            </button>
                                            <hr className="my-2" />

                                            <button
                                                onClick={handleLogout}
                                                className="w-full px-4 py-2 text-left text-sm text-red-600 hover:bg-red-50 flex items-center gap-2"
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
                                <Button variant="outline" size="sm" onClick={onLoginClick}>
                                    Đăng nhập
                                </Button>
                                <Button variant="primary" size="sm" onClick={onSignupClick}>
                                    Đăng ký
                                </Button>
                            </>
                        ) : (
                            <Link to="/">
                                <Button variant="outline" size="sm">
                                    Trang chủ
                                </Button>
                            </Link>
                        )}
                    </div>
                </div>
            </div>
        </nav>
    );
};

export default Navbar;