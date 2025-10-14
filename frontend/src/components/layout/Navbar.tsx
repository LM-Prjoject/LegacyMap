// src/components/layout/Navbar.tsx
import React, { useState, useEffect } from 'react';
import { Link, useNavigate } from 'react-router-dom';
import { LogOut, User, Settings } from 'lucide-react';
import Button from './Button';

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

    // ✅ Kiểm tra authentication status
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

        // ✅ Listen cho storage changes (khi login từ tab khác)
        window.addEventListener('storage', checkAuth);
        return () => window.removeEventListener('storage', checkAuth);
    }, []);

    // ✅ Xử lý logout
    const handleLogout = () => {
        localStorage.removeItem('authToken');
        localStorage.removeItem('user');
        setIsAuthenticated(false);
        setUser(null);
        setShowDropdown(false);
        window.location.href = '/'; // Reload về homepage
    };

    // ✅ Lấy tên hiển thị (ưu tiên fullName từ profile)
    const getDisplayName = () => {
        if (!user) return 'User';
        const profile = user.profile || user.userProfile;
        const fullName = user.fullName || profile?.fullName;
        return fullName || user.username || user.email?.split('@')[0] || 'User';
    };

    // ✅ Lấy URL avatar (ưu tiên avatar_url từ profile)
    const getAvatarUrl = () => {
        if (!user) return null as string | null;
        const profile = user.profile || user.userProfile;
        return user.avatarUrl || profile?.avatarUrl || null;
    };

    // ✅ Lấy chữ cái đầu cho avatar fallback
    const getInitials = () => {
        const name = getDisplayName();
        return name.charAt(0).toUpperCase();
    };

    return (
        <nav className="bg-background/95 backdrop-blur shadow-sm py-4 sticky top-0 z-40 border-b border-border">
            <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
                <div className="flex justify-between items-center">
                    {/* Logo */}
                    <Link to="/" className="flex items-baseline gap-2">
                        <div className="text-2xl font-extrabold text-primary tracking-tight">Cây Gia Phả</div>
                        <div className="text-sm text-muted-foreground italic">Con Rồng Cháu Tiên</div>
                    </Link>

                    {/* Navigation Links */}
                    <div className="hidden md:flex items-center gap-8">
                        <a href="#features" className="text-foreground hover:text-primary transition-colors">Tính năng</a>
                        <a href="#about" className="text-foreground hover:text-primary transition-colors">Về chúng tôi</a>
                        <a href="#testimonials" className="text-foreground hover:text-primary transition-colors">Khách hàng</a>
                        <a href="#cta" className="text-foreground hover:text-primary transition-colors">Liên hệ</a>
                    </div>

                    {/* Auth Section */}
                    <div className="flex items-center gap-3">
                        {isAuthenticated ? (
                            // ✅ Hiển thị khi đã đăng nhập
                            <div className="relative">
                                <button
                                    onClick={() => setShowDropdown(!showDropdown)}
                                    className="flex items-center gap-2 hover:opacity-80 transition-opacity"
                                >
                                    {/* Avatar */}
                                    {getAvatarUrl() ? (
                                        <img
                                            src={getAvatarUrl() as string}
                                            alt={getDisplayName()}
                                            className="w-10 h-10 rounded-full object-cover shadow-lg"
                                            referrerPolicy="no-referrer"
                                        />
                                    ) : (
                                        <div className="w-10 h-10 rounded-full bg-gradient-to-br from-blue-500 to-blue-700 flex items-center justify-center text-white font-semibold shadow-lg">
                                            {getInitials()}
                                        </div>
                                    )}
                                    <span className="hidden md:block text-sm font-medium text-gray-700">
                                        {getDisplayName()}
                                    </span>
                                </button>

                                {/* Dropdown Menu */}
                                {showDropdown && (
                                    <>
                                        {/* Overlay để đóng dropdown khi click bên ngoài */}
                                        <div
                                            className="fixed inset-0 z-10"
                                            onClick={() => setShowDropdown(false)}
                                        />

                                        <div className="absolute right-0 mt-2 w-56 bg-white rounded-lg shadow-xl border border-gray-200 py-2 z-20">
                                            {/* User Info */}
                                            <div className="px-4 py-3 border-b border-gray-100">
                                                <p className="text-sm font-semibold text-gray-900">{getDisplayName()}</p>
                                                <p className="text-xs text-gray-500 truncate">{user?.email}</p>
                                            </div>

                                            {/* Menu Items */}
                                            <button
                                                onClick={() => {
                                                    setShowDropdown(false);
                                                    navigate('/dashboard');
                                                }}
                                                className="w-full px-4 py-2 text-left text-sm text-gray-700 hover:bg-gray-100 flex items-center gap-2"
                                            >
                                                <User className="h-4 w-4" />
                                                Dashboard
                                            </button>

                                            <button
                                                onClick={() => {
                                                    setShowDropdown(false);
                                                    // Navigate to profile/settings page
                                                    alert('Tính năng đang phát triển');
                                                }}
                                                className="w-full px-4 py-2 text-left text-sm text-gray-700 hover:bg-gray-100 flex items-center gap-2"
                                            >
                                                <Settings className="h-4 w-4" />
                                                Cài đặt
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
                        ) : (
                            // ✅ Hiển thị khi chưa đăng nhập
                            isHomePage ? (
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
                                <Link to="/">
                                    <Button variant="outline" size="sm">Trang chủ</Button>
                                </Link>
                            )
                        )}
                    </div>
                </div>
            </div>
        </nav>
    );
};

export default Navbar;