// src/components/admin/AdminLayout.tsx
import React from 'react';
import { Link, useLocation } from 'react-router-dom';
import logoImg from '@/assets/logo.png';

interface AdminLayoutProps {
    children: React.ReactNode;
}

const AdminLayout: React.FC<AdminLayoutProps> = ({ children }) => {
    const location = useLocation();

    const menuItems = [
        { path: '/admin/dashboard', label: 'Tổng Quan', icon: '🏠' },
        { path: '/admin/users', label: 'Người Dùng', icon: '👥' },
        { path: '/admin/trees', label: 'Gia Phả', icon: '🌲' },
        { path: '/admin/settings', label: 'Cài Đặt', icon: '⚙️' },
    ];

    const isActive = (path: string) => {
        // ✅ Special case: Tổng Quan active khi ở /admin hoặc /admin/dashboard
        if (path === '/admin/dashboard') {
            return location.pathname === '/admin' || location.pathname === '/admin/dashboard';
        }
        return location.pathname === path;
    };

    return (
        <div className="min-h-screen flex bg-gradient-to-br from-[#20283d] to-[#2e3a57] text-white relative overflow-hidden">
            {/* Ánh sáng nền mờ vàng kim */}
            <div className="absolute top-0 left-1/2 -translate-x-1/2 w-[900px] h-[400px] bg-[#d1b98a]/10 blur-3xl rounded-full pointer-events-none" />
            <div className="absolute bottom-0 right-1/3 w-[700px] h-[300px] bg-[#b49e7b]/10 blur-3xl rounded-full pointer-events-none" />

            {/* Sidebar */}
            <aside className="w-80 bg-[#1b2233]/95 border-r border-[#d1b98a]/20 shadow-[4px_0_15px_rgba(0,0,0,0.25)] flex flex-col z-20 relative">
                {/* Logo Section */}
                <Link
                    to="/"
                    className="p-6 border-b border-[#d1b98a]/20 hover:bg-[#20283d]/60 transition-all no-underline"
                >
                    <div className="flex items-center gap-4">
                        <img
                            src={logoImg}
                            alt="Logo"
                            className="w-16 h-16 object-contain drop-shadow-[0_0_8px_rgba(209,185,138,0.3)]"
                        />
                        <div>
                            <h1 className="text-[#d1b98a]/90 text-sm italic font-medium">
                                Con Rồng Cháu Tiên
                            </h1>
                        </div>
                    </div>
                </Link>

                {/* Menu Items */}
                <nav className="flex-1 p-4 mt-2">
                    {menuItems.map((item) => {
                        const active = isActive(item.path);
                        return (
                            <Link
                                key={item.path}
                                to={item.path}
                                className={`
                  flex items-center gap-4 px-6 py-4 mb-2 rounded-xl transition-all no-underline
                  ${
                                    active
                                        ? 'bg-gradient-to-r from-[#d1b98a] to-[#f4e9c8] text-[#20283d] font-semibold shadow-lg shadow-[#d1b98a]/20'
                                        : 'text-gray-200 hover:text-[#f4e9c8] hover:bg-[#2e3a57]/60 hover:shadow-md hover:shadow-[#d1b98a]/10'
                                }
                `}
                            >
                                <span className="text-2xl">{item.icon}</span>
                                <span className="text-base">{item.label}</span>
                            </Link>
                        );
                    })}
                </nav>
            </aside>

            {/* Main Content */}
            <main className="flex-1 overflow-auto relative z-10">
                {/* Thanh tiêu đề cố định */}
                <div className="sticky top-0 z-10 backdrop-blur-md bg-[#20283d]/75 border-b border-[#d1b98a]/20 px-8 py-4 flex items-center justify-between shadow-sm">
                    <h2 className="text-xl font-bold bg-gradient-to-r from-[#d1b98a] to-[#f4e9c8] bg-clip-text text-transparent select-none">
                        Khu vực quản trị
                    </h2>
                    <span className="text-sm text-[#f4e9c8]/80 italic">LegacyMap Admin</span>
                </div>

                {/* Nội dung con */}
                <div className="p-8">{children}</div>
            </main>

            {/* Ánh sáng nền phụ */}
            <div className="absolute bottom-0 left-1/2 -translate-x-1/2 w-[1000px] h-[300px] bg-[#d1b98a]/10 blur-[120px] rounded-full pointer-events-none" />
        </div>
    );
};

export default AdminLayout;
