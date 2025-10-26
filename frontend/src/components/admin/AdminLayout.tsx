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
        <div className="min-h-screen flex bg-[#0a4a9e]">
            {/* Sidebar */}
            <aside className="w-80 bg-[#084289] border-r border-[#0a4a9e] flex flex-col">
                {/* ✅ Logo Section - Click để về homepage */}
                <Link
                    to="/"
                    className="p-6 border-b border-[#0a4a9e] hover:bg-[#0a4a9e] transition-colors no-underline"
                >
                    <div className="flex items-center gap-4">
                        <img
                            src={logoImg}
                            alt="Logo"
                            className="w-16 h-16 object-contain"
                        />
                        <div>
                            <h1 className="text-white/60 text-sm italic">Con Rồng Cháu Tiên</h1>
                        </div>
                    </div>
                </Link>

                {/* Menu Items */}
                <nav className="flex-1 p-4">
                    {menuItems.map((item) => {
                        const active = isActive(item.path);
                        return (
                            <Link
                                key={item.path}
                                to={item.path}
                                className={`
                                    flex items-center gap-4 px-6 py-4 mb-2 rounded-xl transition-all no-underline
                                    ${active
                                    ? 'bg-[#D1B066] text-[#084289]'
                                    : 'text-white hover:bg-[#0a4a9e]'
                                }
                                `}
                            >
                                <span className="text-2xl">{item.icon}</span>
                                <span className="font-medium text-base">{item.label}</span>
                            </Link>
                        );
                    })}
                </nav>

                {/* ✅ REMOVED: Logout button section */}
            </aside>

            {/* Main Content */}
            <main className="flex-1 overflow-auto">
                <div className="p-8">
                    {children}
                </div>
            </main>
        </div>
    );
};

export default AdminLayout;