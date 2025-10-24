// src/components/admin/AdminLayout.tsx
import React from 'react';
import { Link, useLocation } from 'react-router-dom';

interface AdminLayoutProps {
    children: React.ReactNode;
}

const AdminLayout: React.FC<AdminLayoutProps> = ({ children }) => {
    const location = useLocation();

    const menuItems = [
        { path: '/admin', label: 'Tổng Quan', icon: '🏠' },
        { path: '/admin/users', label: 'Người Dùng', icon: '👥' },
        { path: '/admin/trees', label: 'Gia Phả', icon: '🌲' },
        { path: '/admin/settings', label: 'Cài Đặt', icon: '⚙️' },
    ];

    const isActive = (path: string) => location.pathname === path;

    return (
        <div className="min-h-screen flex bg-[#0a4a9e]">
            {/* Sidebar */}
            <aside className="w-80 bg-[#084289] border-r border-[#0a4a9e] flex flex-col">
                {/* Logo Section */}
                <div className="p-6 border-b border-[#0a4a9e]">
                    <div className="flex items-center gap-3 mb-2">
                        <div className="w-12 h-12 bg-white rounded-lg flex items-center justify-center">
                            <span className="text-2xl">🏛️</span>
                        </div>
                        <div>
                            <h1 className="text-white font-bold text-lg">Gia Phả Admin</h1>
                        </div>
                    </div>
                </div>

                {/* Menu Items */}
                <nav className="flex-1 p-4">
                    {menuItems.map((item) => {
                        const active = isActive(item.path);
                        return (
                            <Link
                                key={item.path}
                                to={item.path}
                                className={`
                                    flex items-center gap-4 px-6 py-4 mb-2 rounded-xl transition-all
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

                {/* Logout Button */}
                <div className="p-4 border-t border-[#0a4a9e]">
                    <Link
                        to="/"
                        className="flex items-center gap-4 px-6 py-4 rounded-xl text-red-300 hover:bg-red-500/20 transition-all"
                    >
                        <span className="text-2xl">🚪</span>
                        <span className="font-medium">Đăng Xuất</span>
                    </Link>
                </div>
            </aside>

            {/* Main Content - No Top Bar */}
            <main className="flex-1 overflow-auto">
                <div className="p-8">
                    {children}
                </div>
            </main>
        </div>
    );
};

export default AdminLayout;