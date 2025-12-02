import React, { useEffect } from 'react';
import { Link, useLocation } from 'react-router-dom';
import logoImg from '@/assets/logo.png';
import { LayoutDashboard, Users, TreePine } from "lucide-react";

interface AdminLayoutProps {
    children: React.ReactNode;
}

const AdminLayout: React.FC<AdminLayoutProps> = ({ children }) => {
    const location = useLocation();

    useEffect(() => {
        const navbar = document.querySelector('nav');
        if (navbar) {
            (navbar as HTMLElement).style.display = 'none';
        }

        document.body.style.paddingTop = '0';
        document.body.style.backgroundColor = '#20283d';

        return () => {
            if (navbar) {
                (navbar as HTMLElement).style.display = '';
            }
            document.body.style.paddingTop = '';
            document.body.style.backgroundColor = '';
        };
    }, []);

    const menuItems = [
        {
            path: '/admin/dashboard',
            label: 'Tổng Quan',
            icon: <LayoutDashboard size={25} />,
        },
        {
            path: '/admin/users',
            label: 'Người Dùng',
            icon: <Users size={25} />,
        },
        {
            path: '/admin/trees',
            label: 'Gia Phả',
            icon: <TreePine size={25}  />,
        },
    ];

    const isActive = (path: string) => {
        if (path === '/admin/dashboard') {
            return location.pathname === '/admin' || location.pathname === '/admin/dashboard';
        }
        return location.pathname.startsWith(path);
    };

    return (
        <>
            <style dangerouslySetInnerHTML={{ __html: `
                .admin-sidebar-link {
                    display: flex !important;
                    visibility: visible !important;
                    opacity: 1 !important;
                    align-items: center !important;
                    gap: 16px !important;
                    padding: 16px 24px !important;
                    margin-bottom: 8px !important;
                    border-radius: 12px !important;
                    text-decoration: none !important;
                    transition: all 0.3s ease !important;
                    cursor: pointer !important;
                    font-size: 16px !important;
                    font-weight: 500 !important;
                }
                
                .admin-sidebar-link.active {
                    background: linear-gradient(90deg, #d1b98a 0%, #f4e9c8 100%) !important;
                    color: #20283d !important;
                    font-weight: 600 !important;
                    box-shadow: 0 4px 12px rgba(209, 185, 138, 0.2) !important;
                }
                
                .admin-sidebar-link:not(.active) {
                    background: transparent !important;
                    color: #e5e7eb !important;
                }
                
                .admin-sidebar-link:not(.active):hover {
                    background: rgba(46, 58, 87, 0.6) !important;
                    color: #f4e9c8 !important;
                }
                
                .admin-sidebar-nav {
                    display: flex !important;
                    flex-direction: column !important;
                    visibility: visible !important;
                    opacity: 1 !important;
                }
            `}} />

            <div style={{
                position: 'fixed',
                inset: 0,
                width: '100vw',
                height: '100vh',
                display: 'flex',
                background: 'linear-gradient(135deg, #20283d 0%, #2e3a57 100%)',
                color: 'white',
                overflow: 'hidden'
            }}>
                <div style={{
                    position: 'absolute',
                    top: 0,
                    left: '50%',
                    transform: 'translateX(-50%)',
                    width: '900px',
                    height: '400px',
                    background: 'rgba(209, 185, 138, 0.1)',
                    filter: 'blur(120px)',
                    borderRadius: '50%',
                    pointerEvents: 'none'
                }} />

                <aside style={{
                    width: '300px',
                    minWidth: '300px',
                    maxWidth: '300px',
                    height: '100vh',
                    background: 'rgba(27, 34, 51, 0.95)',
                    borderRight: '1px solid rgba(209, 185, 138, 0.2)',
                    boxShadow: '4px 0 15px rgba(0,0,0,0.25)',
                    display: 'flex',
                    flexDirection: 'column',
                    position: 'relative',
                    zIndex: 50,
                    flexShrink: 0
                }}>
                    <Link
                        to="/"
                        style={{
                            height: '120px',
                            minHeight: '120px',
                            maxHeight: '120px',
                            padding: '24px',
                            borderBottom: '1px solid rgba(209, 185, 138, 0.2)',
                            display: 'flex',
                            alignItems: 'center',
                            flexShrink: 0,
                            textDecoration: 'none',
                            cursor: 'pointer',
                            transition: 'background 0.3s ease'
                        }}
                        onMouseEnter={(e) => {
                            e.currentTarget.style.background = 'rgba(32, 40, 61, 0.6)';
                        }}
                        onMouseLeave={(e) => {
                            e.currentTarget.style.background = 'transparent';
                        }}
                    >
                        <div style={{ display: 'flex', alignItems: 'center', gap: '16px' }}>
                            <img
                                src={logoImg}
                                alt="Logo"
                                style={{
                                    width: '64px',
                                    height: '64px',
                                    objectFit: 'contain',
                                    filter: 'drop-shadow(0 0 8px rgba(209, 185, 138, 0.3))'
                                }}
                            />
                            <div>
                                <h1 style={{
                                    color: '#d1b98a',
                                    fontSize: '16px',
                                    fontWeight: 600,
                                    lineHeight: 1.2,
                                    margin: 0
                                }}>
                                    Con Rồng Cháu Tiên
                                </h1>
                                <p style={{
                                    color: 'rgba(209, 185, 138, 0.7)',
                                    fontSize: '12px',
                                    marginTop: '2px',
                                    marginBottom: 0
                                }}>
                                    LegacyMap Admin
                                </p>
                            </div>
                        </div>
                    </Link>

                    <nav
                        className="admin-sidebar-nav"
                        style={{
                            flex: 1,
                            padding: '16px',
                            marginTop: '8px',
                            overflowY: 'auto',
                            display: 'flex',
                            flexDirection: 'column'
                        }}
                    >
                        {menuItems.map((item) => {
                            const active = isActive(item.path);
                            return (
                                <Link
                                    key={item.path}
                                    to={item.path}
                                    className={`admin-sidebar-link ${active ? 'active' : ''}`}
                                >
                                    <span
                                        style={{
                                            display: "flex",
                                            alignItems: "center",
                                            color: active ? "#20283d" : "#e5e7eb"
                                        }}
                                    >
                                    {item.icon}
                                    </span>
                                    <span>{item.label}</span>
                                </Link>
                            );
                        })}
                    </nav>
                </aside>

                <main style={{
                    flex: 1,
                    display: 'flex',
                    flexDirection: 'column',
                    position: 'relative',
                    zIndex: 10,
                    minWidth: 0,
                    height: '100vh',
                    maxHeight: '100vh'
                }}>
                    <div style={{
                        height: '120px',
                        minHeight: '120px',
                        maxHeight: '120px',
                        background: 'rgba(32, 40, 61, 0.75)',
                        backdropFilter: 'blur(10px)',
                        borderBottom: '1px solid rgba(209, 185, 138, 0.2)',
                        padding: '0 32px',
                        display: 'flex',
                        alignItems: 'center',
                        justifyContent: 'space-between',
                        boxShadow: '0 1px 3px rgba(0,0,0,0.1)',
                        flexShrink: 0
                    }}>
                        <h2 style={{
                            fontSize: '24px',
                            fontWeight: 700,
                            background: 'linear-gradient(90deg, #d1b98a 0%, #f4e9c8 100%)',
                            WebkitBackgroundClip: 'text',
                            WebkitTextFillColor: 'transparent',
                            margin: 0
                        }}>
                            Khu vực quản trị
                        </h2>
                        <span style={{
                            fontSize: '14px',
                            color: 'rgba(244, 233, 200, 0.8)',
                            fontStyle: 'italic',
                            fontWeight: 500
                        }}>
                            LegacyMap Admin
                        </span>
                    </div>

                    <div style={{
                        flex: 1,
                        height: 'calc(100vh - 120px)',
                        maxHeight: 'calc(100vh - 120px)',
                        overflowY: 'auto',
                        overflowX: 'hidden',
                        padding: '32px'
                    }}>
                        {children}
                    </div>
                </main>

                <div style={{
                    position: 'fixed',
                    bottom: 0,
                    left: '50%',
                    transform: 'translateX(-50%)',
                    width: '1000px',
                    height: '300px',
                    background: 'rgba(209, 185, 138, 0.1)',
                    filter: 'blur(120px)',
                    borderRadius: '50%',
                    pointerEvents: 'none',
                    zIndex: 0
                }} />
            </div>
        </>
    );
};

export default AdminLayout;