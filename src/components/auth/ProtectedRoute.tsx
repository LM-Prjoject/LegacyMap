// src/components/auth/ProtectedRoute.tsx
import React from 'react';
import { Navigate, useLocation } from 'react-router-dom';

interface ProtectedRouteProps {
    children: React.ReactNode;
    requiredRole?: string;
}

const ProtectedRoute: React.FC<ProtectedRouteProps> = ({ children, requiredRole }) => {
    const location = useLocation();

    const isAuthenticated = (): boolean => {
        const token = localStorage.getItem('authToken');
        const user = localStorage.getItem('user');
        return !!(token && user);
    };

    const getUserRole = (): string | null => {
        const userStr = localStorage.getItem('user');
        if (!userStr) return null;
        try {
            const user = JSON.parse(userStr);
            // ✅ FIX: Kiểm tra cả role và roleName
            return user.role || user.roleName || null;
        } catch (error) {
            console.error('❌ Error parsing user data:', error);
            return null;
        }
    };

    const getUserId = (): string | null => {
        const userStr = localStorage.getItem('user');
        if (!userStr) return null;
        try {
            const user = JSON.parse(userStr);
            return user.id || user.userId || null;
        } catch (error) {
            console.error('❌ Error parsing user ID:', error);
            return null;
        }
    };

    // Debug info
    if (process.env.NODE_ENV === 'development') {
        console.log('🔍 ProtectedRoute Debug:', {
            isAuthenticated: isAuthenticated(),
            userRole: getUserRole(),
            userId: getUserId(),
            requiredRole,
            path: location.pathname
        });
    }

    if (!isAuthenticated()) {
        console.log('🔐 Redirecting to login - Not authenticated');
        return (
            <Navigate
                to="/signin"
                replace
                state={{ from: location }}
            />
        );
    }

    // ✅ Check role nếu required
    if (requiredRole) {
        const userRole = getUserRole();
        const normalizedUserRole = userRole?.toUpperCase();
        const normalizedRequiredRole = requiredRole.toUpperCase();

        if (normalizedUserRole !== normalizedRequiredRole) {
            console.warn(`🚫 Access denied: User role ${normalizedUserRole} does not match required role ${normalizedRequiredRole}`);

            // Redirect về home page nếu không có quyền
            return <Navigate to="/" replace />;
        }
    }

    return <>{children}</>;
};

export default ProtectedRoute;