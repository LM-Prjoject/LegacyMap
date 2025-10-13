import React from 'react';
import { Navigate, useLocation } from 'react-router-dom';

interface ProtectedRouteProps {
    children: React.ReactNode;
}

const ProtectedRoute: React.FC<ProtectedRouteProps> = ({ children }) => {
    const location = useLocation();

    // Kiểm tra authentication với Spring Boot token
    const isAuthenticated = () => {
        const token = localStorage.getItem('authToken'); // Key khớp với SignIn.tsx
        const user = localStorage.getItem('user');
        return !!(token && user);
    };

    if (!isAuthenticated()) {
        return (
            <Navigate
                to="/signin"
                replace
                state={{ from: location }}
            />
        );
    }

    return <>{children}</>;
};

export default ProtectedRoute;