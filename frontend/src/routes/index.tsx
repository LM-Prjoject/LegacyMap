import { createBrowserRouter, Navigate } from 'react-router-dom'
import SignIn from '@/pages/auth/SignIn'
import SignUp from '@/pages/auth/SignUp'
import PasswordReset from '@/pages/auth/password-reset'
import TreesList from '@/pages/dashboard/TreesList'
import HomePage from '@/pages/HomePage'
import ProtectedRoute from '@/components/auth/ProtectedRoute'
import GoogleSuccess from "@/pages/auth/GoogleSuccess.tsx"
import ProfilePage from "@/pages/auth/ProfilePage.tsx";

export const router = createBrowserRouter([
    {
        path: '/',
        element: <HomePage />
    },
    {
        path: '/homepage',
        element: <ProtectedRoute><HomePage /></ProtectedRoute>
    },
    {
        path: '/signin',
        element: <SignIn
            onClose={() => window.history.back()}
            onShowPasswordReset={() => window.location.href = '/password-reset'}
            onShowSignUp={() => window.location.href = '/signup'}
        />
    },
    {
        path: '/signup',
        element: <SignUp
            onClose={() => window.history.back()}
            onShowSignIn={() => window.location.href = '/signin'}
        />
    },
    {
        path: '/auth/google-success',
        element: <GoogleSuccess />
    },
    {
        path: '/password-reset',
        element: <PasswordReset
            onClose={() => window.history.back()}
            onShowSignIn={() => window.location.href = '/signin'}
        />
    },
    {
        path: '/dashboard',
        element: <ProtectedRoute><TreesList /></ProtectedRoute>
    },

    {
        path: '/app',
        element: <Navigate to="/dashboard" replace />
    },
    {
        path: '/app/trees/:treeId',
        element: <Navigate to="/tree/:treeId" replace />
    },
    // Redirect từ route password cũ sang route mới
    {
        path: '/forgot-password',
        element: <Navigate to="/password-reset" replace />
    },
    {
        path: '/reset-password',
        element: <Navigate to="/password-reset" replace />
    },
    {
        path: '/forgot',
        element: <Navigate to="/password-reset" replace />
    },
    {
        path: '/profile',
        element: <ProtectedRoute><ProfilePage /></ProtectedRoute>
    },
])