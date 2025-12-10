import { createBrowserRouter, Navigate, Outlet, useLocation } from 'react-router-dom'
import SignIn from '@/pages/auth/SignIn'
import SignUp from '@/pages/auth/SignUp'
import PasswordReset from '@/pages/auth/password-reset'
import TreesList from '@/pages/dashboard/TreesList'
import HomePage from '@/pages/HomePage'
import ProtectedRoute from '@/components/auth/ProtectedRoute'
import GoogleSuccess from "@/pages/auth/GoogleSuccess.tsx"
import ProfilePage from "@/pages/auth/ProfilePage.tsx"
import AdminDashboard from '@/pages/admin/AdminDashboard'
import UserManagement from '@/pages/admin/UserManagement'
import UserDetail from '@/pages/admin/UserDetail'
import AdminLayout from '@/components/admin/AdminLayout'
import FamilyTreesPage from '@/pages/admin/FamilyTreesPage'
import EventsPage from '@/pages/event/EventsPage'
import EventFormPage from "@/pages/event/EventFormPage.tsx"
import { EventProvider } from '@/contexts/EventContext'
import TreeDetails from '@/pages/dashboard/TreeDetails/TreeDetails.tsx'
import NotificationsPage from "@/pages/notifications/NotificationsPage.tsx"
import SharedTreeView from '@/pages/SharedTreeView'
import ShareTreeViewById from '@/pages/ShareTreeViewById'

// 🧱 Layout để giữ Navbar & gradient cố định
import AppLayout from '@/components/layout/AppLayout'

// Alias component to redirect /login -> /signin while preserving query string
function LoginAlias() {
    const location = useLocation();
    const search = location.search || '';
    return <Navigate to={`/signin${search}` } replace />
}

// Alias component to redirect /auth/reset-password -> /password-reset while preserving query string (token)
function ResetAlias() {
    const location = useLocation();
    const search = location.search || '';
    return <Navigate to={`/password-reset${search}`} replace />
}

export const router = createBrowserRouter([
    // 🌐 Main app layout (Navbar cố định, tránh flash trắng)
    {
        path: '/',
        element: <AppLayout />,
        children: [
            {
                index: true,
                element: <HomePage />,
            },
            {
                path: '/homepage',
                element: (
                    <ProtectedRoute>
                        <HomePage />
                    </ProtectedRoute>
                ),
            },
            {
                path: '/dashboard',
                element: (
                    <ProtectedRoute>
                        <TreesList />
                    </ProtectedRoute>
                ),
            },
            {
                path: '/events',
                element: (
                    <ProtectedRoute>
                        <EventProvider>
                            <EventsPage />
                        </EventProvider>
                    </ProtectedRoute>
                ),
            },
            {
                path: '/events/create',
                element: (
                    <ProtectedRoute>
                        <EventProvider>
                            <EventFormPage />
                        </EventProvider>
                    </ProtectedRoute>
                ),
            },
            {
                path: '/notifications',
                element: (
                    <ProtectedRoute>
                        <NotificationsPage />
                    </ProtectedRoute>
                ),
            },
            {
                path: '/profile',
                element: (
                    <ProtectedRoute>
                        <ProfilePage />
                    </ProtectedRoute>
                ),
            },
            {
                path: '/trees/:treeId',
                element: (
                    <ProtectedRoute>
                        <TreeDetails />
                    </ProtectedRoute>
                ),
            },
        ],
    },

    // ✅ SHARED TREE ROUTES (Không có Navbar)
    {
        path: '/trees/shared/:shareToken',
        element: <SharedTreeView />,
    },
    {
        path: '/share-tree-view/:treeId',
        element: (
            <ProtectedRoute>
                <ShareTreeViewById />
            </ProtectedRoute>
        ),
    },

    {
        path: '/signin',
        element: <Navigate to="/?showSignIn=true" replace />,
    },
    {
        path: '/login',
        element: <LoginAlias />,
    },
    {
        path: '/signup',
        element: (
            <SignUp
                onClose={() => window.history.back()}
                onShowSignIn={() => (window.location.href = '/signin')}
            />
        ),
    },
    {
        path: '/auth/google-success',
        element: <GoogleSuccess />,
    },
    {
        path: '/password-reset',
        element: (
            <PasswordReset
                onClose={() => window.history.back()}
                onShowSignIn={() => (window.location.href = '/signin')}
            />
        ),
    },

    {
        path: '/admin',
        element: (
            <ProtectedRoute requiredRole="admin">
                <AdminLayout>
                    <Outlet />
                </AdminLayout>
            </ProtectedRoute>
        ),
        children: [
            { index: true, element: <AdminDashboard /> },
            { path: 'dashboard', element: <AdminDashboard /> },
            { path: 'users', element: <UserManagement /> },
            { path: 'users/:userId', element: <UserDetail /> },
            { path: 'trees', element: <FamilyTreesPage /> },
        ],
    },

    { path: '/app', element: <Navigate to="/dashboard" replace /> },
    { path: '/forgot-password', element: <Navigate to="/password-reset" replace /> },
    { path: '/reset-password', element: <Navigate to="/password-reset" replace /> },
    { path: '/auth/reset-password', element: <ResetAlias /> },
    { path: '/forgot', element: <Navigate to="/password-reset" replace /> },
])