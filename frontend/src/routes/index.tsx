import { createBrowserRouter, Navigate, Outlet } from 'react-router-dom'
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
import NotificationsPage from "@/pages/auth/NotificationsPage.tsx"
import SharedTreeView from '@/pages/SharedTreeView'

// 🧱 Layout để giữ Navbar & gradient cố định
import AppLayout from '@/components/layout/AppLayout'

export const router = createBrowserRouter([
    // 🌐 Main app layout (Navbar cố định, tránh flash trắng)
    {
        path: '/',
        element: <AppLayout />, // ✅ Bọc toàn bộ các trang người dùng
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

    // ✅ SHARED TREE ROUTE (Công khai - không cần đăng nhập)
    {
        path: '/trees/shared/:shareToken',
        element: <SharedTreeView />,
    },

    // 🔐 Auth routes (ngoài layout)
    {
        path: '/signin',
        element: (
            <SignIn
                onClose={() => window.history.back()}
                onShowPasswordReset={() => (window.location.href = '/password-reset')}
                onShowSignUp={() => (window.location.href = '/signup')}
            />
        ),
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

    // 🧩 Admin routes – GIỮ NGUYÊN KHÔNG ĐỘNG TỚI
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
            {
                path: 'settings',
                element: (
                    <div className="bg-white rounded-lg shadow p-6">
                        <h2 className="text-2xl font-bold text-gray-800 mb-4">⚙️ Settings</h2>
                        <p className="text-gray-600">Coming soon...</p>
                    </div>
                ),
            },
        ],
    },

    // 🔀 Redirect routes
    { path: '/app', element: <Navigate to="/dashboard" replace /> },
    { path: '/forgot-password', element: <Navigate to="/password-reset" replace /> },
    { path: '/reset-password', element: <Navigate to="/password-reset" replace /> },
    { path: '/forgot', element: <Navigate to="/password-reset" replace /> },
])