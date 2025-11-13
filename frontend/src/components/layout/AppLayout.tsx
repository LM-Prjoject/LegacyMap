import { Outlet } from 'react-router-dom'
import Navbar from '@/components/layout/Navbar'

export default function AppLayout() {
    return (
        <div className="min-h-screen bg-gradient-to-br from-[#0f172a] via-[#1e293b] to-[#0f172a] text-white">
            <Navbar />
            <main className="pt-20">
                <Outlet />
            </main>
        </div>

    )
}
