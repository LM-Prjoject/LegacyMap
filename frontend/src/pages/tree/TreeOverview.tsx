import { Outlet, NavLink, useParams } from 'react-router-dom'
import Navbar from '@/components/layout/Navbar'

export default function TreeOverview() {
    const { treeId } = useParams()
    return (
        <div className="min-h-screen flex flex-col">
            <Navbar /> {/* Không cần truyền onLoginClick */}
            <main className="flex-1 container mx-auto p-4 text-ivory">
                <h1 className="text-2xl mb-3">Cây #{treeId}</h1>
                <div className="flex gap-3 mb-4">
                    <Tab to="members" label="Thành viên" />
                    <Tab to="relations" label="Quan hệ" />
                    <Tab to="graph" label="Đồ thị" />
                    <Tab to="share" label="Chia sẻ" />
                    <Tab to="audit" label="Lịch sử" />
                </div>
                <Outlet />
            </main>
        </div>
    )
}

function Tab({ to, label }: { to: string; label: string }) {
    return (
        <NavLink to={to} className={({ isActive }) =>
            `px-3 py-1 rounded-lg border ${isActive ? 'bg-gold text-ink' : 'border-gold'}`
        }>{label}</NavLink>
    )
}