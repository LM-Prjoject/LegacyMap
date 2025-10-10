import Navbar from '@/components/layout/Navbar'
export default function TreesList() {
    return (
        <div className="min-h-screen flex flex-col">
            <Navbar /> {/* Không cần truyền onLoginClick */}
            <main className="flex-1 container mx-auto p-4">
                <div className="flex items-center justify-between mb-4">
                    <h2 className="text-ivory text-xl font-semibold">Cây của tôi</h2>
                    <button className="btn-primary">Tạo gia phả</button>
                </div>
                <div className="grid md:grid-cols-2 xl:grid-cols-3 gap-4">
                    <div className="card p-4">Gia phả mẫu (WIP)</div>
                </div>
            </main>
        </div>
    )
}