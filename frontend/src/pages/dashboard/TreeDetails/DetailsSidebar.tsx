type Props = {
    coverImageUrl?: string | null;
    name?: string | null;
    description?: string | null;
    createdByName?: string | null;
    createdAt?: string | null;
    memberCount: number;
    generationCount: number;
    onMembersClick?: () => void;
};

function formatDate(date?: string | null) {
    if (!date) return "—";
    const d = new Date(date);
    if (isNaN(d.getTime())) return "—";
    return d.toLocaleDateString("vi-VN");
}

export default function DetailsSidebar({
                                           coverImageUrl,
                                           name,
                                           description,
                                           createdByName,
                                           createdAt,
                                           memberCount,
                                           generationCount,
                                           onMembersClick,
                                       }: Props) {
    return (
        <aside className="col-span-12 md:col-span-3 bg-white rounded-xl shadow overflow-hidden flex flex-col max-h-[calc(100vh-160px)]">
            <div className="relative h-[220px] w-full bg-slate-200 shrink-0">
                {coverImageUrl ? (
                    <img
                        src={coverImageUrl}
                        alt="Cover"
                        className="absolute inset-0 w-full h-full object-cover"
                    />
                ) : null}
                <div className="absolute inset-0 bg-gradient-to-b from-transparent via-black/30 to-black/70" />
                <div className="absolute bottom-3 left-3 right-3 text-white">
                    <div className="text-xl font-semibold leading-tight">
                        {name || "Gia phả"}
                    </div>
                </div>
            </div>

            <div className="flex-1 min-h-0 overflow-y-auto p-4 space-y-4">
                {description ? (
                    <section>
                        <div className="text-xs font-medium text-slate-500 mb-1">MÔ TẢ</div>
                        <p className="text-sm text-slate-800 whitespace-pre-wrap break-words leading-relaxed px-1">
                            {description}
                        </p>
                    </section>
                ) : null}

                <section>
                    <div className="text-xs font-medium text-slate-500 mb-1">NGƯỜI TẠO</div>
                    <div className="text-sm text-slate-800">{createdByName || "—"}</div>
                </section>

                <section>
                    <div className="text-xs font-medium text-slate-500 mb-1">NGÀY TẠO</div>
                    <div className="text-sm text-slate-800">{formatDate(createdAt)}</div>
                </section>

                <hr className="my-1 border-slate-200" />

                <section className="grid grid-cols-2 gap-3 pb-1">
                    <button
                        type="button"
                        onClick={onMembersClick}
                        className={`rounded-xl bg-slate-100 px-4 py-3 text-center ${onMembersClick ? 'hover:bg-slate-200 transition-colors cursor-pointer' : ''}`}
                    >
                        <div className="text-2xl font-bold text-slate-900">{memberCount}</div>
                        <div className="text-xs text-slate-600 mt-0.5">Thành viên</div>
                    </button>
                    <div className="rounded-xl bg-slate-100 px-4 py-3 text-center">
                        <div className="text-2xl font-bold text-slate-900">{generationCount}</div>
                        <div className="text-xs text-slate-600 mt-0.5">Thế hệ</div>
                    </div>
                </section>
            </div>
        </aside>
    );
}