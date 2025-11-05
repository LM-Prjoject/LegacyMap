import { User, Pencil } from "lucide-react";
// Using native button instead of shadcn/ui Button to avoid dependency issues
// import { Button } from "@/components/ui/button";

export interface TreeInfo {
    coverImageUrl?: string | null;
    name?: string | null;
    description?: string | null;
    createdByName?: string | null;
    createdAt?: string | null;
}

type Props = {
    tree: TreeInfo | null;
    selectedPerson?: any;
    memberCount: number;
    generationCount: number;
    onEditClick?: () => void;
    onAddClick: () => void;
};

const formatDate = (dateString?: string | null, emptyText: string = 'Chưa cập nhật') => {
    if (!dateString) return emptyText;
    const date = new Date(dateString);
    if (isNaN(date.getTime())) return emptyText;
    return date.toLocaleDateString('vi-VN'); // dd/mm/yyyy
};

export default function DetailsSidebar({
                                           tree,
                                           selectedPerson,
                                           memberCount,
                                           generationCount,
                                           onEditClick = () => {},
                                           onAddClick,
                                       }: Props) {
    return (
        <aside className="col-span-12 md:col-span-3 bg-white rounded-xl shadow overflow-hidden flex flex-col max-h-[calc(100vh-160px)]">
            <div className="relative h-[220px] w-full bg-slate-200 shrink-0">
                {tree?.coverImageUrl ? (
                    <img
                        src={tree.coverImageUrl}
                        alt="Cover"
                        className="absolute inset-0 w-full h-full object-cover"
                    />
                ) : null}
                <div className="absolute inset-0 bg-gradient-to-b from-transparent via-black/30 to-black/70" />
                <div className="absolute bottom-3 left-3 right-3 text-white">
                    <div className="text-xl font-semibold leading-tight">
                        {tree?.name || "Gia phả"}
                    </div>
                </div>
            </div>

            <div className="flex-1 min-h-0 overflow-y-auto p-4 space-y-4">
                {selectedPerson && (
                    <div className="flex-1 p-4 overflow-y-auto">
                        <div className="flex items-start justify-between mb-4">
                            <h3 className="text-lg font-semibold">Thông tin thành viên</h3>
                            {onEditClick && (
                                <button
                                    onClick={onEditClick}
                                    className="p-1.5 rounded-full hover:bg-gray-100"
                                    title="Chỉnh sửa"
                                >
                                    <Pencil className="w-4 h-4 text-gray-500" />
                                </button>
                            )}
                        </div>
                        <div className="space-y-4">
                            <div className="flex items-center space-x-4">
                                <div className="h-20 w-20 rounded-full overflow-hidden bg-gray-100">
                                    {selectedPerson.avatarUrl ? (
                                        <img
                                            src={selectedPerson.avatarUrl}
                                            alt={selectedPerson.fullName}
                                            className="h-full w-full object-cover"
                                        />
                                    ) : (
                                        <div className="h-full w-full bg-gray-200 flex items-center justify-center">
                                            <User className="h-8 w-8 text-gray-400" />
                                        </div>
                                    )}
                                </div>
                                <div>
                                    <h4 className="text-lg font-medium">{selectedPerson.fullName}</h4>
                                    <p className="text-sm text-gray-500">
                                        {selectedPerson.gender === 'male' ? 'Nam' : 'Nữ'}
                                    </p>
                                </div>
                            </div>

                            {(selectedPerson.birthDate || selectedPerson.birthPlace) && (
                                <div className="space-y-1">
                                    {selectedPerson.birthDate && (
                                        <div>
                                            <p className="text-sm font-medium text-gray-500">Ngày sinh</p>
                                            <p className="text-sm">{formatDate(selectedPerson.birthDate)}</p>
                                        </div>
                                    )}
                                    {selectedPerson.birthPlace && (
                                        <div>
                                            <p className="text-sm font-medium text-gray-500">Nơi sinh</p>
                                            <p className="text-sm">{selectedPerson.birthPlace}</p>
                                        </div>
                                    )}
                                </div>
                            )}

                            {(selectedPerson.deathDate || selectedPerson.deathPlace) && (
                                <div className="space-y-1">
                                    {selectedPerson.deathDate && (
                                        <div>
                                            <p className="text-sm font-medium text-gray-500">Ngày mất</p>
                                            <p className="text-sm">{formatDate(selectedPerson.deathDate)}</p>
                                        </div>
                                    )}
                                    {selectedPerson.deathPlace && (
                                        <div>
                                            <p className="text-sm font-medium text-gray-500">Nơi mất</p>
                                            <p className="text-sm">{selectedPerson.deathPlace}</p>
                                        </div>
                                    )}
                                </div>
                            )}

                            {selectedPerson.biography && (
                                <div>
                                    <p className="text-sm font-medium text-gray-500">Tiểu sử</p>
                                    <p className="whitespace-pre-line text-sm">{selectedPerson.biography}</p>
                                </div>
                            )}
                        </div>
                    </div>
                )}

                <hr className="my-4 border-gray-200" />

                <div>
                    <h3 className="text-sm font-medium text-gray-500 mb-2">THÔNG TIN CÂY GIA PHẢ</h3>
                    {tree?.description && (
                        <div className="mb-3">
                            <p className="text-sm text-gray-700 whitespace-pre-wrap">{tree.description}</p>
                        </div>
                    )}
                    <div className="grid grid-cols-2 gap-3 mt-4">
                        <div className="rounded-xl bg-slate-100 px-4 py-3 text-center">
                            <div className="text-2xl font-bold text-slate-900">{memberCount}</div>
                            <div className="text-xs text-slate-600 mt-0.5">Thành viên</div>
                        </div>
                        <div className="rounded-xl bg-slate-100 px-4 py-3 text-center">
                            <div className="text-2xl font-bold text-slate-900">{generationCount}</div>
                            <div className="text-xs text-slate-600 mt-0.5">Thế hệ</div>
                        </div>
                    </div>
                    <div className="mt-4 text-xs text-gray-500">
                        Tạo bởi {tree?.createdByName || 'Ẩn danh'} • {tree?.createdAt ? new Date(tree.createdAt).toLocaleDateString('vi-VN') : ''}
                    </div>
                </div>
            </div>
        </aside>
    );
}