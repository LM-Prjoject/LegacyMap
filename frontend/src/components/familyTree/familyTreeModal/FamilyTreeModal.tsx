import { FC, useEffect, useRef, useState } from 'react';
import { X, Loader } from 'lucide-react';
import { showToast } from '@/lib/toast.ts';
import api, {
    FamilyTreeCreateRequest,
    FamilyTree,
    FamilyTreeUpdateRequest,
} from '@/api/trees.ts';

interface Props {
    userId: string;
    onClose: () => void;
    onCreated?: (tree: FamilyTree) => void;
    onUpdated?: (tree: FamilyTree) => void;
    uploadImage: (file: File) => Promise<string>;
    isEdit?: boolean;
    initialData?: FamilyTree | null;
}

const MAX_SIZE = 5 * 1024 * 1024;

const FamilyTreeModal: FC<Props> = ({
                                        userId,
                                        onClose,
                                        onCreated,
                                        onUpdated,
                                        uploadImage,
                                        isEdit = false,
                                        initialData = null,
                                    }) => {
    const [loading, setLoading] = useState(false);
    const [imgUploading, setImgUploading] = useState(false);
    const [error, setError] = useState<string>('');
    const [success, setSuccess] = useState<string>('');

    const [form, setForm] = useState<FamilyTreeCreateRequest>({
        name: '',
        description: '',
        isPublic: false,
        coverImageUrl: '',
    });

    const [file, setFile] = useState<File | null>(null);
    const [preview, setPreview] = useState<string>('');
    const fileRef = useRef<HTMLInputElement | null>(null);

    useEffect(() => {
        const prev = document.body.style.overflow;
        document.body.style.overflow = 'hidden';
        const onEsc = (e: KeyboardEvent) => e.key === 'Escape' && onClose();
        window.addEventListener('keydown', onEsc);
        return () => {
            document.body.style.overflow = prev;
            window.removeEventListener('keydown', onEsc);
        };
    }, [onClose]);

    useEffect(() => {
        return () => {
            if (preview && preview.startsWith('blob:')) URL.revokeObjectURL(preview);
        };
    }, [preview]);

    useEffect(() => {
        if (initialData) {
            setForm({
                name: initialData.name || '',
                description: initialData.description || '',
                isPublic: !!initialData.isPublic,
                coverImageUrl: initialData.coverImageUrl || '',
            });
            setPreview(initialData.coverImageUrl || '');
        }
    }, [initialData]);

    const onTextChange = (e: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement>) => {
        const { name, type, value, checked } = e.target as HTMLInputElement & HTMLTextAreaElement;
        setForm(prev => ({ ...prev, [name]: type === 'checkbox' ? checked : value }));
    };

    const onFileChange = (e: React.ChangeEvent<HTMLInputElement>) => {
        setError('');
        const f = e.target.files?.[0] || null;

        if (preview && preview.startsWith('blob:')) {
            URL.revokeObjectURL(preview);
        }

        if (!f) {
            setFile(null);
            setPreview(form.coverImageUrl || '');
            return;
        }

        if (!f.type.startsWith('image/')) { setError('File không phải là ảnh'); e.target.value = ''; return; }
        if (f.size > MAX_SIZE) { setError('Ảnh tối đa 5MB'); e.target.value = ''; return; }

        setFile(f);
        setPreview(URL.createObjectURL(f));
    };

    const clearFile = () => {
        if (preview && preview.startsWith('blob:')) URL.revokeObjectURL(preview);
        setFile(null);
        setPreview(form.coverImageUrl || '');
        if (fileRef.current) fileRef.current.value = '';
    };

    const ensureCoverUrl = async (): Promise<string | undefined> => {
        if (file) {
            setImgUploading(true);
            try {
                const url = await uploadImage(file);
                setForm(p => ({ ...p, coverImageUrl: url }));
                return url;
            } finally {
                setImgUploading(false);
            }
        }

        return form.coverImageUrl ? form.coverImageUrl : undefined;
    };

    const onSubmit = async () => {
        if (loading || imgUploading) return;
        setError(''); setSuccess('');

        if (!form.name.trim()) {
            showToast.error('Tên gia phả không được để trống');
            return;
        }

        setLoading(true);
        try {
            const coverUrlOrUndef = await ensureCoverUrl();

            const updatePayload: FamilyTreeUpdateRequest = {
                name: form.name.trim(),
                description: form.description?.trim(),
                isPublic: !!form.isPublic,
                coverImageUrl: coverUrlOrUndef,
            };

            const createPayload: FamilyTreeCreateRequest = {
                name: updatePayload.name!,
                description: updatePayload.description,
                isPublic: updatePayload.isPublic!,
                coverImageUrl: typeof coverUrlOrUndef === 'string' ? coverUrlOrUndef : undefined,
            };

            if (isEdit && initialData) {
                const hasChanged =
                    updatePayload.name !== initialData.name ||
                    (updatePayload.description || '') !== (initialData.description || '') ||
                    updatePayload.isPublic !== !!initialData.isPublic ||
                    (updatePayload.coverImageUrl || '') !== (initialData.coverImageUrl || '');

                if (!hasChanged) {
                    showToast.warning('Không có thay đổi nào để lưu');
                    setLoading(false);
                    return;
                }

                const updated = await api.updateTree(userId, initialData.id, updatePayload);
                showToast.success('Cập nhật gia phả thành công!');
                onUpdated?.(updated);
            } else {
                const created = await api.createTree(userId, createPayload);
                showToast.success('Tạo gia phả thành công!');
                onCreated?.(created);
            }

            setTimeout(() => onClose(), 900);
        } catch (e: any) {
            showToast.error(e?.message || 'Đã xảy ra lỗi không xác định');
        } finally {
            setLoading(false);
        }
    };

    const canSubmit = !loading && !imgUploading;

    return (
        <div className="fixed inset-0 z-50 flex items-center justify-center px-4 py-8">
            <div className="absolute inset-0 bg-slate-900/70 backdrop-blur-md" onClick={onClose} />
            <div className="relative w-full max-w-lg rounded-2xl bg-white/10 text-slate-50 ring-1 ring-white/15 shadow-2xl shadow-black/40 backdrop-blur-xl animate-[modalIn_.2s_ease-out]">
                <div className="flex items-center justify-between px-6 pt-5 pb-2">
                    <h3 className="text-2xl font-semibold tracking-wide">
                        {isEdit ? 'Chỉnh sửa Gia Phả' : 'Tạo Gia Phả Mới'}
                    </h3>
                    <button onClick={onClose} className="p-2 rounded-lg hover:bg-white/10 transition-colors" aria-label="Đóng">
                        <X size={20} className="text-white/80" />
                    </button>
                </div>

                <div className="px-6 pb-6">
                    {error && <div className="mb-4 px-3 py-2 rounded-lg border border-red-400/40 bg-red-500/10 text-red-200 text-sm">{error}</div>}
                    {success && <div className="mb-4 px-3 py-2 rounded-lg border border-emerald-400/40 bg-emerald-500/10 text-emerald-200 text-sm">{success}</div>}

                    <div className="space-y-4">
                        {/* Name */}
                        <div>
                            <label className="block text-sm font-medium text-white/80 mb-1.5">Tên Gia Phả *</label>
                            <input
                                type="text"
                                name="name"
                                value={form.name}
                                onChange={onTextChange}
                                placeholder="Nhập tên gia phả..."
                                className="w-full px-4 py-2.5 rounded-lg bg-white/10 text-white placeholder-white/60 border border-white/20 focus:outline-none focus:ring-2 focus:ring-white/30 focus:border-transparent transition-all"
                                autoFocus
                            />
                        </div>

                        {/* Description */}
                        <div>
                            <label className="block text-sm font-medium text-white/80 mb-1.5">Mô Tả</label>
                            <textarea
                                name="description"
                                value={form.description}
                                onChange={onTextChange}
                                rows={3}
                                placeholder="Nhập mô tả gia phả..."
                                className="w-full px-4 py-2.5 rounded-lg resize-none bg-white/10 text-white placeholder-white/60 border border-white/20 focus:outline-none focus:ring-2 focus:ring-white/30 focus:border-transparent transition-all"
                            />
                        </div>

                        {/* Cover */}
                        <div>
                            <label className="block text-sm font-medium text-white/80 mb-1.5">Ảnh Bìa</label>
                            <input
                                ref={fileRef}
                                type="file"
                                accept="image/*"
                                onChange={onFileChange}
                                className="block w-full text-sm file:mr-4 file:rounded-md file:border file:px-3 file:py-2 file:bg-white/10 file:text-white file:border-white/20 file:hover:bg-white/15 border border-white/20 rounded-lg bg-white/10 text-white/90"
                            />

                            {preview && (
                                <div className="mt-3 relative w-full h-32 rounded-lg overflow-hidden border border-white/15">
                                    <img src={preview} alt="Preview" className="w-full h-full object-cover" />
                                    {preview.startsWith('blob:') && (
                                        <button
                                            type="button"
                                            onClick={clearFile}
                                            className="absolute top-2 right-2 p-1 rounded-lg bg-white/80 hover:bg-white shadow"
                                            aria-label="Xóa ảnh"
                                        >
                                            <X size={12} className="text-slate-700" />
                                        </button>
                                    )}
                                </div>
                            )}

                            {imgUploading && (
                                <div className="mt-2 flex items-center gap-2 text-white/80 text-sm">
                                    <Loader className="animate-spin" size={14} /> Đang upload ảnh...
                                </div>
                            )}
                        </div>

                        {/* Public */}
                        <label className="inline-flex items-center gap-2 cursor-pointer select-none">
                            <input
                                type="checkbox"
                                id="isPublic"
                                name="isPublic"
                                checked={!!form.isPublic}
                                onChange={onTextChange}
                                className="w-4 h-4 rounded border-white/30 text-blue-400 focus:ring-2 focus:ring-white/30"
                            />
                            <span className="text-sm text-white/85">Công khai gia phả này</span>
                        </label>

                        {/* Actions */}
                        <div className="flex gap-3 pt-2">
                            <button
                                type="button"
                                onClick={onClose}
                                disabled={loading}
                                className="flex-1 px-4 py-2.5 rounded-lg border border-white/20 bg-white/5 text-white hover:bg-white/10 transition-colors disabled:opacity-50"
                            >
                                Hủy
                            </button>
                            <button
                                type="button"
                                onClick={onSubmit}
                                disabled={!canSubmit}
                                className="flex-1 px-4 py-2.5 rounded-lg bg-gradient-to-r from-blue-500 to-blue-600 text-white font-medium hover:from-blue-600 hover:to-blue-700 transition-all disabled:opacity-50 flex items-center justify-center gap-2"
                            >
                                {(loading || imgUploading) && <Loader size={16} className="animate-spin" />}
                                {isEdit ? (loading || imgUploading ? 'Đang lưu...' : 'Lưu thay đổi') : (loading || imgUploading ? 'Đang tạo...' : 'Tạo Gia Phả')}
                            </button>
                        </div>
                    </div>
                </div>
            </div>

            <style>{`
        @keyframes modalIn {
          from { opacity: 0; transform: translateY(8px) scale(.98); }
          to   { opacity: 1; transform: translateY(0) scale(1); }
        }
      `}</style>
        </div>
    );
};

export default FamilyTreeModal;