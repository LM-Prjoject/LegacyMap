import React, { useEffect, useMemo, useRef, useState } from "react";
import { Loader2, X } from "lucide-react";
import { showToast } from "@/lib/toast.ts";

export type MemberFormValues = {
    fullName: string;
    gender?: "male" | "female" | "other" | "";
    birthDate?: string;
    deathDate?: string;
    birthPlace?: string;
    deathPlace?: string;
    biography?: string;
    avatarUrl?: string;
    avatarFile?: File | null;
};

type Props = {
    open: boolean;
    title?: string;
    initialValues?: MemberFormValues;
    submitting?: boolean;
    onClose: () => void;
    onSubmit: (values: MemberFormValues) => void;
};

const MAX_SIZE = 5 * 1024 * 1024;

export default function MemberModal({
                                        open,
                                        title = "Thêm thành viên",
                                        initialValues,
                                        submitting = false,
                                        onClose,
                                        onSubmit,
                                    }: Props) {
    const [form, setForm] = useState<MemberFormValues>(initialValues ?? { fullName: "" });
    const [preview, setPreview] = useState<string>("");
    const [dragOver, setDragOver] = useState(false);
    const [isDeceased, setIsDeceased] = useState<boolean>(Boolean(initialValues?.deathDate));
    const inputRef = useRef<HTMLInputElement | null>(null);
    const [submitted, setSubmitted] = useState(false);

    useEffect(() => {
        if (!open) return;
        const prev = document.body.style.overflow;
        document.body.style.overflow = "hidden";
        return () => {
            document.body.style.overflow = prev;
        };
    }, [open]);

    useEffect(() => {
        if (!open) return;
        setForm(initialValues ?? { fullName: "" });
        setPreview(initialValues?.avatarUrl || "");
        setIsDeceased(Boolean(initialValues?.deathDate));
        setSubmitted(false);
    }, [open, initialValues]);

    useEffect(() => {
        if (!form.avatarFile) return;
        const url = URL.createObjectURL(form.avatarFile);
        setPreview(url);
        return () => URL.revokeObjectURL(url);
    }, [form.avatarFile]);

    const today = useMemo(() => new Date().toISOString().slice(0, 10), []);
    const nameError = form.fullName.trim().length === 0 ? "Vui lòng nhập họ và tên." : "";
    const birthDateError =
        !form.birthDate
            ? "Vui lòng chọn ngày sinh."
            : form.birthDate > today
                ? "Ngày sinh không hợp lệ."
                : "";
    const genderError = !form.gender ? "Vui lòng chọn giới tính." : "";

    const deathDateError = (() => {
        if (!isDeceased) return "";
        if (!form.deathDate) return "Vui lòng chọn ngày mất.";
        if (form.birthDate && form.deathDate < form.birthDate) return "Ngày mất không thể là ngày trước ngày sinh.";
        if (form.deathDate > today) return "Ngày mất không thể ở tương lai.";
        return "";
    })();

    const canSubmitAll = !nameError && !birthDateError && !deathDateError && !genderError;

    const handlePickFile = (file?: File) => {
        if (!file) return;
        if (!file.type.startsWith("image/")) {
            showToast.warning("Vui lòng chọn tệp hình ảnh");
            return;
        }
        if (file.size > MAX_SIZE) {
            showToast.error("Ảnh quá lớn (tối đa 5MB)");
            return;
        }
        setForm((f) => ({ ...f, avatarFile: file, avatarUrl: undefined }));
    };

    const clearImage = () => {
        setForm((f) => ({ ...f, avatarFile: undefined, avatarUrl: undefined }));
        setPreview("");
        if (inputRef.current) inputRef.current.value = "";
    };

    const onDrop = (e: React.DragEvent) => {
        e.preventDefault();
        setDragOver(false);
        const file = e.dataTransfer.files?.[0];
        handlePickFile(file);
    };

    const toggleDeceased = (checked: boolean) => {
        setIsDeceased(checked);
        if (!checked) {
            setForm((f) => ({
                ...f,
                deathDate: undefined,
                deathPlace: undefined,
            }));
        }
    };

    const handleSubmit = () => {
        setSubmitted(true);

        if (!canSubmitAll) {
            if (nameError) showToast.error(nameError);
            else if (birthDateError) showToast.error(birthDateError);
            else if (deathDateError) showToast.error(deathDateError);
            else if (genderError) showToast.error(genderError);
            return;
        }
        onSubmit({ ...form, gender: (form.gender || "") as any });
    };

    if (!open) return null;

    return (
        <div
            className="fixed inset-0 z-50 grid place-items-center p-4 bg-black/50 overscroll-contain"
            role="dialog"
            aria-modal="true"
        >
            <div className="w-full max-w-2xl bg-white text-slate-900 rounded-2xl shadow-xl max-h-[85vh] overflow-hidden flex flex-col mt-auto">
                <div className="flex items-center justify-between px-5 py-4 border-b">
                    <h3 className="text-lg font-semibold">{title}</h3>
                    <button aria-label="Đóng" onClick={onClose} className="p-2 rounded-full hover:bg-slate-100">
                        <X />
                    </button>
                </div>

                <div className="px-5 py-4 overflow-y-auto">
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                        <label className="flex flex-col gap-1">
                            <span className="text-sm text-slate-600">Họ và tên
                                <span className="text-red-600">*</span>
                            </span>
                            <input
                                className={`border rounded-lg px-3 py-2 ${
                                    submitted && nameError ? "border-red-500 focus-visible:outline-red-500" : ""
                                }`}
                                value={form.fullName}
                                onChange={(e) => setForm((f) => ({ ...f, fullName: e.target.value }))}
                                placeholder="Nguyễn Văn A"
                                required
                                aria-invalid={submitted && !!nameError}
                                aria-describedby={submitted && nameError ? "name-error" : undefined}
                            />
                            {submitted && nameError && (
                                <span id="name-error" className="text-xs text-red-600 mt-1">
                  {nameError}
                </span>
                            )}
                        </label>

                        <label className="flex flex-col gap-1">
                            <span className="text-sm text-slate-600">Giới tính
                                <span className="text-red-600">*</span>
                            </span>
                            <select
                                className={`border rounded-lg px-3 py-2 ${submitted && genderError ? "border-red-500 focus-visible:outline-red-500" : ""}`}
                                value={form.gender || ""}
                                onChange={(e) => setForm((f) => ({ ...f, gender: (e.target.value as any) || "" }))}
                                required
                                aria-invalid={submitted && !!genderError}
                                aria-describedby={submitted && genderError ? "gender-error" : undefined}
                            >
                                <option value="">—</option>
                                <option value="male">Nam</option>
                                <option value="female">Nữ</option>
                                <option value="other">Khác</option>
                            </select>
                            {submitted && genderError && (
                                <span id="gender-error" className="text-xs text-red-600 mt-1">{genderError}</span>
                            )}
                        </label>

                        <div className="grid grid-cols-1 md:grid-cols-2 gap-3 md:col-span-2">
                            <label className="flex flex-col gap-1">
                                <span className="text-sm text-slate-600">Ngày sinh
                                    <span className="text-red-600">*</span>
                                </span>
                                <input
                                    type="date"
                                    className={`border rounded-lg px-3 py-2 ${
                                        submitted && birthDateError ? "border-red-500 focus-visible:outline-red-500" : ""
                                    }`}
                                    max={today}
                                    value={form.birthDate || ""}
                                    onChange={(e) => setForm((f) => ({ ...f, birthDate: e.target.value || undefined }))}
                                    required
                                    aria-invalid={submitted && !!birthDateError}
                                    aria-describedby={submitted && birthDateError ? "birth-error" : undefined}
                                />
                                {submitted && birthDateError && (
                                    <span id="birth-error" className="text-xs text-red-600 mt-1">
                    {birthDateError}
                  </span>
                                )}
                            </label>

                            <label className="flex flex-col gap-1">
                                <span className="text-sm text-slate-600">Nơi sinh</span>
                                <input
                                    className="border rounded-lg px-3 py-2"
                                    value={form.birthPlace || ""}
                                    onChange={(e) => setForm((f) => ({ ...f, birthPlace: e.target.value || undefined }))}
                                />
                            </label>
                        </div>

                        <div className="md:col-span-2">
                            <label className="inline-flex items-center gap-2 select-none mt-1">
                                <input
                                    type="checkbox"
                                    className="size-4"
                                    checked={isDeceased}
                                    onChange={(e) => toggleDeceased(e.target.checked)}
                                />
                                <span className="text-sm text-slate-700">Đã mất</span>
                            </label>
                        </div>

                        {isDeceased && (
                            <div className="grid grid-cols-1 md:grid-cols-2 gap-3 md:col-span-2">
                                <label className="flex flex-col gap-1">
                                    <span className="text-sm text-slate-600">Ngày mất
                                        <span className="text-red-600">*</span>
                                    </span>
                                    <input
                                        type="date"
                                        className={`border rounded-lg px-3 py-2 ${
                                            submitted && deathDateError ? "border-red-500 focus-visible:outline-red-500" : ""
                                        }`}
                                        max={today}
                                        min={form.birthDate || undefined}
                                        value={form.deathDate || ""}
                                        onChange={(e) => setForm((f) => ({ ...f, deathDate: e.target.value || undefined }))}
                                        required
                                        aria-invalid={submitted && !!deathDateError}
                                        aria-describedby={submitted && deathDateError ? "death-error" : undefined}
                                    />
                                    {submitted && deathDateError && (
                                        <span id="death-error" className="text-xs text-red-600 mt-1">
                      {deathDateError}
                    </span>
                                    )}
                                </label>

                                <label className="flex flex-col gap-1">
                                    <span className="text-sm text-slate-600">Nơi mất</span>
                                    <input
                                        className="border rounded-lg px-3 py-2"
                                        value={form.deathPlace || ""}
                                        onChange={(e) => setForm((f) => ({ ...f, deathPlace: e.target.value || undefined }))}
                                    />
                                </label>
                            </div>
                        )}

                        <label className="flex flex-col gap-1 md:col-span-2">
                            <span className="text-sm text-slate-600">Tiểu sử</span>
                            <textarea
                                className="border rounded-lg px-3 py-2 min-h-[96px]"
                                value={form.biography || ""}
                                onChange={(e) => setForm((f) => ({ ...f, biography: e.target.value || undefined }))}
                                placeholder="Một vài dòng mô tả..."
                            />
                        </label>

                        <div className="md:col-span-2">
                            <span className="block text-sm text-slate-600 mb-1">Ảnh đại diện</span>

                            {preview ? (
                                <div className="relative inline-block">
                                    <img
                                        src={preview}
                                        alt="avatar preview"
                                        className={`w-20 h-20 rounded-lg object-cover border cursor-pointer ${
                                            submitting ? "opacity-60 pointer-events-none" : ""
                                        }`}
                                        onClick={() => inputRef.current?.click()}
                                    />
                                    <button
                                        type="button"
                                        onClick={clearImage}
                                        className="absolute -top-2 -right-2 bg-white border border-slate-300 rounded-full w-6 h-6 flex items-center justify-center text-slate-700 hover:bg-slate-100 shadow"
                                        aria-label="Xoá ảnh"
                                        title="Xoá ảnh"
                                        disabled={submitting}
                                        style={{
                                            opacity: submitting ? 0.6 : 1,
                                            pointerEvents: submitting ? "none" : "auto",
                                        }}
                                    >
                                        <X size={14} />
                                    </button>
                                </div>
                            ) : (
                                <div
                                    onDragOver={(e) => {
                                        e.preventDefault();
                                        setDragOver(true);
                                    }}
                                    onDragLeave={() => setDragOver(false)}
                                    onDrop={onDrop}
                                    className={`border rounded-lg px-3 py-10 text-center cursor-pointer ${
                                        dragOver ? "bg-slate-50" : "hover:bg-slate-50"
                                    }`}
                                    onClick={() => inputRef.current?.click()}
                                    aria-label="Kéo thả ảnh vào đây hoặc bấm để chọn"
                                >
                                    Kéo thả ảnh vào đây hoặc bấm để chọn
                                </div>
                            )}

                            <input
                                ref={inputRef}
                                type="file"
                                accept="image/*"
                                className="hidden"
                                onChange={(e) => handlePickFile(e.target.files?.[0])}
                            />
                        </div>
                    </div>
                </div>

                <div className="px-5 py-4 border-t flex justify-end gap-2">
                    <button className="px-4 py-2 rounded-xl hover:bg-slate-100 border " onClick={onClose}>
                        Hủy
                    </button>
                    <button
                        onClick={handleSubmit}
                        disabled={submitting}
                        className="px-4 py-2 rounded-xl bg-black text-white disabled:opacity-50"
                    >
                        {submitting ? (
                            <span className="inline-flex items-center gap-2">
                <Loader2 className="animate-spin" /> Đang lưu…
              </span>
                        ) : (
                            "Lưu"
                        )}
                    </button>
                </div>
            </div>
        </div>
    );
}