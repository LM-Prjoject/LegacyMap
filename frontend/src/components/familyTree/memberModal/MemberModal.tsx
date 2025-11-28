import React, { useEffect, useMemo, useRef, useState } from "react";
import { Loader2, X, UserPlus, Upload } from "lucide-react";
import { showToast } from "@/lib/toast.ts";
import { usersApi } from "@/api/users";

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
    phone?: string;
    email?: string;
    sendInvite?: boolean;
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
const email_regrex = /^[a-zA-Z0-9._-]+@[a-zA-Z0-9-]+\.[a-zA-Z.]{2,}$/;

const hasInvalidNameChars = (raw: string): boolean => {
    const trimmed = raw.trim();
    if (!trimmed) return false;
    return /[^\p{L}\s]/u.test(trimmed);
};

const getNameError = (raw: string): string => {
    const trimmed = raw.trim();
    if (!trimmed) return "Vui lòng nhập họ và tên.";
    if (hasInvalidNameChars(trimmed)) {
        return "Tên không được chứa các ký tự đặc biệt và số.";
    }
    return "";
};

const normalizeFullName = (raw: string): string => {
    return raw
        .replace(/\s+/g, " ")
        .trim()
        .split(" ")
        .filter(Boolean)
        .map(
            (w) =>
                w.charAt(0).toLocaleUpperCase("vi-VN") +
                w.slice(1).toLocaleLowerCase("vi-VN")
        )
        .join(" ");
};

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
    const [emailExists, setEmailExists] = useState<boolean>(false);
    const [sendNotify, setSendNotify] = useState<boolean>(false);

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
        setEmailExists(false);
        setSendNotify(false);
    }, [open]);

    useEffect(() => {
        if (!form.avatarFile) return;
        const url = URL.createObjectURL(form.avatarFile);
        setPreview(url);
        return () => URL.revokeObjectURL(url);
    }, [form.avatarFile]);

    const today = useMemo(() => new Date().toISOString().slice(0, 10), []);
    const nameError = getNameError(form.fullName);
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

    const emailError = (() => {
        if (!form.email) return "";
        const value = form.email.trim();
        if (!value) return "";
        if (!email_regrex.test(value)) {
            return "Email không đúng định dạng.";
        }
        return "";
    })();

    const canSubmitAll = !nameError && !birthDateError && !deathDateError && !genderError && !emailError;

    useEffect(() => {
        const v = form.email?.trim().toLowerCase() || "";
        if (!v || emailError) {
            setEmailExists(false);
            setSendNotify(false);
            return;
        }
        let alive = true;
        const t = setTimeout(async () => {
            try {
                const r = await usersApi.checkEmail(v);
                if (!alive) return;
                setEmailExists(!!r?.exists);
                setSendNotify(false);
            } catch (e) {
                // ignore
            }
        }, 400);
        return () => {
            alive = false;
            clearTimeout(t);
        };
    }, [form.email, emailError]);

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
            else if (emailError) showToast.error(emailError);
            return;
        }

        const normalizedFullName = normalizeFullName(form.fullName);

        onSubmit({
            ...form,
            fullName: normalizedFullName,
            gender: (form.gender || "") as any,
            sendInvite: emailExists && sendNotify,
        });
    };

    if (!open) return null;

    return (
        <div className="fixed inset-0 z-50 flex items-center justify-center p-4">
            {/* Backdrop with gradient */}
            <div
                className="absolute inset-0 bg-gradient-to-br from-[#0f1419]/95 via-[#1e2a3a]/95 to-[#0f1419]/95 backdrop-blur-sm"
                onClick={onClose}
            />

            {/* Ambient glow effects */}
            <div className="absolute top-1/4 left-1/4 w-96 h-96 bg-[#ffd89b]/10 rounded-full blur-[120px] pointer-events-none" />
            <div className="absolute bottom-1/4 right-1/4 w-96 h-96 bg-[#d4af7a]/10 rounded-full blur-[120px] pointer-events-none" />

            <div className="relative bg-gradient-to-br from-[#1e2a3a]/95 via-[#0f1419]/90 to-[#1e2a3a]/95 backdrop-blur-2xl rounded-2xl shadow-[0_0_60px_rgba(255,216,155,0.15),0_20px_80px_rgba(0,0,0,0.6)] w-full max-w-2xl max-h-[90vh] flex flex-col border-2 border-[#ffd89b]/20 overflow-hidden">
                {/* Decorative top border glow */}
                <div className="absolute top-0 left-0 right-0 h-[2px] bg-gradient-to-r from-transparent via-[#ffd89b] to-transparent opacity-60" />

                {/* Header */}
                <div className="relative flex items-center justify-between p-6 border-b border-[#ffd89b]/20 bg-gradient-to-r from-[#ffd89b]/5 to-transparent">
                    <div className="flex items-center gap-3">
                        <div className="relative p-2.5 bg-gradient-to-br from-[#ffd89b]/20 to-[#d4af7a]/20 rounded-xl border border-[#ffd89b]/30 shadow-[0_0_20px_rgba(255,216,155,0.2)]">
                            <UserPlus className="w-5 h-5 text-[#ffd89b]" />
                            <div className="absolute inset-0 bg-[#ffd89b]/10 rounded-xl blur-sm" />
                        </div>
                        <h3 className="text-xl font-bold text-[#ffd89b]" style={{
                            textShadow: '0 0 15px rgba(255,216,155,0.3), 0 2px 4px rgba(0,0,0,0.5)'
                        }}>
                            {title}
                        </h3>
                    </div>
                    <button
                        onClick={onClose}
                        className="p-2 hover:bg-[#ffd89b]/10 rounded-lg transition-all duration-300 text-gray-400 hover:text-[#ffd89b] border border-transparent hover:border-[#ffd89b]/30"
                    >
                        <X className="w-5 h-5" />
                    </button>
                </div>

                {/* Content */}
                <div className="flex-1 overflow-y-auto p-6 custom-scrollbar">
                    <div className="space-y-4">
                        {/* Full Name & Gender */}
                        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                            <label className="flex flex-col gap-2">
                                <span className="text-sm font-medium text-gray-300">
                                    Họ và tên <span className="text-[#ffd89b]">*</span>
                                </span>
                                <input
                                    className={`px-4 py-2.5 bg-white/5 border rounded-lg focus:ring-2 focus:ring-[#ffd89b]/50 transition-all text-white placeholder-gray-500 outline-none ${
                                        submitted && nameError
                                            ? "border-red-500/50 focus:border-red-500"
                                            : "border-[#ffd89b]/30 focus:border-[#ffd89b]"
                                    }`}
                                    value={form.fullName}
                                    onChange={(e) => setForm((f) => ({ ...f, fullName: e.target.value }))}
                                    placeholder="Nguyễn Văn A"
                                />
                                {submitted && nameError && (
                                    <span className="text-xs text-red-400">{nameError}</span>
                                )}
                            </label>

                            <label className="flex flex-col gap-2">
                                <span className="text-sm font-medium text-gray-300">
                                    Giới tính <span className="text-[#ffd89b]">*</span>
                                </span>
                                <select
                                    className={`px-4 py-2.5 bg-white/5 border rounded-lg focus:ring-2 focus:ring-[#ffd89b]/50 transition-all text-white outline-none ${
                                        submitted && genderError
                                            ? "border-red-500/50 focus:border-red-500"
                                            : "border-[#ffd89b]/30 focus:border-[#ffd89b]"
                                    }`}
                                    style={{
                                        colorScheme: 'dark'
                                    }}
                                    value={form.gender || ""}
                                    onChange={(e) => setForm((f) => ({ ...f, gender: (e.target.value as any) || "" }))}
                                >
                                    <option value="" className="bg-[#1e2a3a] text-white">—</option>
                                    <option value="male" className="bg-[#1e2a3a] text-white">Nam</option>
                                    <option value="female" className="bg-[#1e2a3a] text-white">Nữ</option>
                                    <option value="other" className="bg-[#1e2a3a] text-white">Khác</option>
                                </select>
                                {submitted && genderError && (
                                    <span className="text-xs text-red-400">{genderError}</span>
                                )}
                            </label>
                        </div>

                        {/* Birth Date & Birth Place */}
                        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                            <label className="flex flex-col gap-2">
                                <span className="text-sm font-medium text-gray-300">
                                    Ngày sinh <span className="text-[#ffd89b]">*</span>
                                </span>
                                <input
                                    type="date"
                                    className={`px-4 py-2.5 bg-white/5 border rounded-lg focus:ring-2 focus:ring-[#ffd89b]/50 transition-all text-white outline-none ${
                                        submitted && birthDateError
                                            ? "border-red-500/50 focus:border-red-500"
                                            : "border-[#ffd89b]/30 focus:border-[#ffd89b]"
                                    }`}
                                    max={today}
                                    value={form.birthDate || ""}
                                    onChange={(e) => setForm((f) => ({ ...f, birthDate: e.target.value || undefined }))}
                                />
                                {submitted && birthDateError && (
                                    <span className="text-xs text-red-400">{birthDateError}</span>
                                )}
                            </label>

                            <label className="flex flex-col gap-2">
                                <span className="text-sm font-medium text-gray-300">Nơi sinh</span>
                                <input
                                    className="px-4 py-2.5 bg-white/5 border border-[#ffd89b]/30 rounded-lg focus:ring-2 focus:ring-[#ffd89b]/50 focus:border-[#ffd89b] transition-all text-white placeholder-gray-500 outline-none"
                                    value={form.birthPlace || ""}
                                    onChange={(e) => setForm((f) => ({ ...f, birthPlace: e.target.value || undefined }))}
                                    placeholder="Hà Nội"
                                />
                            </label>
                        </div>

                        {/* Phone & Email */}
                        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                            <label className="flex flex-col gap-2">
                                <span className="text-sm font-medium text-gray-300">Số điện thoại</span>
                                <input
                                    className="px-4 py-2.5 bg-white/5 border border-[#ffd89b]/30 rounded-lg focus:ring-2 focus:ring-[#ffd89b]/50 focus:border-[#ffd89b] transition-all text-white placeholder-gray-500 outline-none"
                                    value={form.phone || ""}
                                    onChange={(e) => setForm((f) => ({ ...f, phone: e.target.value}))}
                                    placeholder="0901234567"
                                />
                            </label>

                            <label className="flex flex-col gap-2">
                                <span className="text-sm font-medium text-gray-300">Email</span>
                                <input
                                    type="email"
                                    className={`px-4 py-2.5 bg-white/5 border rounded-lg focus:ring-2 focus:ring-[#ffd89b]/50 transition-all text-white placeholder-gray-500 outline-none ${
                                        submitted && emailError
                                            ? "border-red-500/50 focus:border-red-500"
                                            : "border-[#ffd89b]/30 focus:border-[#ffd89b]"
                                    }`}
                                    value={form.email || ""}
                                    onChange={(e) => setForm((f) => ({ ...f, email: e.target.value }))}
                                    placeholder="ten@example.com"
                                />
                                {submitted && emailError && (
                                    <span className="text-xs text-red-400">{emailError}</span>
                                )}
                                {!emailError && form.email?.trim() && emailExists && (
                                    <div className="space-y-2">
                                        <div className="text-xs text-amber-300 bg-amber-500/10 border border-amber-500/30 rounded-lg px-3 py-2">
                                            Email đã có tài khoản trong hệ thống.
                                        </div>
                                        <label className="inline-flex items-center gap-2 select-none">
                                            <input
                                                type="checkbox"
                                                className="size-4 accent-[#ffd89b]"
                                                checked={sendNotify}
                                                onChange={(e) => setSendNotify(e.target.checked)}
                                            />
                                            <span className="text-sm text-gray-300">Gửi thông báo sau khi tạo</span>
                                        </label>
                                    </div>
                                )}
                            </label>
                        </div>

                        {/* Deceased Checkbox */}
                        <div className="pt-2">
                            <label className="inline-flex items-center gap-2 select-none cursor-pointer">
                                <input
                                    type="checkbox"
                                    className="size-4 accent-[#ffd89b]"
                                    checked={isDeceased}
                                    onChange={(e) => toggleDeceased(e.target.checked)}
                                />
                                <span className="text-sm font-medium text-gray-300">Đã mất</span>
                            </label>
                        </div>

                        {/* Death Date & Death Place */}
                        {isDeceased && (
                            <div className="grid grid-cols-1 md:grid-cols-2 gap-4 p-4 bg-white/5 rounded-lg border border-[#ffd89b]/20">
                                <label className="flex flex-col gap-2">
                                    <span className="text-sm font-medium text-gray-300">
                                        Ngày mất <span className="text-[#ffd89b]">*</span>
                                    </span>
                                    <input
                                        type="date"
                                        className={`px-4 py-2.5 bg-white/5 border rounded-lg focus:ring-2 focus:ring-[#ffd89b]/50 transition-all text-white outline-none ${
                                            submitted && deathDateError
                                                ? "border-red-500/50 focus:border-red-500"
                                                : "border-[#ffd89b]/30 focus:border-[#ffd89b]"
                                        }`}
                                        max={today}
                                        min={form.birthDate || undefined}
                                        value={form.deathDate || ""}
                                        onChange={(e) => setForm((f) => ({ ...f, deathDate: e.target.value || undefined }))}
                                    />
                                    {submitted && deathDateError && (
                                        <span className="text-xs text-red-400">{deathDateError}</span>
                                    )}
                                </label>

                                <label className="flex flex-col gap-2">
                                    <span className="text-sm font-medium text-gray-300">Nơi mất</span>
                                    <input
                                        className="px-4 py-2.5 bg-white/5 border border-[#ffd89b]/30 rounded-lg focus:ring-2 focus:ring-[#ffd89b]/50 focus:border-[#ffd89b] transition-all text-white placeholder-gray-500 outline-none"
                                        value={form.deathPlace || ""}
                                        onChange={(e) => setForm((f) => ({ ...f, deathPlace: e.target.value || undefined }))}
                                        placeholder="Hà Nội"
                                    />
                                </label>
                            </div>
                        )}

                        {/* Biography */}
                        <label className="flex flex-col gap-2">
                            <span className="text-sm font-medium text-gray-300">Tiểu sử</span>
                            <textarea
                                className="px-4 py-2.5 bg-white/5 border border-[#ffd89b]/30 rounded-lg focus:ring-2 focus:ring-[#ffd89b]/50 focus:border-[#ffd89b] transition-all text-white placeholder-gray-500 outline-none min-h-[100px] resize-none"
                                value={form.biography || ""}
                                onChange={(e) => setForm((f) => ({ ...f, biography: e.target.value || undefined }))}
                                placeholder="Một vài dòng mô tả..."
                            />
                        </label>

                        {/* Avatar Upload */}
                        <div className="space-y-2">
                            <span className="block text-sm font-medium text-gray-300">Ảnh đại diện</span>

                            {preview ? (
                                <div className="relative inline-block group">
                                    <img
                                        src={preview}
                                        alt="avatar preview"
                                        className="w-24 h-24 rounded-xl object-cover border-2 border-[#ffd89b]/30 cursor-pointer hover:border-[#ffd89b]/60 transition-all shadow-[0_0_20px_rgba(255,216,155,0.2)]"
                                        onClick={() => inputRef.current?.click()}
                                    />
                                    <button
                                        type="button"
                                        onClick={clearImage}
                                        disabled={submitting}
                                        className="absolute -top-2 -right-2 bg-gradient-to-br from-red-500 to-red-600 border border-red-400/50 rounded-full w-7 h-7 flex items-center justify-center text-white hover:from-red-600 hover:to-red-700 shadow-lg transition-all duration-300 disabled:opacity-50 disabled:cursor-not-allowed"
                                        aria-label="Xoá ảnh"
                                    >
                                        <X size={16} />
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
                                    className={`relative overflow-hidden border-2 border-dashed rounded-xl px-6 py-8 text-center cursor-pointer transition-all duration-300 ${
                                        dragOver
                                            ? "bg-[#ffd89b]/10 border-[#ffd89b]"
                                            : "bg-white/5 border-[#ffd89b]/30 hover:bg-white/10 hover:border-[#ffd89b]/50"
                                    }`}
                                    onClick={() => inputRef.current?.click()}
                                >
                                    <div className="flex flex-col items-center gap-2">
                                        <div className="p-3 bg-[#ffd89b]/10 rounded-full">
                                            <Upload className="w-6 h-6 text-[#ffd89b]" />
                                        </div>
                                        <div className="text-sm text-gray-300">
                                            Kéo thả ảnh vào đây hoặc <span className="text-[#ffd89b] font-medium">bấm để chọn</span>
                                        </div>
                                        <div className="text-xs text-gray-500">Tối đa 5MB</div>
                                    </div>
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

                {/* Footer */}
                <div className="p-6 border-t border-[#ffd89b]/20 bg-gradient-to-r from-[#ffd89b]/5 to-transparent flex justify-end gap-3">
                    <button
                        onClick={onClose}
                        className="px-6 py-2.5 bg-white/5 hover:bg-white/10 border border-[#ffd89b]/30 hover:border-[#ffd89b]/50 text-gray-300 hover:text-white rounded-lg transition-all duration-300 font-medium"
                    >
                        Hủy
                    </button>
                    <button
                        onClick={handleSubmit}
                        disabled={submitting}
                        className="relative overflow-hidden px-6 py-2.5 bg-gradient-to-r from-[#d4af7a] via-[#ffd89b] to-[#d4af7a] bg-[length:200%_100%] hover:bg-[position:100%] text-[#0f1419] rounded-lg transition-all duration-500 font-semibold shadow-[0_8px_30px_rgba(255,216,155,0.3)] hover:shadow-[0_12px_40px_rgba(255,216,155,0.5)] hover:scale-105 disabled:opacity-50 disabled:cursor-not-allowed disabled:hover:scale-100 group"
                    >
                        {submitting ? (
                            <span className="inline-flex items-center gap-2">
                                <Loader2 className="animate-spin w-4 h-4" /> Đang lưu…
                            </span>
                        ) : (
                            "Lưu"
                        )}
                        <div className="absolute inset-0 -translate-x-full group-hover:translate-x-full transition-transform duration-1000 bg-gradient-to-r from-transparent via-white/30 to-transparent skew-x-12" />
                    </button>
                </div>

                {/* Decorative bottom border glow */}
                <div className="absolute bottom-0 left-0 right-0 h-[2px] bg-gradient-to-r from-transparent via-[#ffd89b] to-transparent opacity-60" />
            </div>

            <style>{`
                .custom-scrollbar::-webkit-scrollbar {
                    width: 8px;
                }
                .custom-scrollbar::-webkit-scrollbar-track {
                    background: rgba(255, 216, 155, 0.05);
                    border-radius: 4px;
                }
                .custom-scrollbar::-webkit-scrollbar-thumb {
                    background: rgba(255, 216, 155, 0.3);
                    border-radius: 4px;
                }
                .custom-scrollbar::-webkit-scrollbar-thumb:hover {
                    background: rgba(255, 216, 155, 0.5);
                }
            `}</style>
        </div>
    );
}