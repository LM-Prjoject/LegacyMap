import React, { useEffect, useMemo, useState } from "react";
import { ShieldCheck, Eye, EyeOff, CheckCircle2 } from "lucide-react";
import { showToast } from "@/lib/toast";
import { authApi } from "@/api/auth";

function PasswordInput({
                           label,
                           value,
                           onChange,
                       }: {
    label: string;
    value: string;
    onChange: (v: string) => void;
}) {
    const [show, setShow] = useState(false);
    return (
        <label className="block">
            <span className="block text-sm font-medium text-slate-700 mb-1">{label}</span>
            <div className="relative">
                <input
                    type={show ? "text" : "password"}
                    className="w-full rounded-xl border border-slate-200 bg-white/70 backdrop-blur px-3 py-2 outline-none focus:ring-2 focus:ring-amber-500 focus:border-amber-500 shadow-sm"
                    value={value}
                    onChange={(e) => onChange(e.target.value)}
                    placeholder="••••••••"
                />
                <button
                    type="button"
                    onClick={() => setShow((s) => !s)}
                    className="absolute right-2 top-1/2 -translate-y-1/2 p-1 rounded-md hover:bg-slate-100"
                >
                    {show ? <EyeOff className="h-5 w-5" /> : <Eye className="h-5 w-5" />}
                </button>
            </div>
        </label>
    );
}

function StrengthBar({ value }: { value: string }) {
    const score = useMemo(() => {
        let s = 0;
        if (/[a-z]/.test(value)) s++;
        if (/[A-Z]/.test(value)) s++;
        if (/\d/.test(value)) s++;
        if (/[^\w\s]/.test(value)) s++;
        s += Math.min(2, Math.floor((value?.length || 0) / 6));
        return Math.min(6, s);
    }, [value]);

    const pct = Math.round((score / 6) * 100);
    const color = pct >= 70 ? "bg-emerald-500" : pct >= 40 ? "bg-amber-500" : "bg-rose-500";
    const label = pct >= 70 ? "Strong" : pct >= 40 ? "Medium" : "Weak";

    return (
        <div className="text-xs text-slate-600">
            <div className="h-1.5 w-full bg-slate-200 rounded-full overflow-hidden">
                <div className={`h-full ${color}`} style={{ width: `${pct}%` }} />
            </div>
            <div className="mt-1 flex justify-between">
                <span>Password strength</span>
                <span className="font-medium">{label}</span>
            </div>
        </div>
    );
}

function generateStrongPassword(length = 12): string {
    const upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    const lower = "abcdefghijklmnopqrstuvwxyz";
    const digits = "0123456789";
    const special = "!@#$%^&*()-_=+[]{}|;:,.<>?";
    const all = upper + lower + digits + special;

    let password = [
        upper[Math.floor(Math.random() * upper.length)],
        lower[Math.floor(Math.random() * lower.length)],
        digits[Math.floor(Math.random() * digits.length)],
        special[Math.floor(Math.random() * special.length)],
    ];

    for (let i = password.length; i < length; i++) {
        password.push(all[Math.floor(Math.random() * all.length)]);
    }

    return password.sort(() => Math.random() - 0.5).join("");
}

export default function AccountSecuritySection({
                                                   me,
                                                   onChanged,
                                               }: { me: any; onChanged?: (u: any) => void }) {
    const [current, setCurrent] = useState("");
    const [nextPwd, setNextPwd] = useState("");
    const [confirm, setConfirm] = useState("");
    const [submitting, setSubmitting] = useState(false);
    const [error, setError] = useState<string | null>(null);

    const canSubmit = useMemo(() => {
        if (!current || !nextPwd || !confirm) return false;
        if (nextPwd !== confirm) return false;
        if (nextPwd.length < 8) return false;
        return true;
    }, [current, nextPwd, confirm]);

    useEffect(() => {
        if (window.location.hash === "#security") {
            document.getElementById("security")?.scrollIntoView({ behavior: "smooth" });
        }
    }, []);

    const onSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        setError(null);

        if (!canSubmit || submitting) return;

        if (current === nextPwd) {
            setError("Mật khẩu mới không được trùng với mật khẩu hiện tại.");
            return;
        }

        setSubmitting(true);
        await authApi.changePassword({ currentPassword: current, newPassword: nextPwd });

        const iso = new Date().toISOString();
        const updated = { ...me, passwordChangedAt: iso };
        localStorage.setItem("user", JSON.stringify(updated));
        window.dispatchEvent(new Event("storage"));

        onChanged?.(updated);

        setCurrent(""); setNextPwd(""); setConfirm("");
        showToast.success("Đổi mật khẩu thành công");

    }

    const rawChangedAt = me?.passwordChangedAt ?? me?.password_changed_at ?? null;

    const lastChangedLabel = rawChangedAt
        ? new Intl.DateTimeFormat("vi-VN", { dateStyle: "medium", timeStyle: "short" })
            .format(new Date(rawChangedAt))
        : "Chưa có dữ liệu";

    const hasChanged = Boolean(rawChangedAt);

    return (
        <section
            id="security"
            className="mt-8 rounded-3xl border border-white/30 shadow-2xl p-6 md:p-8 bg-white/25 backdrop-blur-md"
        >
            <div className="flex items-start justify-between gap-4">
                <h3 className="text-lg md:text-xl font-semibold tracking-tight flex items-center gap-2">
                    <ShieldCheck className="h-5 w-5 text-amber-600" /> Bảo mật tài khoản
                </h3>

                <div className="flex items-center gap-2 text-xs">
            <span
                className={`inline-flex items-center gap-1 rounded-full px-2.5 py-1 ${
                hasChanged ? "bg-slate-100/70 text-slate-700" : "bg-slate-100/50 text-slate-500"}`}>
                <CheckCircle2 className={`h-4 w-4 ${hasChanged ? "text-emerald-600" : "text-slate-400"}`} />
                Lần cuối thay đổi mật khẩu: {lastChangedLabel}
            </span>
                </div>
            </div>

            <form onSubmit={onSubmit} className="mt-5 grid md:grid-cols-2 gap-6">
                <div className="space-y-4 max-w-md">
                    <PasswordInput label="Mật khẩu hiện tại" value={current} onChange={setCurrent} />
                    <div className="space-y-1 relative">
                        <PasswordInput label="Mật khẩu mới" value={nextPwd} onChange={setNextPwd} />

                        <div className="relative mt-1">
                            <StrengthBar value={nextPwd} />

                            <button
                                type="button"
                                onClick={() => setNextPwd(generateStrongPassword())}
                                className="absolute right-[5px] bottom-[75px] text-xs text-gray-700 font-medium hover:underline leading-none"
                            >
                                Tạo mật khẩu mạnh
                            </button>
                        </div>

                    </div>
                </div>
                <div className="space-y-4 max-w-md">
                    <PasswordInput label="Xác nhận mật khẩu mới" value={confirm} onChange={setConfirm} />
                    {confirm && nextPwd && nextPwd !== confirm && (
                        <p className="text-sm text-red-600 font-medium mt-1">
                            Mật khẩu xác nhận không khớp
                        </p>
                    )}
                    {error && <p className="text-sm text-red-600 font-medium">{error}</p>}
                    <ul className="text-sm list-disc pl-5 space-y-1">
                        <li className={`
                        ${nextPwd.length >= 8 ? "text-emerald-600" : "text-slate-600"}`}>
                            Ít nhất 8 ký tự
                        </li>
                        <li className={`
                        ${/[A-Z]/.test(nextPwd) ? "text-emerald-600" : "text-slate-600"}`}>
                            Có ít nhất một chữ hoa
                        </li>
                        <li className={`
                        ${/[a-z]/.test(nextPwd) ? "text-emerald-600" : "text-slate-600"}`}>
                            Có ít nhất một chữ thường
                        </li>
                        <li className={`
                        ${/\d/.test(nextPwd) ? "text-emerald-600" : "text-slate-600"}`}>
                            Có ít nhất một chữ số
                        </li>
                        <li className={`
                        ${/[^\w\s]/.test(nextPwd) ? "text-emerald-600" : "text-slate-600"}`}>
                            Có ít nhất một ký tự đặc biệt
                        </li>
                        <li className={`
                        ${nextPwd && nextPwd !== current ? "text-emerald-600" : "text-slate-600"}`}>
                            Không trùng với mật khẩu hiện tại
                        </li>
                    </ul>
                </div>
                <div className="md:col-span-2 flex items-center justify-between pt-2">
                    <button
                        type="submit"
                        disabled={!canSubmit || submitting}
                        className={`px-4 py-2 rounded-xl font-semibold text-white shadow-md transition ${
                            canSubmit && !submitting
                                ? "bg-gradient-to-r from-emerald-600 to-teal-500 hover:opacity-95"
                                : "bg-slate-300 cursor-not-allowed"
                        }`}
                    >
                        {submitting ? "Đang lưu..." : "Đổi mật khẩu"}
                    </button>
                </div>
            </form>
        </section>
    );
}