import React, { useEffect, useMemo, useState } from "react";
import { ShieldCheck, Eye, EyeOff, CheckCircle2, Sparkles } from "lucide-react";
import { showToast } from "@/lib/toast.ts";
import { authApi } from "@/api/auth.ts";

export function PasswordInput({
                                  label,
                                  value,
                                  onChange,
                                  variant = "dark",
                              }: {
    label: string;
    value: string;
    onChange: (v: string) => void;
    variant?: "dark" | "light";
}) {
    const [show, setShow] = useState(false);

    const isLight = variant === "light";

    const labelStyle = isLight
        ? { color: "#2a3548" }
        : { color: "#ffd89b" };

    const inputClassName = isLight
        ? "w-full rounded-xl border-2 px-4 py-3 outline-none focus:ring-2 focus:ring-[#2a3548] font-medium pr-10"
        : "w-full rounded-xl border border-[#ffd89b]/20 bg-white/5 backdrop-blur-xl px-4 py-3 text-white placeholder:text-white/40 outline-none focus:ring-2 focus:ring-[#ffd89b]/50 focus:border-[#ffd89b]/50 shadow-lg transition-all duration-300 pr-10";

    const inputStyle = isLight
        ? {
            background:
                "linear-gradient(135deg, rgba(255, 255, 255, 0.8) 0%, rgba(255, 255, 255, 0.6) 100%)",
            borderColor: "rgba(42, 53, 72, 0.3)",
            color: "#2a3548",
        }
        : undefined;

    const iconClassName = isLight
        ? "absolute right-3 top-1/2 -translate-y-1/2 p-1 text-[#2a3548]/60 hover:text-[#2a3548] transition"
        : "absolute right-3 top-1/2 -translate-y-1/2 p-1.5 rounded-lg hover:bg-white/10 text-white/70 hover:text-[#ffd89b] transition-all duration-300";

    return (
        <label className="block">
      <span
          className={
              isLight
                  ? "block text-sm font-semibold mb-2"
                  : "block text-[15px] font-semibold text-[#ffd89b] tracking-wide mb-2"
          }
          style={labelStyle}
      >
        {label}
      </span>
            <div className="relative">
                <input
                    type={show ? "text" : "password"}
                    className={inputClassName}
                    style={inputStyle}
                    value={value}
                    onChange={(e) => onChange(e.target.value)}
                    placeholder= "••••••••"
                />
                <button
                    type="button"
                    onClick={() => setShow((s) => !s)}
                    className={iconClassName}
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
    const label = pct >= 70 ? "Mạnh" : pct >= 40 ? "Trung bình" : "Yếu";

    return (
        <div className="text-sm text-white/80 mt-2">
            <div className="h-2 w-full bg-white/10 rounded-full overflow-hidden shadow-inner">
                <div className={`h-full ${color} transition-all duration-500 shadow-lg`} style={{ width: `${pct}%` }} />
            </div>
            <div className="mt-2 flex justify-between items-center">
                <span className="text-white/70">Độ mạnh mật khẩu</span>
                <span className="font-semibold text-[#ffd89b]">{label}</span>
            </div>
        </div>
    );
}

function generateStrongPassword(length =18): string {
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
        try {
            await authApi.changePassword({ currentPassword: current, newPassword: nextPwd });

            showToast.success("Đổi mật khẩu thành công. Vui lòng đăng nhập lại.");

            setCurrent("");
            setNextPwd("");
            setConfirm("");

            authApi.logout();

            setTimeout(() => {
                window.location.href = "/signin";
            }, 800);

            return;
        } catch (err: any) {
            setError(err?.message || "Đổi mật khẩu thất bại");
            showToast.error(err?.message || "Đổi mật khẩu thất bại");
        } finally {
            setSubmitting(false);
        }
    };

    const rawChangedAt = me?.passwordChangedAt ?? me?.password_changed_at ?? null;

    const lastChangedLabel = rawChangedAt
        ? new Intl.DateTimeFormat("vi-VN", {dateStyle: "medium", timeStyle: "short",})
            .format(new Date(rawChangedAt))
        : null;

    const hasChanged = Boolean(rawChangedAt);

    return (
        <section
            id="security"
            className="mt-12 rounded-3xl border border-[#ffd89b]/20 shadow-[0_10px_40px_rgba(0,0,0,0.4),0_0_40px_rgba(255,216,155,0.05)] p-10 bg-gradient-to-br from-white/5 via-white/3 to-white/3 backdrop-blur-xl transition-all duration-500 hover:border-[#ffd89b]/30 hover:shadow-[0_15px_50px_rgba(0,0,0,0.5),0_0_60px_rgba(255,216,155,0.1)]"
        >
            {/* Header */}
            <div className="flex flex-col sm:flex-row items-start sm:items-center justify-between gap-4 mb-8">
                <h3 className="text-3xl font-bold tracking-tight flex items-center gap-3">
                    <span className="relative">
                        <span className="absolute inset-0 blur-lg opacity-10" style={{
                            background: 'linear-gradient(to right, #ffd89b, #f5e6d3, #d4af7a)',
                            WebkitBackgroundClip: 'text',
                            WebkitTextFillColor: 'transparent',
                            backgroundClip: 'text'
                        }}>
                            <ShieldCheck className="h-8 w-8" />
                        </span>
                        <ShieldCheck className="h-8 w-8 relative" style={{
                            color: '#ffd89b',
                            filter: 'drop-shadow(0 0 10px rgba(255,216,155,0.3))'
                        }} />
                    </span>
                    <span className="relative">
                        <span className="absolute inset-0 blur-lg opacity-10" style={{
                            background: 'linear-gradient(to right, #ffd89b, #f5e6d3, #d4af7a)',
                            WebkitBackgroundClip: 'text',
                            WebkitTextFillColor: 'transparent',
                            backgroundClip: 'text'
                        }}>
                            Bảo mật tài khoản
                        </span>
                        <span className="relative" style={{
                            color: '#ffd89b',
                            textShadow: '0 0 10px rgba(255,216,155,0.2), 0 0 5px rgba(255,216,155,0.15)'
                        }}>
                            Bảo mật tài khoản
                        </span>
                    </span>
                </h3>

                {hasChanged && lastChangedLabel && (
                <div className="flex items-center gap-2 text-sm">
                    <span className={`inline-flex items-center gap-2 rounded-full px-4 py-2 ${
                        hasChanged
                            ? "bg-emerald-900/40 text-emerald-300 border border-emerald-400/30 shadow-[0_0_20px_rgba(52,211,153,0.2)]"
                            : "bg-white/5 text-white/60 border border-white/20"
                    }`}>
                        <CheckCircle2 className={`h-4 w-4 ${hasChanged ? "text-emerald-400 animate-pulse" : "text-white/40"}`} />
                        <span className="font-medium">
                            Lần cuối thay đổi mật khẩu: {lastChangedLabel}
                        </span>
                    </span>
                </div>
                    )}
            </div>

            {/* Form */}
            <form onSubmit={onSubmit} className="space-y-8">
                <div className="grid md:grid-cols-2 gap-8">
                    {/* Left Column */}
                    <div className="space-y-6">
                        <PasswordInput
                            label="Mật khẩu hiện tại"
                            value={current}
                            onChange={setCurrent}
                        />

                        <div className="space-y-2">
                            <div className="flex items-center justify-between">
                                <PasswordInput
                                    label="Mật khẩu mới"
                                    value={nextPwd}
                                    onChange={setNextPwd}
                                />
                            </div>

                            {nextPwd.length > 0 && <StrengthBar value={nextPwd} />}

                            <button
                                type="button"
                                onClick={() => {
                                    const pwd = generateStrongPassword();
                                    setNextPwd(pwd);
                                    showToast.success("Đã tạo mật khẩu mạnh!");
                                }}
                                className="mt-3 inline-flex items-center gap-2 px-4 py-2 rounded-lg bg-gradient-to-r from-[#d4af7a]/20 to-[#ffd89b]/20 border border-[#ffd89b]/30 text-[#ffd89b] text-sm font-medium hover:from-[#d4af7a]/30 hover:to-[#ffd89b]/30 hover:shadow-[0_0_20px_rgba(255,216,155,0.2)] transition-all duration-300"
                            >
                                <Sparkles className="h-4 w-4" />
                                Tạo mật khẩu mạnh
                            </button>
                        </div>
                    </div>

                    {/* Right Column */}
                    <div className="space-y-6">
                        <PasswordInput
                            label="Xác nhận mật khẩu mới"
                            value={confirm}
                            onChange={setConfirm}
                        />

                        {confirm && nextPwd && nextPwd !== confirm && (
                            <div className="rounded-lg bg-rose-500/10 border border-rose-500/30 px-4 py-3 flex items-start gap-2">
                                <span className="text-rose-400 text-sm font-medium">
                                    Mật khẩu xác nhận không khớp
                                </span>
                            </div>
                        )}

                        {error && (
                            <div className="rounded-lg bg-rose-500/10 border border-rose-500/30 px-4 py-3 flex items-start gap-2">
                                <span className="text-rose-400 text-sm font-medium">
                                  {error}
                                </span>
                            </div>
                        )}

                        {/* Requirements */}
                        <div className="rounded-xl bg-white/5 border border-[#ffd89b]/10 p-4">
                            <p className="text-sm font-semibold text-[#ffd89b] mb-3">Yêu cầu mật khẩu:</p>
                            <ul className="space-y-2">
                                <li className={`flex items-center gap-2 text-sm transition-colors duration-300 ${
                                    nextPwd.length >= 8 ? "text-emerald-400" : "text-white/50"
                                }`}>
                                    <span className={`flex items-center justify-center w-5 h-5 rounded-full ${
                                        nextPwd.length >= 8 ? "bg-emerald-500/20 text-emerald-400" : "bg-white/5 text-white/30"
                                    }`}>
                                        {nextPwd.length >= 8 ? "✓" : "○"}
                                    </span>
                                    Ít nhất 8 ký tự
                                </li>
                                <li className={`flex items-center gap-2 text-sm transition-colors duration-300 ${
                                    /[A-Z]/.test(nextPwd) ? "text-emerald-400" : "text-white/50"
                                }`}>
                                    <span className={`flex items-center justify-center w-5 h-5 rounded-full ${
                                        /[A-Z]/.test(nextPwd) ? "bg-emerald-500/20 text-emerald-400" : "bg-white/5 text-white/30"
                                    }`}>
                                        {/[A-Z]/.test(nextPwd) ? "✓" : "○"}
                                    </span>
                                    Có ít nhất một chữ hoa
                                </li>
                                <li className={`flex items-center gap-2 text-sm transition-colors duration-300 ${
                                    /[a-z]/.test(nextPwd) ? "text-emerald-400" : "text-white/50"
                                }`}>
                                    <span className={`flex items-center justify-center w-5 h-5 rounded-full ${
                                        /[a-z]/.test(nextPwd) ? "bg-emerald-500/20 text-emerald-400" : "bg-white/5 text-white/30"
                                    }`}>
                                        {/[a-z]/.test(nextPwd) ? "✓" : "○"}
                                    </span>
                                    Có ít nhất một chữ thường
                                </li>
                                <li className={`flex items-center gap-2 text-sm transition-colors duration-300 ${
                                    /\d/.test(nextPwd) ? "text-emerald-400" : "text-white/50"
                                }`}>
                                    <span className={`flex items-center justify-center w-5 h-5 rounded-full ${
                                        /\d/.test(nextPwd) ? "bg-emerald-500/20 text-emerald-400" : "bg-white/5 text-white/30"
                                    }`}>
                                        {/\d/.test(nextPwd) ? "✓" : "○"}
                                    </span>
                                    Có ít nhất một chữ số
                                </li>
                                <li className={`flex items-center gap-2 text-sm transition-colors duration-300 ${
                                    /[^\w\s]/.test(nextPwd) ? "text-emerald-400" : "text-white/50"
                                }`}>
                                    <span className={`flex items-center justify-center w-5 h-5 rounded-full ${
                                        /[^\w\s]/.test(nextPwd) ? "bg-emerald-500/20 text-emerald-400" : "bg-white/5 text-white/30"
                                    }`}>
                                        {/[^\w\s]/.test(nextPwd) ? "✓" : "○"}
                                    </span>
                                    Có ít nhất một ký tự đặc biệt
                                </li>
                                <li className={`flex items-center gap-2 text-sm transition-colors duration-300 ${
                                    nextPwd && nextPwd !== current ? "text-emerald-400" : "text-white/50"
                                }`}>
                                    <span className={`flex items-center justify-center w-5 h-5 rounded-full ${
                                        nextPwd && nextPwd !== current ? "bg-emerald-500/20 text-emerald-400" : "bg-white/5 text-white/30"
                                    }`}>
                                        {nextPwd && nextPwd !== current ? "✓" : "○"}
                                    </span>
                                    Không trùng với mật khẩu hiện tại
                                </li>
                            </ul>
                        </div>
                    </div>
                </div>

                {/* Submit Button */}
                <div className="flex items-center justify-end pt-4">
                    <button
                        type="submit"
                        disabled={!canSubmit || submitting}
                        className={`px-8 py-3.5 rounded-xl font-semibold text-lg shadow-lg transition-all duration-300 ${
                            canSubmit && !submitting
                                ? "bg-gradient-to-r from-[#d4af7a] via-[#ffd89b] to-[#d4af7a] text-[#0f1419] hover:shadow-[0_12px_35px_rgba(255,216,155,0.5),0_0_25px_rgba(255,216,155,0.3)] hover:scale-105 active:scale-100"
                                : "bg-white/10 text-white/40 cursor-not-allowed"
                        }`}
                    >
                        {submitting ? (
                            <span className="flex items-center gap-2">
                                <svg className="animate-spin h-5 w-5" viewBox="0 0 24 24">
                                    <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4" fill="none" />
                                    <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z" />
                                </svg>
                                Đang lưu...
                            </span>
                        ) : (
                            "Đổi mật khẩu"
                        )}
                    </button>
                </div>
            </form>
        </section>
    );
}