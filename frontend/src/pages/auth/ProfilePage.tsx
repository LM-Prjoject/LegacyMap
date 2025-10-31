import { useEffect, useMemo, useRef, useState, ChangeEvent } from "react";
import Navbar from "@/components/layout/Navbar";
import { Edit3 } from "lucide-react";
import { authApi, type User, type UserProfile, type Address } from "@/api/auth";
import { showToast } from "@/lib/toast";
import { uploadAvatarToSupabase } from "@/lib/upload";
import PicChim from "@/assets/picchim.png";
import PicNoi from "@/assets/picnoi.png";
import ProfileEditModal, { type Form } from "@/components/ProfileEditModal/ProfileEditModal";
import { fetchProvinces, fetchWardsByProvince, type Province, type Ward } from "@/api/locations";
import { type Option } from "@/components/ui/SearchCombo";
import AccountSecuritySection from "@/components/AccountSecuritySection/AccountSecuritySection";

function LabeledText({ label, value }: { label: string; value: string }) {
    return (
        <div>
            <div className="text-[15px] font-semibold text-[#ffd89b] tracking-wide mb-1">{label}</div>
            <div className="text-lg font-medium text-white/95 leading-snug break-words">{value}</div>
        </div>
    );
}

const emptyAddr: Address = { houseNumber: "", ward: "", city: "" };

export default function ProfilePage() {
    const [me, setMe] = useState<User | null>(null);
    const [form, setForm] = useState<Form>({ address: { ...emptyAddr } });
    const [saving, setSaving] = useState(false);
    const [editing, setEditing] = useState(false);
    const [uploadingAvatar, setUploadingAvatar] = useState(false);
    const fileRef = useRef<HTMLInputElement | null>(null);

    const [provinces, setProvinces] = useState<Province[]>([]);
    const [provinceCode, setProvinceCode] = useState<number | "">("");
    const [wards, setWards] = useState<Ward[]>([]);
    const [wardCode, setWardCode] = useState<number | "">("");

    useEffect(() => {
        fetchProvinces().then(setProvinces).catch(() => setProvinces([]));
    }, []);

    /** Load user info */
    useEffect(() => {
        (async () => {
            const u = await authApi.getMe();
            setMe(u);
            const a = (u.profile?.address || {}) as Address;
            const dob = (u.profile?.dob || "").split("T")[0] || "";
            setForm({
                fullName: u.profile?.fullName || "",
                clanName: u.profile?.clanName || "",
                gender: (u.profile?.gender as Form["gender"]) || "",
                phone: u.profile?.phone || "",
                dob,
                avatarUrl: u.profile?.avatarUrl || "",
                address: {
                    houseNumber: a?.houseNumber || "",
                    ward: a?.ward || "",
                    city: a?.city || "",
                },
            });
        })();
    }, []);

    const clean = (s?: string) => (s && s.trim().length ? s.trim() : undefined);

    const buildPayloadFromForm = (f: Form): UserProfile => ({
        fullName: clean(f.fullName),
        clanName: clean(f.clanName),
        gender: f.gender || undefined,
        phone: clean(f.phone),
        dob: clean(f.dob),
        avatarUrl: clean(f.avatarUrl),
        address: {
            houseNumber: clean(f.address?.houseNumber),
            ward: clean(f.address?.ward),
            city: clean(f.address?.city),
        },
    });

    const buildPayloadFromProfile = (p?: UserProfile): UserProfile => {
        const a = (p?.address || {}) as Address;
        const dob = (p?.dob || "").split("T")[0] || "";
        return {
            fullName: clean(p?.fullName),
            clanName: clean(p?.clanName),
            gender: (p?.gender as Form["gender"]) || undefined,
            phone: clean(p?.phone),
            dob: clean(dob),
            avatarUrl: clean(p?.avatarUrl),
            address: {
                houseNumber: clean(a?.houseNumber),
                ward: clean(a?.ward),
                city: clean(a?.city),
            },
        };
    };

    const hasChanges = useMemo(() => {
        if (!me) return false;
        const current = buildPayloadFromForm(form);
        const original = buildPayloadFromProfile(me.profile);
        return JSON.stringify(current) !== JSON.stringify(original);
    }, [form, me]);

    const onFieldChange = (e: ChangeEvent<HTMLInputElement | HTMLSelectElement>) => {
        const { name, value } = e.target;
        if (name.startsWith("address.")) {
            const k = name.split(".")[1] as keyof Address;
            setForm((p) => ({ ...p, address: { ...p.address, [k]: value } }));
        } else {
            setForm((p) => ({ ...p, [name]: value }));
        }
    };

    const onPickAvatar = () => {
        if (saving || uploadingAvatar) return;
        fileRef.current?.click();
    };

    const onAvatarSelected = async (e: ChangeEvent<HTMLInputElement>) => {
        const file = e.target.files?.[0];
        if (!file) return;
        try {
            setUploadingAvatar(true);
            const url = await uploadAvatarToSupabase(file);
            setForm((p) => ({ ...p, avatarUrl: url }));
            showToast.success("Tải ảnh lên thành công");
        } catch (err: any) {
            showToast.error(err?.message || "Không thể tải ảnh lên");
        } finally {
            setUploadingAvatar(false);
            e.target.value = "";
        }
    };

    const onSave = async () => {
        if (!me) return;
        if (!hasChanges) {
            showToast.warning("Không có thông tin nào thay đổi");
            setEditing(false);
            return;
        }
        setSaving(true);
        try {
            const payload = buildPayloadFromForm(form);
            await authApi.updateUser(me.id, payload);
            const updatedUser = { ...me, profile: { ...(me.profile || {}), ...payload } };
            setMe(updatedUser);
            localStorage.setItem("user", JSON.stringify(updatedUser));
            window.dispatchEvent(new Event("storage"));
            showToast.success("Lưu thông tin thành công");
            setEditing(false);
        } catch (err: any) {
            showToast.error(err?.message || "Lưu thông tin thất bại");
        } finally {
            setSaving(false);
        }
    };

    const onProvinceChange = async (codeStr: string) => {
        const code = codeStr ? Number(codeStr) : "";
        setProvinceCode(code);
        setWards([]);
        setWardCode("");
        const p = provinces.find((x) => x.code === code);
        setForm((prev) => ({ ...prev, address: { ...prev.address, city: p?.name || "", ward: "" } }));
        if (code) {
            const ws = await fetchWardsByProvince(Number(code));
            setWards(ws);
        }
    };

    const onWardChange = (codeStr: string) => {
        const code = codeStr ? Number(codeStr) : "";
        setWardCode(code);
        const w = wards.find((x) => x.code === code);
        setForm((prev) => ({ ...prev, address: { ...prev.address, ward: w?.name || "" } }));
    };

    const provinceOptions: Option[] = provinces.map((p) => ({ value: p.code, label: p.name }));
    const wardOptions: Option[] = wards.map((w) => ({ value: w.code, label: w.name }));

    if (!me) {
        return (
            <div className="min-h-screen flex items-center justify-center text-white/80 bg-gradient-to-br from-[#0f1419] via-[#1e2a3a] to-[#0f1419]">
                Đang tải hồ sơ...
            </div>
        );
    }

    const isLocal = me?.provider?.toLowerCase?.() === "local";
    return (
        <div className={`min-h-screen overflow-x-hidden relative ${editing ? "overflow-hidden" : ""}`}>
            {/* Enhanced Background */}
            <div className="absolute inset-0 bg-gradient-to-br from-[#0f1419] via-[#1e2a3a] to-[#0f1419]"></div>

            {/* Ambient glows - reduced intensity */}
            <div className="absolute top-0 left-1/2 -translate-x-1/2 w-[1400px] h-[700px] bg-gradient-radial from-[#ffd89b]/5 via-[#d4af7a]/2 to-transparent blur-[140px] rounded-full pointer-events-none animate-pulse-slow" />
            <div className="absolute bottom-0 right-0 w-[900px] h-[550px] bg-gradient-radial from-[#ffd89b]/6 via-[#d4af7a]/2 to-transparent blur-[130px] rounded-full pointer-events-none animate-pulse-slow" />

            {/* Noise texture */}
            <div className="absolute inset-0 opacity-[0.025] pointer-events-none mix-blend-overlay bg-[url('data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIzMDAiIGhlaWdodD0iMzAwIj48ZmlsdGVyIGlkPSJhIj48ZmVUdXJidWxlbmNlIGJhc2VGcmVxdWVuY3k9Ii43NSIgc3RpdGNoVGlsZXM9InN0aXRjaCIgdHlwZT0iZnJhY3RhbE5vaXNlIi8+PGZlQ29sb3JNYXRyaXggdHlwZT0ic2F0dXJhdGUiIHZhbHVlcz0iMCIvPjwvZmlsdGVyPjxyZWN0IHdpZHRoPSIxMDAlIiBoZWlnaHQ9IjEwMCUiIGZpbHRlcj0idXJsKCNhKSIvPjwvc3ZnPg==')]" />

            {/* Decorative Images */}
            <div className="pointer-events-none fixed inset-0 z-0 overflow-hidden">
                <img
                    src={PicChim}
                    alt=""
                    className="absolute bottom-0 left-1/2 -translate-x-1/2 w-[1400px] lg:w-[1600px] h-auto opacity-15 scale-[1.05]"
                    draggable={false}
                />
                <img
                    src={PicNoi}
                    alt=""
                    className="absolute bottom-0 left-0 w-[520px] sm:w-[600px] lg:w-[680px] opacity-60 drop-shadow-2xl"
                    draggable={false}
                />
            </div>

            <Navbar />

            {/* Profile Section */}
            <div className="relative z-10 w-full px-8 py-20 animate-fade-in">
                <div className="mx-auto w-full max-w-[1200px]">
                    <div className="grid grid-cols-1 lg:grid-cols-2 gap-12">
                        {/* Info card */}
                        <section className="rounded-3xl border border-[#ffd89b]/20 shadow-[0_10px_40px_rgba(0,0,0,0.4),0_0_40px_rgba(255,216,155,0.05)] p-10 bg-gradient-to-br from-white/5 via-white/3 to-white/3 backdrop-blur-xl transition-all duration-500 hover:border-[#ffd89b]/30 hover:shadow-[0_15px_50px_rgba(0,0,0,0.5),0_0_60px_rgba(255,216,155,0.1)]">
                            <div className="flex items-center justify-between mb-8">
                                <h3 className="text-3xl font-bold tracking-tight">
                                    <span className="relative">
                                        <span className="absolute inset-0 blur-lg opacity-10" style={{
                                            background: 'linear-gradient(to right, #ffd89b, #f5e6d3, #d4af7a)',
                                            WebkitBackgroundClip: 'text',
                                            WebkitTextFillColor: 'transparent',
                                            backgroundClip: 'text'
                                        }}>
                                            Thông tin chi tiết
                                        </span>
                                        <span className="relative" style={{
                                            color: '#ffd89b',
                                            textShadow: '0 0 10px rgba(255,216,155,0.2), 0 0 5px rgba(255,216,155,0.15)'
                                        }}>
                                            Thông tin chi tiết
                                        </span>
                                    </span>
                                </h3>
                                <span className="inline-block w-3 h-3 rounded-full bg-emerald-400 shadow-[0_0_12px_rgba(52,211,153,0.6)]" />
                            </div>

                            <div className="grid grid-cols-1 sm:grid-cols-2 gap-x-10">
                                <div className="space-y-8">
                                    <LabeledText label="Họ & Tên" value={form.fullName || "—"} />
                                    <LabeledText
                                        label="Giới tính"
                                        value={
                                            form.gender === "male"
                                                ? "Nam"
                                                : form.gender === "female"
                                                    ? "Nữ"
                                                    : form.gender === "other"
                                                        ? "Khác"
                                                        : "—"
                                        }
                                    />
                                    <LabeledText label="Ngày sinh" value={form.dob || "—"} />
                                    <LabeledText label="Tên tộc" value={form.clanName || "—"} />
                                </div>

                                <div className="space-y-8 mt-6 sm:mt-0">
                                    <LabeledText label="Email" value={me.email} />
                                    <LabeledText label="Số điện thoại" value={form.phone || "—"} />
                                    <LabeledText
                                        label="Địa chỉ"
                                        value={[form.address?.houseNumber, form.address?.ward, form.address?.city]
                                            .filter(Boolean)
                                            .join(", ") || "—"}
                                    />
                                    <div className="flex items-center gap-3">
                                        <span className="text-base text-white/80 font-medium">Trạng thái</span>
                                        <span className="inline-flex items-center gap-2 text-base px-4 h-9 rounded-full bg-emerald-900/40 text-emerald-300 border border-emerald-400/30 shadow-[0_0_20px_rgba(52,211,153,0.2)]">
                                            <span className="w-2.5 h-2.5 rounded-full bg-emerald-400 shadow-[0_0_8px_rgba(52,211,153,0.6)] animate-pulse" />
                                            Active
                                        </span>
                                    </div>
                                </div>
                            </div>
                        </section>

                        {/* Avatar card */}
                        <section className="rounded-3xl border border-[#ffd89b]/20 shadow-[0_10px_40px_rgba(0,0,0,0.4),0_0_40px_rgba(255,216,155,0.05)] p-10 bg-gradient-to-br from-white/5 via-white/3 to-white/3 backdrop-blur-xl flex flex-col items-center justify-center transition-all duration-500 hover:border-[#ffd89b]/30 hover:shadow-[0_15px_50px_rgba(0,0,0,0.5),0_0_60px_rgba(255,216,155,0.1)]">
                            <div className="text-center mb-8">
                                <h1 className="text-4xl md:text-5xl font-bold tracking-tight">
                                    <span className="relative inline-block">
                                        <span className="absolute inset-0 blur-lg opacity-10" style={{
                                            background: 'linear-gradient(to right, #ffd89b, #f5e6d3, #d4af7a)',
                                            WebkitBackgroundClip: 'text',
                                            WebkitTextFillColor: 'transparent',
                                            backgroundClip: 'text'
                                        }}>
                                            {form.fullName || "Fullname"}
                                        </span>
                                        <span className="relative" style={{
                                            color: '#ffd89b',
                                            textShadow: '0 0 10px rgba(255,216,155,0.2), 0 0 5px rgba(255,216,155,0.15)'
                                        }}>
                                            {form.fullName || "Fullname"}
                                        </span>
                                    </span>
                                </h1>
                                <div className="text-lg text-white/80 mt-2 font-medium">@{me.username}</div>
                            </div>

                            <div className="relative mt-4 group">
                                {/* Avatar glow effects - reduced */}
                                <div className="absolute -inset-8 bg-gradient-to-r from-[#d4af7a] via-[#ffd89b] to-[#d4af7a] rounded-full blur-[50px] opacity-25 group-hover:opacity-40 transition-all duration-1000 animate-pulse-slow" />
                                <div className="absolute -inset-12 bg-[#ffd89b]/20 rounded-full blur-[80px] opacity-0 group-hover:opacity-25 transition-all duration-1000" />

                                <div className="w-[260px] h-[260px] rounded-full overflow-hidden relative shadow-[0_15px_45px_rgba(0,0,0,0.5),0_0_30px_rgba(255,216,155,0.2)] border-4 border-[#ffd89b]/40 ring-4 ring-[#ffd89b]/15 group-hover:border-[#ffd89b]/60 group-hover:shadow-[0_20px_55px_rgba(0,0,0,0.6),0_0_45px_rgba(255,216,155,0.3)] transition-all duration-500">
                                    {form.avatarUrl ? (
                                        <img src={form.avatarUrl} alt="avatar" className="w-full h-full object-cover select-none" draggable={false} />
                                    ) : (
                                        <div className="w-full h-full bg-gradient-to-br from-[#2e3a57] to-[#0f1419] flex items-center justify-center font-semibold text-8xl">
                                            <span style={{
                                                color: '#ffd89b',
                                                textShadow: '0 0 25px rgba(255,216,155,0.5), 0 0 12px rgba(255,216,155,0.3)'
                                            }}>
                                                {(form.fullName || me.username || "U").charAt(0).toUpperCase()}
                                            </span>
                                        </div>
                                    )}
                                </div>

                                {!editing && (
                                    <button
                                        onClick={() => setEditing(true)}
                                        className="absolute left-1/2 -translate-x-1/2 -bottom-7 h-14 w-14 rounded-full bg-gradient-to-r from-[#d4af7a] via-[#ffd89b] to-[#d4af7a] text-[#0f1419] grid place-content-center shadow-[0_12px_35px_rgba(255,216,155,0.5),0_0_25px_rgba(255,216,155,0.3)] hover:shadow-[0_15px_45px_rgba(255,216,155,0.7),0_0_40px_rgba(255,216,155,0.5)] hover:scale-110 active:scale-105 transition-all duration-300 border-2 border-[#ffd89b]/25"
                                        title="Chỉnh sửa"
                                    >
                                        <Edit3 className="w-7 h-7" strokeWidth={2.5} />
                                    </button>
                                )}
                            </div>
                        </section>
                    </div>
                    {isLocal && <AccountSecuritySection me={me} onChanged={(u) => setMe(u)} />}
                </div>
            </div>

            {editing && (
                <ProfileEditModal
                    me={me}
                    form={form}
                    saving={saving}
                    uploadingAvatar={uploadingAvatar}
                    fileRef={fileRef}
                    provinceCode={provinceCode}
                    wardCode={wardCode}
                    provinceOptions={provinceOptions}
                    wardOptions={wardOptions}
                    onCancel={() => setEditing(false)}
                    onSave={onSave}
                    onFieldChange={onFieldChange}
                    onPickAvatar={onPickAvatar}
                    onAvatarSelected={onAvatarSelected}
                    onProvinceChange={onProvinceChange}
                    onWardChange={onWardChange}
                />
            )}

            {/* Animations */}
            <style>{`
                /* Prevent scrollbar jump - simple and effective solution */
                html, body {
                    overflow-x: hidden;
                    margin: 0;
                    padding: 0;
                }
                
                body {
                    min-height: 100vh;
                }
                
                /* Custom scrollbar for better aesthetics */
                ::-webkit-scrollbar {
                    width: 10px;
                }
                
                ::-webkit-scrollbar-track {
                    background: #1e2a3a;
                }
                
                ::-webkit-scrollbar-thumb {
                    background: #ffd89b;
                    border-radius: 5px;
                }
                
                ::-webkit-scrollbar-thumb:hover {
                    background: #d4af7a;
                }
                
                /* Firefox scrollbar */
                * {
                    scrollbar-width: thin;
                    scrollbar-color: #ffd89b #1e2a3a;
                }
                
                @keyframes fade-in {
                    from { opacity: 0; transform: translateY(20px); }
                    to { opacity: 1; transform: translateY(0); }
                }
                
                @keyframes pulse-slow {
                    0%, 100% { 
                        opacity: 0.3;
                        transform: scale(1);
                    }
                    50% { 
                        opacity: 0.5;
                        transform: scale(1.08);
                    }
                }
                
                .animate-fade-in {
                    animation: fade-in 1s ease-out;
                }
                
                .animate-pulse-slow {
                    animation: pulse-slow 5s ease-in-out infinite;
                }
                
                .bg-gradient-radial {
                    background: radial-gradient(circle, var(--tw-gradient-stops));
                }
            `}</style>
        </div>
    );
}