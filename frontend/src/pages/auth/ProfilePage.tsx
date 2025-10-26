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
            <div className="text-[15px] font-semibold text-slate-700 tracking-wide mb-1">{label}</div>
            <div className="text-lg font-medium text-slate-900 leading-snug break-words">{value}</div>
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
            <div className="min-h-screen flex items-center justify-center text-slate-600">
                Đang tải hồ sơ...
            </div>
        );
    }

    const isLocal = me?.provider?.toLowerCase?.() === "local";
    return (
        <div className={`min-h-screen bg-[#F6F0EF] overflow-x-hidden relative ${editing ? "overflow-hidden" : ""}`}>
            <Navbar />
            <div className="pointer-events-none fixed inset-0 z-0 overflow-hidden">
                <div className="relative w-full h-full">
                    <img
                        src={PicChim}
                        alt=""
                        className="absolute bottom-0 left-1/2 -translate-x-1/2 w-[1400px] lg:w-[1600px] h-auto max-w-none opacity-25 scale-[1.05]"
                        draggable={false}
                    />
                    <div
                        className="absolute right-[60%] rounded-full bg-[#BB8357]/50 blur-3xl"
                        style={{ bottom: "10vh", width: "55vw", maxWidth: "820px", aspectRatio: "1 / 1" }}
                    />
                    <img
                        src={PicNoi}
                        alt=""
                        className="absolute bottom-0 left-0 w-[520px] sm:w-[600px] lg:w-[680px] opacity-95 drop-shadow-2xl"
                        draggable={false}
                    />
                </div>
            </div>
            <div className="relative z-10 w-full px-8 py-16">
                <div className="mx-auto w-full max-w-[1200px]">
                    <div className="grid grid-cols-1 lg:grid-cols-2 gap-12">
                        {/* Bio card */}
                        <section className="rounded-3xl border border-white/30 shadow-2xl p-12 bg-white/25 backdrop-blur-md transition-all duration-300">
                            <div className="flex items-center justify-between mb-8">
                                <h3 className="text-3xl font-bold text-slate-900 tracking-tight">Thông tin cá nhân</h3>
                                <span className="inline-block w-3 h-3 rounded-full bg-emerald-500" />
                            </div>

                            <div className="grid grid-cols-1 sm:grid-cols-2 gap-x-12">
                                <div className="space-y-8">
                                    <LabeledText label="Họ & Tên" value={form.fullName || "—"} />
                                    <LabeledText
                                        label="Giới tính"
                                        value={
                                            form.gender === "male" ? "Nam" : form.gender === "female" ? "Nữ" : form.gender === "other" ? "Khác" : "—"
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
                                        value={[form.address?.houseNumber, form.address?.ward, form.address?.city].filter(Boolean).join(", ") || "—"}
                                    />
                                    <div className="flex items-center gap-3">
                                        <span className="text-base text-slate-500 font-medium">Trạng thái</span>
                                        <span className="inline-flex items-center gap-2 text-base px-4 h-9 rounded-full bg-emerald-50 text-emerald-700 border border-emerald-200">
                      <span className="w-2.5 h-2.5 rounded-full bg-emerald-500" />
                      Active
                    </span>
                                    </div>
                                </div>
                            </div>
                        </section>

                        {/* Avatar card */}
                        <section className="rounded-3xl border border-white/30 shadow-2xl p-12 bg-white/25 backdrop-blur-md flex flex-col items-center justify-center transition-all duration-300">
                            <div className="text-center mb-8">
                                <h1 className="text-4xl md:text-5xl font-bold text-slate-900 tracking-tight">{form.fullName || "Fullname"}</h1>
                                <div className="text-lg text-slate-500 mt-2">@{me.username}</div>
                            </div>

                            <div className="relative mt-4">
                                <div className="w-[260px] h-[260px] rounded-full overflow-hidden relative shadow-2xl border-4 border-white/50 ring-2 ring-slate-200/40">
                                    {form.avatarUrl ? (
                                        <img src={form.avatarUrl} alt="avatar" className="w-full h-full object-cover select-none" draggable={false} />
                                    ) : (
                                        <div className="w-full h-full bg-gradient-to-br from-blue-500 to-blue-700 flex items-center justify-center text-white font-semibold text-8xl">
                                            {(form.fullName || me.username || "U").charAt(0).toUpperCase()}
                                        </div>
                                    )}
                                </div>

                                {!editing && (
                                    <button
                                        onClick={() => setEditing(true)}
                                        className="absolute left-1/2 -translate-x-1/2 -bottom-7 h-14 w-14 rounded-full bg-[#1E3A8A] text-white grid place-content-center shadow-lg hover:bg-[#2745a1] transition"
                                        title="Chỉnh sửa"
                                    >
                                        <Edit3 className="w-7 h-7" />
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
        </div>
    );
}
