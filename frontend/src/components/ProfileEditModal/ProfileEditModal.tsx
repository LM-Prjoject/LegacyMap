import { ChangeEvent, RefObject, useMemo } from "react";
import { X, Save, Mail, Phone, Users, Calendar, Home, MapPin, User as UserIcon } from "lucide-react";
import { Field } from "@/components/ui/Field";
import { SearchCombo, type Option } from "@/components/ui/SearchCombo";
import { type User, type Address } from "@/api/auth";

type Gender = "male" | "female" | "other" | "";

export type Form = {
    fullName?: string;
    clanName?: string;
    gender?: Gender;
    phone?: string;
    dob?: string;
    avatarUrl?: string;
    address: Address;
};

function getTodayLocalISO() {
    const d = new Date();
    const yyyy = d.getFullYear();
    const mm = String(d.getMonth() + 1).padStart(2, "0");
    const dd = String(d.getDate()).padStart(2, "0");
    return `${yyyy}-${mm}-${dd}`;
}

export default function ProfileEditModal({
                                             me,
                                             form,
                                             saving,
                                             uploadingAvatar,
                                             fileRef,
                                             provinceCode,
                                             wardCode,
                                             provinceOptions,
                                             wardOptions,
                                             onCancel,
                                             onSave,
                                             onFieldChange,
                                             onPickAvatar,
                                             onAvatarSelected,
                                             onProvinceChange,
                                             onWardChange,
                                         }: {
    me: User;
    form: Form;
    saving: boolean;
    uploadingAvatar: boolean;
    fileRef: RefObject<HTMLInputElement>;
    provinceCode: number | "";
    wardCode: number | "";
    provinceOptions: Option[];
    wardOptions: Option[];
    onCancel: () => void;
    onSave: () => void;
    onFieldChange: (e: ChangeEvent<HTMLInputElement | HTMLSelectElement>) => void;
    onPickAvatar: () => void;
    onAvatarSelected: (e: ChangeEvent<HTMLInputElement>) => void;
    onProvinceChange: (codeStr: string) => void;
    onWardChange: (codeStr: string) => void;
}) {
    const today = useMemo(() => getTodayLocalISO(), []);
    const invalidDob = !!(form.dob && form.dob > today);

    return (
        <div className="fixed inset-0 z-[120]">
            <div className="absolute inset-0 bg-black/50 backdrop-blur-[2px]" onClick={() => !saving && onCancel()} />

            <div className="absolute inset-0 z-[121] flex items-center justify-center p-4 sm:p-6">
                <div
                    role="dialog"
                    aria-modal="true"
                    className="w-full max-w-3xl h-[85svh] sm:h-[85vh] bg-white rounded-xl sm:rounded-2xl shadow-2xl border border-slate-200 overflow-hidden flex flex-col min-h-0 overscroll-contain"
                >
                    <div className="px-6 py-4 border-b border-slate-200 flex items-center justify-between sticky top-0 bg-white z-10">
                        <h3 className="text-lg font-semibold text-slate-900">Chỉnh sửa thông tin</h3>
                        <button onClick={() => !saving && onCancel()} className="p-2 rounded-lg hover:bg-slate-100" title="Đóng">
                            <X className="w-5 h-5 text-slate-600" />
                        </button>
                    </div>

                    <div className="px-6 py-5 overflow-y-auto flex-1 min-h-0">
                        <div className="flex items-center gap-4 mb-6">
                            <div className="relative">
                                <div
                                    onClick={onPickAvatar}
                                    className="w-[120px] h-[120px] rounded-full overflow-hidden border-4 border-white ring-2 ring-slate-200 shadow cursor-pointer hover:scale-[1.02] transition"
                                    title="Đổi ảnh đại diện"
                                >
                                    {form.avatarUrl ? (
                                        <img src={form.avatarUrl} alt="avatar" className="w-full h-full object-cover select-none" draggable={false} />
                                    ) : (
                                        <div className="w-full h-full bg-gradient-to-br from-blue-500 to-blue-700 flex items-center justify-center text-white text-4xl font-semibold">
                                            {(form.fullName || me.username || "U").charAt(0).toUpperCase()}
                                        </div>
                                    )}
                                </div>
                                {uploadingAvatar && (
                                    <div className="absolute inset-0 grid place-items-center rounded-full bg-black/40 text-white text-xs">
                                        Đang tải...
                                    </div>
                                )}
                                <input ref={fileRef} type="file" accept="image/*" className="hidden" onChange={onAvatarSelected} />
                            </div>

                            <div className="text-slate-600">
                                <div className="font-medium">Ảnh đại diện</div>
                                <div className="text-sm">Nhấn vào ảnh để tải lên ảnh mới.</div>
                            </div>
                        </div>

                        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                            <Field label="Họ và tên" icon={<UserIcon className="w-4 h-4 text-slate-400" />}>
                                <input
                                    name="fullName"
                                    value={form.fullName || ""}
                                    onChange={onFieldChange}
                                    placeholder="Ví dụ: Nguyễn Văn A"
                                    className="w-full bg-transparent outline-none placeholder:text-slate-400"
                                />
                            </Field>

                            <Field label="Số điện thoại" icon={<Phone className="w-4 h-4 text-slate-400" />}>
                                <input
                                    name="phone"
                                    value={form.phone || ""}
                                    onChange={onFieldChange}
                                    placeholder="SĐT"
                                    className="w-full bg-transparent outline-none placeholder:text-slate-400"
                                />
                            </Field>

                            <Field label="Giới tính" icon={<Users className="w-4 h-4 text-slate-400" />}>
                                <select name="gender" value={form.gender || ""} onChange={onFieldChange} className="w-full bg-transparent outline-none">
                                    <option value="">Chọn giới tính</option>
                                    <option value="male">Nam</option>
                                    <option value="female">Nữ</option>
                                    <option value="other">Khác</option>
                                </select>
                            </Field>

                            <Field label="Ngày sinh" icon={<Calendar className="w-4 h-4 text-slate-400" />}>
                                <input
                                    type="date"
                                    name="dob"
                                    value={form.dob || ""}
                                    max={today}
                                    onChange={(e) => {
                                        const v = e.target.value;
                                        if (v && v > today) {
                                            e.target.value = today;
                                        }
                                        onFieldChange(e);
                                    }}
                                    className="w-full bg-transparent outline-none"
                                    aria-invalid={invalidDob}
                                />
                            </Field>

                            <Field label="Tên tộc" icon={<Users className="w-4 h-4 text-slate-400" />}>
                                <input
                                    name="clanName"
                                    value={form.clanName || ""}
                                    onChange={onFieldChange}
                                    placeholder="Ví dụ: Lê, Nguyễn…"
                                    className="w-full bg-transparent outline-none placeholder:text-slate-400"
                                />
                            </Field>

                            <Field label="Email" icon={<Mail className="w-4 h-4 text-slate-400" />}>
                                <input value={me.email} disabled className="w-full bg-transparent outline-none text-slate-700" />
                            </Field>

                            <Field label="Số nhà" icon={<Home className="w-4 h-4 text-slate-400" />}>
                                <input
                                    name="address.houseNumber"
                                    value={form.address?.houseNumber || ""}
                                    onChange={onFieldChange}
                                    placeholder="VD: 123/4 abc"
                                    className="w-full bg-transparent outline-none placeholder:text-slate-400"
                                />
                            </Field>

                            {/* Province */}
                            <Field label="Tỉnh/Thành phố" icon={<MapPin className="w-4 h-4 text-slate-400" />}>
                                <SearchCombo
                                    bare
                                    value={provinceCode}
                                    onChange={(v) => onProvinceChange(String(v ?? ""))}
                                    options={provinceOptions}
                                    placeholder="Tỉnh/thành..."
                                />
                            </Field>

                            {/* Ward */}
                            <Field label="Phường/Xã" icon={<MapPin className="w-4 h-4 text-slate-400" />}>
                                <SearchCombo
                                    bare
                                    value={wardCode}
                                    onChange={(v) => onWardChange(String(v ?? ""))}
                                    options={wardOptions}
                                    placeholder={provinceCode ? "Phường/xã..." : ""}
                                    disabled={!provinceCode}
                                />
                            </Field>
                        </div>
                    </div>

                    {/* Footer */}
                    <div className="px-6 py-4 border-t border-slate-200 flex items-center justify-end gap-3 sticky bottom-0 bg-white">
                        <button onClick={onCancel} disabled={saving} className="px-4 h-10 rounded-xl border border-slate-300 text-slate-700 hover:bg-slate-50 disabled:opacity-60">
                            Hủy
                        </button>
                        <button
                            onClick={onSave}
                            disabled={saving || invalidDob}
                            className="flex items-center gap-2 px-5 h-10 rounded-xl bg-emerald-600 text-white font-medium shadow hover:bg-emerald-700 disabled:opacity-60"
                        >
                            <Save className="w-5 h-5" />
                            {saving ? "Đang lưu..." : "Lưu"}
                        </button>
                    </div>
                </div>
            </div>
        </div>
    );
}