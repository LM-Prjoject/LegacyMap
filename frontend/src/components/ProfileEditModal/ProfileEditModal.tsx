import { ChangeEvent, RefObject } from "react";
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
    return (
        <div className="fixed inset-0 z-[120] flex items-center justify-center">
            {/* Overlay với gradient vàng đen */}
            <div
                className="absolute inset-0 bg-gradient-to-br from-[#000000]/70 via-[#1b2233]/80 to-[#2e3a57]/90 backdrop-blur-sm"
                onClick={() => !saving && onCancel()}
            />

            {/* Modal */}
            <div className="relative z-[121] w-full max-w-3xl h-[85vh] rounded-2xl bg-[#1b2233]/95 border border-[#D1B066]/30 shadow-2xl flex flex-col overflow-hidden animate-fade-in-up">
                {/* Header */}
                <div className="px-6 py-4 border-b border-[#D1B066]/30 flex items-center justify-between bg-[#2e3a57]/40 backdrop-blur-sm">
                    <h3 className="text-lg font-bold text-[#D1B066] tracking-wide">
                        ✏️ Chỉnh sửa thông tin
                    </h3>
                    <button
                        onClick={() => !saving && onCancel()}
                        className="p-2 rounded-lg hover:bg-[#D1B066]/10 transition"
                        title="Đóng"
                    >
                        <X className="w-5 h-5 text-[#D1B066]" />
                    </button>
                </div>

                {/* Body */}
                <div className="px-6 py-5 flex-1 overflow-y-auto custom-scrollbar">
                    {/* Avatar */}
                    <div className="flex items-center gap-6 mb-8">
                        <div className="relative">
                            <div
                                onClick={onPickAvatar}
                                className="w-[120px] h-[120px] rounded-full overflow-hidden border-4 border-[#D1B066]/50 ring-2 ring-[#D1B066]/30 shadow-lg cursor-pointer hover:scale-[1.03] transition-transform"
                                title="Đổi ảnh đại diện"
                            >
                                {form.avatarUrl ? (
                                    <img
                                        src={form.avatarUrl}
                                        alt="avatar"
                                        className="w-full h-full object-cover select-none"
                                        draggable={false}
                                    />
                                ) : (
                                    <div className="w-full h-full bg-gradient-to-br from-[#2e3a57] to-[#1b2233] flex items-center justify-center text-[#D1B066] text-5xl font-bold">
                                        {(form.fullName || me.username || "U").charAt(0).toUpperCase()}
                                    </div>
                                )}
                            </div>
                            {uploadingAvatar && (
                                <div className="absolute inset-0 grid place-items-center bg-black/50 text-white text-sm rounded-full">
                                    Đang tải...
                                </div>
                            )}
                            <input
                                ref={fileRef}
                                type="file"
                                accept="image/*"
                                className="hidden"
                                onChange={onAvatarSelected}
                            />
                        </div>

                        <div>
                            <div className="text-[#D1B066] font-semibold">Ảnh đại diện</div>
                            <div className="text-sm text-white/70">
                                Nhấn vào ảnh để tải lên ảnh mới.
                            </div>
                        </div>
                    </div>

                    {/* Fields */}
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-5 text-white">
                        <Field label="Họ và tên" icon={<UserIcon className="w-4 h-4 text-[#D1B066]/70" />}>
                            <input
                                name="fullName"
                                value={form.fullName || ""}
                                onChange={onFieldChange}
                                placeholder="VD: Nguyễn Văn A"
                                className="w-full bg-transparent text-white outline-none placeholder:text-white/40"
                            />
                        </Field>

                        <Field label="Số điện thoại" icon={<Phone className="w-4 h-4 text-[#D1B066]/70" />}>
                            <input
                                name="phone"
                                value={form.phone || ""}
                                onChange={onFieldChange}
                                placeholder="SĐT"
                                className="w-full bg-transparent text-white outline-none placeholder:text-white/40"
                            />
                        </Field>

                        <Field label="Giới tính" icon={<Users className="w-4 h-4 text-[#D1B066]/70" />}>
                            <select
                                name="gender"
                                value={form.gender || ""}
                                onChange={onFieldChange}
                                className="w-full bg-transparent text-white outline-none [&>option]:bg-[#2e3a57] [&>option]:text-white"
                            >
                                <option value="">Chọn giới tính</option>
                                <option value="male">Nam</option>
                                <option value="female">Nữ</option>
                                <option value="other">Khác</option>
                            </select>
                        </Field>

                        <Field label="Ngày sinh" icon={<Calendar className="w-4 h-4 text-[#D1B066]/70" />}>
                            <input
                                type="date"
                                name="dob"
                                value={form.dob || ""}
                                onChange={onFieldChange}
                                className="w-full bg-transparent text-white outline-none"
                            />
                        </Field>

                        <Field label="Tên tộc" icon={<Users className="w-4 h-4 text-[#D1B066]/70" />}>
                            <input
                                name="clanName"
                                value={form.clanName || ""}
                                onChange={onFieldChange}
                                placeholder="VD: Lê, Nguyễn..."
                                className="w-full bg-transparent text-white outline-none placeholder:text-white/40"
                            />
                        </Field>

                        <Field label="Email" icon={<Mail className="w-4 h-4 text-[#D1B066]/70" />}>
                            <input
                                value={me.email}
                                disabled
                                className="w-full bg-transparent text-white/80 outline-none"
                            />
                        </Field>

                        <Field label="Số nhà" icon={<Home className="w-4 h-4 text-[#D1B066]/70" />}>
                            <input
                                name="address.houseNumber"
                                value={form.address?.houseNumber || ""}
                                onChange={onFieldChange}
                                placeholder="VD: 123/4 ABC"
                                className="w-full bg-transparent text-white outline-none placeholder:text-white/40"
                            />
                        </Field>

                        <Field label="Tỉnh/Thành phố" icon={<MapPin className="w-4 h-4 text-[#D1B066]/70" />}>
                            <SearchCombo
                                bare
                                value={provinceCode}
                                onChange={(v) => onProvinceChange(String(v ?? ""))}
                                options={provinceOptions}
                                placeholder="Tỉnh/thành..."
                            />
                        </Field>

                        <Field label="Phường/Xã" icon={<MapPin className="w-4 h-4 text-[#D1B066]/70" />}>
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
                <div className="px-6 py-4 border-t border-[#D1B066]/30 flex items-center justify-end gap-3 bg-[#2e3a57]/40 backdrop-blur-sm">
                    <button
                        onClick={onCancel}
                        disabled={saving}
                        className="px-4 h-10 rounded-xl border border-[#D1B066]/40 text-[#D1B066] font-medium hover:bg-[#D1B066]/10 transition disabled:opacity-60"
                    >
                        Hủy
                    </button>
                    <button
                        onClick={onSave}
                        disabled={saving}
                        className="flex items-center gap-2 px-5 h-10 rounded-xl font-medium text-[#1b2233] bg-gradient-to-r from-[#EEDC9A] to-[#B69563] shadow-lg hover:brightness-110 transition disabled:opacity-60"
                    >
                        <Save className="w-5 h-5" />
                        {saving ? "Đang lưu..." : "Lưu"}
                    </button>
                </div>
            </div>
        </div>
    );
}
