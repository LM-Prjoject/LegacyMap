import { useEffect, useState, ChangeEvent, useMemo, useRef } from 'react';
import Navbar from '@/components/layout/Navbar';
import {Edit3, Save, Mail, Phone, User as UserIcon, Calendar, MapPin, Home, Users} from 'lucide-react';
import { authApi, type User, type UserProfile, type Address } from '@/api/auth';
import { showToast } from '@/lib/toast';
import { uploadAvatarToSupabase } from '@/lib/upload';
import PicChim from '@/assets/picchim.png';
import PicNoi from '@/assets/picnoi.png';

type Form = {
    fullName?: string;
    clanName?: string;
    gender?: 'male' | 'female' | 'other' | '';
    phone?: string;
    dob?: string;
    avatarUrl?: string;
    description?: string;
    address: Address;
};

const emptyAddr: Address = { houseNumber: '', ward: '', city: '' };

export default function ProfilePage() {
    const [me, setMe] = useState<User | null>(null);
    const [form, setForm] = useState<Form>({ address: { ...emptyAddr } });
    const [saving, setSaving] = useState(false);
    const [editing, setEditing] = useState(false);
    const [uploadingAvatar, setUploadingAvatar] = useState(false);
    const fileRef = useRef<HTMLInputElement | null>(null);

    useEffect(() => {
        (async () => {
            const u = await authApi.getMe();
            setMe(u);
            const a = (u.profile?.address || {}) as Address;
            const dob = (u.profile?.dob || '').split('T')[0] || '';
            setForm({
                fullName: u.profile?.fullName || '',
                clanName: u.profile?.clanName || '',
                gender: (u.profile?.gender as Form['gender']) || '',
                phone: u.profile?.phone || '',
                dob,
                avatarUrl: u.profile?.avatarUrl || '',
                description: u.profile?.description || '',
                address: {
                    houseNumber: a?.houseNumber || '',
                    ward: a?.ward || '',
                    city: a?.city || '',
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
        description: clean(f.description),
        address: {
            houseNumber: clean(f.address?.houseNumber),
            ward: clean(f.address?.ward),
            city: clean(f.address?.city),
        },
    });

    const buildPayloadFromProfile = (p?: UserProfile): UserProfile => {
        const a = (p?.address || {}) as Address;
        const dob = (p?.dob || '').split('T')[0] || '';
        return {
            fullName: clean(p?.fullName),
            clanName: clean(p?.clanName),
            gender: (p?.gender as Form['gender']) || undefined,
            phone: clean(p?.phone),
            dob: clean(dob),
            avatarUrl: clean(p?.avatarUrl),
            description: clean(p?.description),
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

    const onChange = (e: ChangeEvent<HTMLInputElement | HTMLSelectElement>) => {
        const { name, value } = e.target;
        if (!editing) return;
        if (name.startsWith('address.')) {
            const k = name.split('.')[1] as keyof Address;
            setForm((p) => ({ ...p, address: { ...p.address, [k]: value } }));
        } else {
            setForm((p) => ({ ...p, [name]: value }));
        }
    };

    const onPickAvatar = () => {
        if (!editing || saving || uploadingAvatar) return;
        fileRef.current?.click();
    };

    const onAvatarSelected = async (e: ChangeEvent<HTMLInputElement>) => {
        const file = e.target.files?.[0];
        if (!file) return;
        try {
            setUploadingAvatar(true);
            const url = await uploadAvatarToSupabase(file);
            setForm((p) => ({ ...p, avatarUrl: url }));
            showToast.success('Tải ảnh lên thành công');
        } catch (err: any) {
            showToast.error(err?.message || 'Không thể tải ảnh lên');
        } finally {
            setUploadingAvatar(false);
            e.target.value = '';
        }
    };

    const onSave = async () => {
        if (!me) return;
        if (!hasChanges) {
            showToast.warning('Không có thông tin nào thay đổi');
            setEditing(false);
            return;
        }
        setSaving(true);
        try {
            const payload = buildPayloadFromForm(form);
            await authApi.updateUser(me.id, payload);

            // 🔥 CẬP NHẬT: Update state `me`
            const updatedUser = {
                ...me,
                profile: { ...(me.profile || {}), ...payload },
            };
            setMe(updatedUser);

            // 🎯 QUAN TRỌNG: Cập nhật localStorage để Navbar thấy avatar mới
            localStorage.setItem('user', JSON.stringify(updatedUser));

            // 🔔 Trigger event để Navbar refresh
            window.dispatchEvent(new Event('storage'));

            showToast.success('Lưu thông tin thành công');
            setEditing(false);
        } catch (err: any) {
            showToast.error(err?.message || 'Lưu thông tin thất bại');
        } finally {
            setSaving(false);
        }
    };

    if (!me) {
        return (
            <div className="min-h-screen flex items-center justify-center text-slate-600">
                Đang tải hồ sơ...
            </div>
        );
    }

    return (
        <div className="min-h-screen bg-[#F6F0EF] overflow-x-hidden">
            <Navbar />

            <div className="hidden lg:block fixed inset-y-0 left-0 w-[720px] z-0 pointer-events-none">
                <div className="relative h-full">
                    <img
                        src={PicChim}
                        alt=""
                        className="absolute left-0 bottom-0 w-[1000px] h-[800px] max-w-none opacity-70 scale-[1.2] origin-left"
                        draggable={false}
                    />
                    <div className="absolute inset-y-0 left-0 w-[720px] overflow-hidden">
                        <div className="absolute -left-[0px] bottom-[30px] w-[560px] h-[560px] rounded-full bg-[#E6CDB9]" />
                    </div>
                    <img
                        src={PicNoi}
                        alt=""
                        className="absolute bottom-0 left-0 w-[517px] select-none"
                        draggable={false}
                    />
                </div>
            </div>

            <div className="w-full px-4 py-8 lg:pl-[760px] relative z-10">
                <div className="flex items-center justify-end gap-6 mb-2">
                    <div className="text-right">
                        <h1 className="text-[40px] leading-[44px] font-semibold text-black">
                            {form.fullName || 'Fullname'}
                        </h1>
                        <div className="text-[22px] text-black/70 -mt-1">@{me.username}</div>
                    </div>

                    <div className="relative">
                        <div
                            onClick={onPickAvatar}
                            role={editing ? 'button' : undefined}
                            tabIndex={editing ? 0 : -1}
                            onKeyDown={(e) => {
                                if (!editing) return;
                                if (e.key === 'Enter' || e.key === ' ') {
                                    e.preventDefault();
                                    onPickAvatar();
                                }
                            }}
                            title={editing ? 'Click để đổi ảnh' : undefined}
                            className={`w-[170px] h-[170px] rounded-full overflow-hidden relative shadow-xl border-4 border-white
                                ${editing ? 'cursor-pointer hover:scale-105 hover:shadow-2xl transition-all duration-300' : 'cursor-default'}`}
                        >
                            {form.avatarUrl ? (
                                <img
                                    src={form.avatarUrl}
                                    alt="avatar"
                                    className="w-full h-full object-cover select-none"
                                    draggable={false}
                                />
                            ) : (
                                <div className="w-full h-full bg-gradient-to-br from-blue-500 to-blue-700 flex items-center justify-center text-white font-semibold text-6xl">
                                    {(form.fullName || me.username || 'U').charAt(0).toUpperCase()}
                                </div>
                            )}

                            {uploadingAvatar && (
                                <div className="absolute inset-0 bg-black/50 text-white text-sm flex items-center justify-center backdrop-blur-sm">
                                    <div className="text-center">
                                        <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-white mx-auto mb-2"></div>
                                        <div>Đang tải...</div>
                                    </div>
                                </div>
                            )}
                        </div>

                        {!editing ? (
                            <button
                                onClick={() => setEditing(true)}
                                className="absolute left-1/2 -translate-x-1/2 -bottom-4 h-10 w-10 rounded-full bg-[#1E3A8A] text-white grid place-content-center shadow hover:bg-[#2745a1] transition"
                                title="Chỉnh sửa"
                            >
                                <Edit3 className="w-5 h-5" />
                            </button>
                        ) : (
                            <button
                                onClick={onSave}
                                disabled={saving}
                                className="absolute left-1/2 -translate-x-1/2 -bottom-4 h-10 w-10 rounded-full bg-emerald-600 text-white grid place-content-center shadow hover:bg-emerald-700 disabled:opacity-60 transition"
                                title="Lưu thay đổi"
                            >
                                <Save className="w-5 h-5" />
                            </button>
                        )}

                        <input
                            ref={fileRef}
                            type="file"
                            accept="image/*"
                            className="hidden"
                            onChange={onAvatarSelected}
                        />
                    </div>
                </div>

                <div className="h-6" />

                <div className="space-y-6 w-full max-w-[980px] ml-auto">

                    <section className="bg-white rounded-2xl border border-slate-200 shadow-sm">
                        <header className="px-5 py-4 border-b border-slate-200 flex items-center gap-2">
                            <div className="w-8 h-8 rounded-lg bg-[#1E3A8A]/10 grid place-content-center">
                                <Mail className="w-4 h-4 text-[#1E3A8A]" />
                            </div>
                            <h3 className="text-[15px] font-semibold text-slate-800">Liên hệ</h3>
                        </header>

                        <div className="p-5 grid grid-cols-1 md:grid-cols-2 gap-4">
                            <div className="relative">
                                <label className="text-xs font-medium text-slate-500 mb-1 block">Email</label>
                                <div className="flex items-center bg-slate-50 border border-slate-200 rounded-xl h-12 px-3">
                                    <Mail className="w-4 h-4 text-slate-400 mr-2" />
                                    <input
                                        disabled
                                        value={me.email}
                                        className="w-full bg-transparent outline-none text-slate-700"
                                    />
                                </div>
                            </div>

                            <div className="relative">
                                <label className="text-xs font-medium text-slate-500 mb-1 block">Số điện thoại</label>
                                <div className={`flex items-center rounded-xl h-12 px-3 border ${
                                    !editing || saving ? 'bg-slate-50 border-slate-200' : 'bg-white border-slate-300 focus-within:border-[#1E3A8A] focus-within:ring-2 focus-within:ring-[#1E3A8A]/20'
                                }`}>
                                    <Phone className="w-4 h-4 text-slate-400 mr-2" />
                                    <input
                                        name="phone"
                                        value={form.phone || ''}
                                        onChange={onChange}
                                        placeholder="SĐT"
                                        disabled={!editing || saving}
                                        className="w-full bg-transparent outline-none placeholder:text-slate-400"
                                    />
                                </div>
                            </div>
                        </div>
                    </section>

                    <section className="bg-white rounded-2xl border border-slate-200 shadow-sm">
                        <header className="px-5 py-4 border-b border-slate-200 flex items-center gap-2">
                            <div className="w-8 h-8 rounded-lg bg-[#1E3A8A]/10 grid place-content-center">
                                <UserIcon className="w-4 h-4 text-[#1E3A8A]" />
                            </div>
                            <h3 className="text-[15px] font-semibold text-slate-800">Thông tin cá nhân</h3>
                        </header>

                        <div className="p-5 space-y-4">
                            {/* Row 1: Họ và tên - Full width */}
                            <div>
                                <label className="text-xs font-medium text-slate-500 mb-1 block">Họ và tên</label>
                                <div className={`rounded-xl h-12 px-3 border ${
                                    !editing || saving ? 'bg-slate-50 border-slate-200' : 'bg-white border-slate-300 focus-within:border-[#1E3A8A] focus-within:ring-2 focus-within:ring-[#1E3A8A]/20'
                                } flex items-center`}>
                                    <UserIcon className="w-4 h-4 text-slate-400 mr-2" />
                                    <input
                                        name="fullName"
                                        value={form.fullName || ''}
                                        onChange={onChange}
                                        placeholder="Ví dụ: Thị Diệu Linh Lê"
                                        disabled={!editing || saving}
                                        className="w-full bg-transparent outline-none placeholder:text-slate-400"
                                    />
                                </div>
                            </div>

                            {/* Row 2: Giới tính, Ngày sinh, Tên Tộc - Grid 3 columns */}
                            <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                                <div>
                                    <label className="text-xs font-medium text-slate-500 mb-1 block">Giới tính</label>
                                    <div className={`rounded-xl h-12 px-3 border ${
                                        !editing || saving ? 'bg-slate-50 border-slate-200' : 'bg-white border-slate-300 focus-within:border-[#1E3A8A] focus-within:ring-2 focus-within:ring-[#1E3A8A]/20'
                                    } flex items-center`}>
                                        <Users className="w-4 h-4 text-slate-400 mr-2" />
                                        <select
                                            name="gender"
                                            value={form.gender || ''}
                                            onChange={onChange}
                                            disabled={!editing || saving}
                                            className="w-full bg-transparent outline-none"
                                        >
                                            <option value="">Chọn giới tính</option>
                                            <option value="male">Nam</option>
                                            <option value="female">Nữ</option>
                                            <option value="other">Khác</option>
                                        </select>
                                    </div>
                                </div>

                                <div>
                                    <label className="text-xs font-medium text-slate-500 mb-1 block">Ngày sinh</label>
                                    <div className={`rounded-xl h-12 px-3 border ${
                                        !editing || saving ? 'bg-slate-50 border-slate-200' : 'bg-white border-slate-300 focus-within:border-[#1E3A8A] focus-within:ring-2 focus-within:ring-[#1E3A8A]/20'
                                    } flex items-center`}>
                                        <Calendar className="w-4 h-4 text-slate-400 mr-2" />
                                        <input
                                            type="date"
                                            name="dob"
                                            value={form.dob || ''}
                                            onChange={onChange}
                                            disabled={!editing || saving}
                                            className="w-full bg-transparent outline-none"
                                        />
                                    </div>
                                </div>

                                <div>
                                    <label className="text-xs font-medium text-slate-500 mb-1 block">Tên Tộc</label>
                                    <div className={`rounded-xl h-12 px-3 border ${
                                        !editing || saving ? 'bg-slate-50 border-slate-200' : 'bg-white border-slate-300 focus-within:border-[#1E3A8A] focus-within:ring-2 focus-within:ring-[#1E3A8A]/20'
                                    } flex items-center`}>
                                        <Users className="w-4 h-4 text-slate-400 mr-2" />
                                        <input
                                            name="clanName"
                                            value={form.clanName || ''}
                                            onChange={onChange}
                                            placeholder="Ví dụ: Lê, Nguyễn…"
                                            disabled={!editing || saving}
                                            className="w-full bg-transparent outline-none placeholder:text-slate-400"
                                        />
                                    </div>
                                </div>
                            </div>
                        </div>
                    </section>

                    <section className="bg-white rounded-2xl border border-slate-200 shadow-sm">
                        <header className="px-5 py-4 border-b border-slate-200 flex items-center gap-2">
                            <div className="w-8 h-8 rounded-lg bg-[#1E3A8A]/10 grid place-content-center">
                                <MapPin className="w-4 h-4 text-[#1E3A8A]" />
                            </div>
                            <h3 className="text-[15px] font-semibold text-slate-800">Địa chỉ</h3>
                        </header>

                        <div className="p-5">
                            <div className="grid grid-cols-1 md:grid-cols-3 gap-4">

                                <div>
                                    <label className="text-xs font-medium text-slate-500 mb-1 block">Số nhà</label>
                                    <div className={`rounded-xl h-12 px-3 border ${
                                        !editing || saving ? 'bg-slate-50 border-slate-200' : 'bg-white border-slate-300 focus-within:border-[#1E3A8A] focus-within:ring-2 focus-within:ring-[#1E3A8A]/20'
                                    } flex items-center`}>
                                        <Home className="w-4 h-4 text-slate-400 mr-2" />
                                        <input
                                            name="address.houseNumber"
                                            value={form.address.houseNumber || ''}
                                            onChange={onChange}
                                            placeholder="VD: 123/4"
                                            disabled={!editing || saving}
                                            className="w-full bg-transparent outline-none placeholder:text-slate-400"
                                        />
                                    </div>
                                </div>

                                <div>
                                    <label className="text-xs font-medium text-slate-500 mb-1 block">Phường/Xã</label>
                                    <div className={`rounded-xl h-12 px-3 border ${
                                        !editing || saving ? 'bg-slate-50 border-slate-200' : 'bg-white border-slate-300 focus-within:border-[#1E3A8A] focus-within:ring-2 focus-within:ring-[#1E3A8A]/20'
                                    } flex items-center`}>
                                        <MapPin className="w-4 h-4 text-slate-400 mr-2" />
                                        <input
                                            name="address.ward"
                                            value={form.address.ward || ''}
                                            onChange={onChange}
                                            placeholder="VD: Phường 7"
                                            disabled={!editing || saving}
                                            className="w-full bg-transparent outline-none placeholder:text-slate-400"
                                        />
                                    </div>
                                </div>
                                <div>
                                    <label className="text-xs font-medium text-slate-500 mb-1 block">Thành phố</label>
                                    <div className={`rounded-xl h-12 px-3 border ${
                                        !editing || saving ? 'bg-slate-50 border-slate-200' : 'bg-white border-slate-300 focus-within:border-[#1E3A8A] focus-within:ring-2 focus-within:ring-[#1E3A8A]/20'
                                    } flex items-center`}>
                                        <MapPin className="w-4 h-4 text-slate-400 mr-2" />
                                        <input
                                            name="address.city"
                                            value={form.address.city || ''}
                                            onChange={onChange}
                                            placeholder="VD: TP. Hồ Chí Minh"
                                            disabled={!editing || saving}
                                            className="w-full bg-transparent outline-none placeholder:text-slate-400"
                                        />
                                    </div>
                                </div>
                            </div>
                        </div>
                    </section>

                    <section className="bg-white rounded-2xl border border-slate-200 shadow-sm">
                        <header className="px-5 py-4 border-b border-slate-200 flex items-center gap-2">
                            <div className="w-8 h-8 rounded-lg bg-[#1E3A8A]/10 grid place-content-center">
                                <UserIcon className="w-4 h-4 text-[#1E3A8A]" />
                            </div>
                            <h3 className="text-[15px] font-semibold text-slate-800">Mô tả bản thân</h3>
                        </header>

                        <div className="p-5">
                            <textarea
                                name="description"
                                value={form.description || ''}
                                onChange={(e) => {
                                    if (!editing) return;
                                    setForm((prev) => ({ ...prev, description: e.target.value }));
                                }}
                                placeholder="Giới thiệu ngắn gọn về bạn (sở thích, vai trò trong gia phả, v.v.)"
                                disabled={!editing || saving}
                                className={`w-full rounded-xl border p-3 min-h-[120px] resize-none ${
                                    !editing || saving
                                        ? 'bg-slate-50 border-slate-200'
                                        : 'bg-white border-slate-300 focus:border-[#1E3A8A] focus:ring-2 focus:ring-[#1E3A8A]/20'
                                }`}
                            />
                        </div>
                    </section>
                </div>
            </div>
        </div>
    );
}