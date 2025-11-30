import React, { useState, useEffect, useMemo } from 'react';
import {X, Edit2, Trash2, Calendar, MapPin, Bell, FileText, Save, List} from 'lucide-react';
import lunisolar from 'lunisolar';
import { eventsApi } from '@/api/eventApi';
import { Event, EventType, CalendarType, RecurrenceRule } from '@/types/event';
import { getVietnameseLunarDay, getVietnameseLunarMonth, getVietnameseStemBranch } from '@/utils/lunarUtils';
import { EventDetailModalProps } from "@/types/event";
import { showToast } from '@/lib/toast';

interface Errors {
    title?: string;
    startDate?: string;
}

const EventDetailModal: React.FC<EventDetailModalProps> = ({ eventId, isOpen, onClose, onDelete, onUpdate, selectedDate }) => {
    const [event, setEvent] = useState<Event | null>(null);
    const [isEditing, setIsEditing] = useState(false);
    const [editedEvent, setEditedEvent] = useState<Partial<Event & { reminder?: Event['reminder'] }>>({});
    const [showDeleteModal, setShowDeleteModal] = useState(false);
    const [loading, setLoading] = useState(true);
    const [errors, setErrors] = useState<Errors>({});

    useEffect(() => {
        if (isOpen) {
            document.body.style.overflow = 'hidden';
        } else {
            document.body.style.overflow = 'unset';
        }
        return () => {
            document.body.style.overflow = 'unset';
        };
    }, [isOpen]);

    useEffect(() => {
        if (isOpen && eventId) {
            fetchEvent();
        } else {
            setIsEditing(false);
            setErrors({});
        }
    }, [isOpen, eventId]);

    const fetchEvent = async () => {
        setLoading(true);
        try {
            const data = await eventsApi.getEvent(eventId);
            setEvent(data);

            const formatted = {
                ...data,
                startDate: data.startDate ? isoToLocalInput(data.startDate, data.isFullDay) : '',
                endDate: data.endDate ? isoToLocalInput(data.endDate, data.isFullDay) : '',
                reminder: data.reminder || { daysBefore: 3, methods: ['notification'] }
            };
            setEditedEvent(formatted);
        } catch (err) {
            showToast.error('Không tải được sự kiện');
        } finally {
            setLoading(false);
        }
    };

    const validate = () => {
        const err: Errors = {};
        if (!editedEvent.title?.trim()) err.title = 'Vui lòng nhập tiêu đề sự kiện';
        if (!editedEvent.startDate) err.startDate = 'Vui lòng chọn ngày bắt đầu';
        setErrors(err);
        return Object.keys(err).length === 0;
    };

    const handleSave = async () => {
        if (!validate()) return;

        try {
            const payload = {
                ...editedEvent,
                startDate: localInputToIso(editedEvent.startDate!, editedEvent.isFullDay),
                endDate: editedEvent.endDate ? localInputToIso(editedEvent.endDate, editedEvent.isFullDay) : undefined,
                isRecurring: !!editedEvent.recurrenceRule && editedEvent.recurrenceRule !== RecurrenceRule.NONE,
            };

            const updated = await eventsApi.updateEvent(eventId, payload);
            const fullEvent = { ...event!, ...updated } as Event;

            setEvent(fullEvent);
            setEditedEvent(fullEvent);
            onUpdate?.(fullEvent);

            setIsEditing(false);
            setErrors({});
            showToast.success('Cập nhật thành công!');
        } catch {
            showToast.error('Cập nhật thất bại');
        }
    };

    const handleDelete = async () => {
        try {
            await eventsApi.deleteEvent(eventId);
            showToast.success('Đã xóa sự kiện!');
            onDelete?.();
            setShowDeleteModal(false);
            onClose();
        } catch {
            showToast.error('Xóa thất bại');
        }
    };

    const isoToLocalInput = (iso: string, isFullDay = false): string => {
        if (!iso) return '';
        const date = new Date(iso);

        const vnOffset = 7 * 60;
        const localDate = new Date(date.getTime() + vnOffset * 60 * 1000);

        if (isFullDay) {
            return localDate.toISOString().split('T')[0];
        }
        return localDate.toISOString().slice(0, 16);
    };

    const localInputToIso = (local: string, isFullDay = false): string => {
        if (!local) return '';

        let date: Date;
        if (isFullDay) {
            date = new Date(local + 'T00:00:00.000+07:00');
        } else {
            date = new Date(local + (local.includes('T') ? '' : 'T12:00') + ':00.000+07:00');
        }

        return date.toISOString();
    };

    const formatDisplayDateTime = (dateStr: string) => {
        if (!dateStr) return '';
        return new Date(dateStr).toLocaleString('vi-VN', {
            year: 'numeric', month: 'long', day: 'numeric',
            hour: '2-digit', minute: '2-digit', hour12: false
        });
    };

    const getLunarDate = (solar: string) => {
        try {
            const lsr = lunisolar(new Date(solar));
            const l = lsr.lunar;
            const day = getVietnameseLunarDay(l.day);
            const month = getVietnameseLunarMonth(l.month, l.isLeapMonth);
            const year = getVietnameseStemBranch(lsr.format('cY'));
            return `${day} tháng ${month} năm ${year}`;
        } catch {
            return 'Không thể chuyển đổi';
        }
    };

    const displayDate = useMemo(() => {
        if (selectedDate && event) {
            const orig = new Date(event.startDate);
            const d = new Date(selectedDate);
            d.setHours(orig.getHours(), orig.getMinutes());
            return d.toISOString();
        }
        return event?.startDate;
    }, [selectedDate, event]);

    if (!isOpen) return null;

    return (
        <div className="fixed inset-0 z-[9999] flex items-center justify-center p-4">
            <div className="absolute inset-0" style={{
                backdropFilter: 'blur(8px)',
                WebkitBackdropFilter: 'blur(8px)',
                backgroundColor: 'rgba(0, 0, 0, 0.3)'
            }}
                 onClick={() => !loading && onClose()}
            />

            {/* Modal Content */}
            <div className="relative z-[100002] w-full max-w-2xl max-h-[80vh] rounded-2xl bg-gradient-to-br from-[#1b2233] to-[#2e3a57] border-2 border-[#D1B066]/40 shadow-[0_0_50px_rgba(209,176,102,0.3)] flex flex-col overflow-hidden">
                {/* Decorative corner accents */}
                <div className="absolute top-0 left-0 w-32 h-32 bg-gradient-to-br from-[#D1B066]/20 to-transparent rounded-br-[100px] pointer-events-none" />
                <div className="absolute bottom-0 right-0 w-32 h-32 bg-gradient-to-tl from-[#D1B066]/20 to-transparent rounded-tl-[100px] pointer-events-none" />

                {/* Header */}
                <div className="px-6 py-4 border-b border-[#D1B066]/30 flex items-center justify-between bg-[#2e3a57]/40 backdrop-blur-sm">
                    <h3 className="text-2xl font-bold bg-gradient-to-r from-[#EEDC9A] to-[#D1B066] bg-clip-text text-transparent">
                        {isEditing ? 'Chỉnh sửa sự kiện' : 'Chi tiết sự kiện'}
                    </h3>
                    <div className="flex items-center gap-2">
                        <button
                            onClick={() => !loading && onClose()}
                            className="p-2 rounded-lg hover:bg-[#D1B066]/10 transition"
                            title="Đóng"
                        >
                            <X className="w-5 h-5 text-[#D1B066]" />
                        </button>
                    </div>
                </div>

                <div className="relative px-8 py-6 flex-1 overflow-y-auto">
                    {loading ? (
                        <div className="flex flex-col items-center justify-center py-20">
                            <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#D1B066] mb-4"></div>
                            <p className="text-[#D1B066] font-medium">Đang tải...</p>
                        </div>
                    ) : !event ? (
                        <div className="text-center py-20">
                            <p className="text-red-400 font-semibold text-lg">Không tìm thấy sự kiện</p>
                        </div>
                    ) : (
                        <div className="space-y-5">
                            {/* Tiêu đề */}
                            {isEditing ? (
                                <div className="flex gap-4">
                                    <Edit2 className="w-5 h-5 text-[#EEDC9A] mt-1" />
                                    <div className="flex-1">
                                        <div className="font-bold text-m text-[#EEDC9A] uppercase tracking-wider mb-3">Tiêu đề <span className="text-red-400">*</span></div>
                                        <input
                                            type="text"
                                            value={editedEvent.title || ''}
                                            onChange={e => setEditedEvent(prev => ({ ...prev, title: e.target.value }))}
                                            className="w-full px-3 py-2 rounded-lg bg-[#0d1321]/60 border border-[#D1B066]/20 focus:border-[#D1B066] text-white text-sm placeholder-white/40 outline-none transition-all"
                                            placeholder="Nhập tiêu đề..."
                                        />
                                        {errors.title && <p className="text-red-400 text-sm mt-1">{errors.title}</p>}
                                    </div>
                                </div>
                            ) : (
                                <h3 className="text-3xl font-bold bg-gradient-to-r from-[#EEDC9A] to-[#D1B066] bg-clip-text text-transparent">
                                    {event.title}
                                </h3>
                            )}

                            {/* Time */}
                            <div className="space-y-6">
                                <div className="flex gap-4">
                                    <Calendar className="w-5 h-5 text-[#EEDC9A] mt-1" />
                                    <div className="flex-1">
                                        <div className="font-bold text-m text-[#EEDC9A] uppercase tracking-wider mb-3">Ngày & Giờ</div>

                                        {isEditing ? (
                                            <div className="space-y-5">
                                                <div className="grid grid-cols-2 gap-3">
                                                    {/* Thời gian bắt đầu */}
                                                    <div>
                                                        <label className="block text-sm font-medium text-[#EEDC9A] mb-1">
                                                            Thời gian bắt đầu <span className="text-red-400">*</span>
                                                        </label>
                                                        <input
                                                            type={editedEvent.isFullDay ? 'date' : 'datetime-local'}
                                                            value={editedEvent.startDate || ''}
                                                            onChange={e => setEditedEvent(prev => ({ ...prev, startDate: e.target.value }))}
                                                            min={new Date().toISOString().slice(0, 16)}
                                                            className="w-full px-3 py-2 rounded-lg bg-[#0d1321]/60 border border-[#D1B066]/20 focus:border-[#D1B066] text-white text-sm placeholder-white/40 outline-none transition-all"
                                                        />
                                                        {errors.startDate && <p className="text-red-400 text-sm mt-1">{errors.startDate}</p>}
                                                    </div>

                                                    {/* Thời gian kết thúc */}
                                                    <div>
                                                        <label className="block text-sm font-medium text-[#EEDC9A] mb-1">Thời gian kết thúc (tùy chọn)</label>
                                                        <input
                                                            type={editedEvent.isFullDay ? 'date' : 'datetime-local'}
                                                            value={editedEvent.endDate || ''}
                                                            min={editedEvent.startDate || new Date().toISOString().slice(0, 16)}
                                                            onChange={e => setEditedEvent(prev => ({ ...prev, endDate: e.target.value }))}
                                                            className="w-full px-3 py-2 rounded-lg bg-[#0d1321]/60 border border-[#D1B066]/20 focus:border-[#D1B066] text-white text-sm placeholder-white/40 outline-none transition-all"
                                                        />
                                                    </div>
                                                </div>

                                                {/* Loại lịch */}
                                                <div className="flex items-center gap-6">
                                                    <span className="text-sm text-white/80">Lịch:</span>
                                                    <div className="flex gap-3">
                                                        <button
                                                            type="button"
                                                            onClick={() => setEditedEvent(prev => ({ ...prev, calendarType: CalendarType.SOLAR }))}
                                                            className={`px-4 py-2 rounded-lg text-xs font-medium transition ${editedEvent.calendarType === CalendarType.SOLAR ? 'bg-[#D1B066] text-[#1b2233]' : 'bg-[#2e3a57] text-white'}`}
                                                        >
                                                            Dương lịch
                                                        </button>
                                                        <button
                                                            type="button"
                                                            onClick={() => setEditedEvent(prev => ({ ...prev, calendarType: CalendarType.LUNAR }))}
                                                            className={`px-4 py-2 rounded-lg text-xs font-medium transition ${editedEvent.calendarType === CalendarType.LUNAR ? 'bg-[#D1B066] text-[#1b2233]' : 'bg-[#2e3a57] text-white'}`}
                                                        >
                                                            Âm lịch
                                                        </button>
                                                    </div>
                                                </div>

                                                <div className="space-y-3">
                                                    {/* Cả ngày */}
                                                    <label className="flex items-center gap-2 cursor-pointer group">
                                                        <input
                                                            type="checkbox"
                                                            checked={editedEvent.isFullDay || false}
                                                            onChange={e => {
                                                                const checked = e.target.checked;
                                                                const datePart = editedEvent.startDate?.split('T')[0] || '';
                                                                setEditedEvent(prev => ({
                                                                    ...prev,
                                                                    isFullDay: checked,
                                                                    startDate: checked ? datePart : `${datePart}T${new Date().toTimeString().slice(0,5)}`,
                                                                    endDate: checked ? datePart : prev.endDate
                                                                }));
                                                            }}
                                                            className="w-4 h-4"
                                                            style={{ accentColor: 'rgb(255, 216, 155)' }}
                                                        />
                                                        <span className="text-white/90 group-hover:text-white transition-colors">Sự kiện cả ngày</span>
                                                    </label>

                                                    {/* Lặp lại */}
                                                    <label className="flex items-center gap-2 cursor-pointer group">
                                                        <input
                                                            type="checkbox"
                                                            checked={editedEvent.isRecurring || false}
                                                            onChange={e => setEditedEvent(prev => ({
                                                                ...prev,
                                                                isRecurring: e.target.checked,
                                                                recurrenceRule: e.target.checked && !prev.recurrenceRule ? RecurrenceRule.YEARLY : prev.recurrenceRule
                                                            }))}
                                                            className="w-4 h-4"
                                                            style={{ accentColor: 'rgb(255, 216, 155)' }}
                                                        />
                                                        <span className="text-white/90 group-hover:text-white transition-colors">Sự kiện lặp lại</span>
                                                    </label>

                                                    {editedEvent.isRecurring && (
                                                        <div className="ml-8 space-y-2">
                                                            {Object.values(RecurrenceRule).filter(r => r !== RecurrenceRule.NONE).map(rule => (
                                                                <label key={rule} className="flex items-center gap-2 cursor-pointer">
                                                                    <input
                                                                        type="radio"
                                                                        name="recurrence"
                                                                        checked={editedEvent.recurrenceRule === rule}
                                                                        onChange={() => setEditedEvent(prev => ({ ...prev, recurrenceRule: rule }))}
                                                                        className="w-4 h-4"
                                                                        style={{ accentColor: 'rgb(255, 216, 155)' }}
                                                                    />
                                                                    <span className="text-white text-sm">
                                                                    {rule === RecurrenceRule.YEARLY ? 'Hàng năm' : 'Hàng tháng'}
                                                                </span>
                                                                </label>
                                                            ))}
                                                        </div>
                                                    )}
                                                </div>
                                            </div>
                                        ) : (
                                            <>
                                                <p className="font-semibold text-white text-lg">
                                                    {displayDate ? formatDisplayDateTime(displayDate) : 'Đang tải...'}
                                                </p>
                                                {event.endDate && (
                                                    <p className="text-sm text-white/70 mt-1">
                                                        đến {formatDisplayDateTime(event.endDate)}
                                                    </p>
                                                )}
                                                <div className="flex flex-wrap gap-2 mt-3">
                                                    <span className="px-4 py-2 rounded-xl text-xs font-bold bg-gradient-to-r from-[#EEDC9A] to-[#D1B066] text-[#1b2233] shadow-lg">
                                                        {event.calendarType === CalendarType.LUNAR ? 'Âm lịch' : 'Dương lịch'}
                                                    </span>
                                                    {event.isFullDay && (
                                                        <span className="px-4 py-2 rounded-xl text-xs font-bold bg-gradient-to-r from-emerald-500 to-emerald-600 text-white shadow-lg">
                                                            Cả ngày
                                                        </span>
                                                    )}
                                                </div>
                                                {event.calendarType === CalendarType.LUNAR && displayDate && (
                                                    <p className="text-sm text-[#EEDC9A] mt-3">
                                                        Âm lịch: {getLunarDate(displayDate)}
                                                    </p>
                                                )}
                                            </>
                                        )}
                                    </div>
                                </div>

                                {/* Thông báo nhắc nhở*/}
                                <div className="flex gap-4">
                                    <Bell className="w-5 h-5 text-[#EEDC9A] mt-1" />
                                    <div className="flex-1">
                                        <div className="font-bold text-m text-[#EEDC9A] uppercase tracking-wider mb-3">Thông báo nhắc nhở</div>
                                        {isEditing ? (
                                            <div className="grid grid-cols-2 gap-4">
                                                <div>
                                                    <label className="text-sm text-white/80">Thời gian nhắc</label>
                                                    <select
                                                        value={editedEvent.reminder?.daysBefore ?? 3}
                                                        onChange={e => setEditedEvent(prev => ({
                                                            ...prev,
                                                            reminder: {
                                                                ...(prev.reminder || { daysBefore: 3, methods: ['notification'] }),
                                                                daysBefore: Number(e.target.value)
                                                            }
                                                        }))}
                                                        className="w-full mt-1 px-3 py-2 rounded-lg bg-[#0d1321]/60 border border-[#D1B066]/20 focus:border-[#D1B066] text-white text-sm outline-none cursor-pointer"
                                                    >
                                                    <option value={0}>Đúng giờ</option>
                                                    <option value={1}>1 ngày trước</option>
                                                    <option value={3}>3 ngày trước</option>
                                                    <option value={7}>1 tuần trước</option>
                                                    </select>
                                                </div>
                                                <div>
                                                    <label className="text-sm text-white/80">Phương thức</label>
                                                    <select
                                                        value={editedEvent.reminder?.methods?.[0] || 'notification'}
                                                        onChange={e => setEditedEvent(prev => ({
                                                            ...prev,
                                                            reminder: {
                                                                ...(prev.reminder || { daysBefore: 3, methods: ['notification'] }),
                                                                methods: [e.target.value as any]
                                                            }
                                                        }))}
                                                        className="w-full mt-1 px-3 py-2 rounded-lg bg-[#0d1321]/60 border border-[#D1B066]/20 focus:border-[#D1B066] text-white text-sm outline-none cursor-pointer"
                                                    >
                                                        <option value="notification">Thông báo</option>
                                                        <option value="email">Email</option>
                                                        <option value="both">Cả hai</option>
                                                    </select>
                                                </div>
                                            </div>
                                        ) : (
                                            <p className="text-white">
                                                {event.reminder ? `${event.reminder.daysBefore} ngày trước ` : 'Chưa cài đặt'}
                                            </p>
                                        )}
                                    </div>
                                </div>

                                {/* Loại sự kiện */}
                                <div className="flex gap-4">
                                    <List className="w-5 h-5 text-[#EEDC9A] mt-1" />
                                    <div>
                                        <div className="font-bold text-m text-[#EEDC9A] uppercase tracking-wider mb-3">Loại sự kiện</div>
                                        {isEditing ? (
                                            <select
                                                value={editedEvent.eventType || EventType.OTHER}
                                                onChange={e => setEditedEvent(prev => ({ ...prev, eventType: e.target.value as EventType }))}
                                                className="w-full px-3 py-2 rounded-lg bg-[#0d1321]/60 border border-[#D1B066]/20 focus:border-[#D1B066] text-white text-sm outline-none transition-all cursor-pointer"
                                            >
                                                {Object.values(EventType).map(t => (
                                                    <option key={t} value={t}>
                                                        {t === EventType.BIRTHDAY && 'Sinh nhật'}
                                                        {t === EventType.DEATH_ANNIVERSARY && 'Ngày giỗ'}
                                                        {t === EventType.WEDDING_ANNIVERSARY && 'Ngày cưới'}
                                                        {t === EventType.WEDDING && 'Đám cưới'}
                                                        {t === EventType.FUNERAL && 'Tang lễ'}
                                                        {t === EventType.FAMILY_REUNION && 'Họp mặt gia đình'}
                                                        {t === EventType.CEREMONY && 'Nghi lễ'}
                                                        {t === EventType.OTHER && 'Khác'}
                                                    </option>
                                                ))}
                                            </select>
                                        ) : (
                                            <p className="font-semibold text-white text-m">
                                                {event.eventType === EventType.BIRTHDAY ? 'Sinh nhật' :
                                                    event.eventType === EventType.DEATH_ANNIVERSARY ? 'Ngày giỗ' :
                                                        event.eventType === EventType.WEDDING_ANNIVERSARY ? 'Ngày cưới' :
                                                            event.eventType === EventType.WEDDING ? 'Đám cưới' :
                                                                event.eventType === EventType.FUNERAL ? 'Tang lễ' :
                                                                    event.eventType === EventType.FAMILY_REUNION ? 'Họp mặt gia đình' :
                                                                        event.eventType === EventType.CEREMONY ? 'Nghi lễ' : 'Khác'}
                                            </p>
                                        )}
                                    </div>
                                </div>

                                {/* Địa điểm & Mô tả */}
                                {(editedEvent.location || isEditing) && (
                                    <div className="flex gap-4">
                                        <MapPin className="w-5 h-5 text-[#EEDC9A] mt-1" />
                                        <div className="flex-1">
                                            <div className="font-bold text-m text-[#EEDC9A] uppercase tracking-wider mb-3">Địa điểm</div>
                                            {isEditing ? (
                                                <input
                                                    type="text"
                                                    value={editedEvent.location || ''}
                                                    onChange={e => setEditedEvent(prev => ({ ...prev, location: e.target.value }))}
                                                    className="w-full px-3 py-2 rounded-lg bg-[#0d1321]/60 border border-[#D1B066]/20 focus:border-[#D1B066] text-white text-sm placeholder-white/40 outline-none transition-all"
                                                    placeholder="Nhập địa điểm..."
                                                />
                                            ) : (
                                                <p className="text-white">{event.location || '—'}</p>
                                            )}
                                        </div>
                                    </div>
                                )}

                                {(editedEvent.description || isEditing) && (
                                    <div className="flex gap-4">
                                        <FileText className="w-5 h-5 text-[#EEDC9A] mt-1" />
                                        <div className="flex-1">
                                            <div className="font-bold text-m text-[#EEDC9A] uppercase tracking-wider mb-3">Mô tả</div>
                                            {isEditing ? (
                                                <textarea
                                                    value={editedEvent.description || ''}
                                                    onChange={e => setEditedEvent(prev => ({ ...prev, description: e.target.value }))}
                                                    rows={3}
                                                    className="w-full px-3 py-2 rounded-lg bg-[#0d1321]/60 border border-[#D1B066]/20 focus:border-[#D1B066] text-white text-sm placeholder-white/40 outline-none transition-all resize-none"
                                                    placeholder="Nhập mô tả..."
                                                />
                                            ) : (
                                                <p className="text-white/90 whitespace-pre-wrap">{event.description || '—'}</p>
                                            )}
                                        </div>
                                    </div>
                                )}
                            </div>
                        </div>
                    )}
                </div>

                {/* Footer */}
                <div className="relative px-8 py-3 border-t border-[#D1B066]/30 bg-gradient-to-r from-[#2e3a57]/60 to-[#1b2233]/60 backdrop-blur-xl">
                    <div className="flex items-center justify-end gap-3">
                        {isEditing ? (
                            <>
                                <button
                                    onClick={() => {
                                        setIsEditing(false);
                                        setEditedEvent(event!);
                                        setErrors({});
                                    }}
                                    className="px-6 h-10 rounded-xl border-2 border-[#D1B066]/50 text-[#EEDC9A] font-semibold hover:bg-[#D1B066]/10 hover:border-[#D1B066] transition-all duration-300 hover:scale-105"
                                >
                                    Hủy
                                </button>
                                <button
                                    onClick={handleSave}
                                    className="relative flex items-center gap-2 px-8 h-10 rounded-xl font-bold text-[#1b2233] bg-gradient-to-r from-[#EEDC9A] to-[#B69563] shadow-[0_0_20px_rgba(209,176,102,0.5)] hover:shadow-[0_0_30px_rgba(209,176,102,0.7)] hover:scale-105 transition-all duration-300 overflow-hidden"
                                >
                                    <Save className="w-5 h-5" />
                                    Lưu
                                </button>
                            </>
                        ) : (
                            <>
                                <button
                                    onClick={() => setIsEditing(true)}
                                    className="relative flex items-center gap-2 px-6 h-10 rounded-lg font-bold text-[#1b2233] bg-gradient-to-r from-[#EEDC9A] to-[#B69563] shadow-[0_0_20px_rgba(209,176,102,0.5)] hover:shadow-[0_0_30px_rgba(209,176,102,0.7)] hover:scale-105 transition-all duration-300 overflow-hidden"
                                >
                                    <Edit2 className="w-5 h-5" />
                                    Chỉnh sửa
                                </button>
                                <button
                                    onClick={() => setShowDeleteModal(true)}
                                    className="relative flex items-center gap-2 px-6 h-10 rounded-lg font-bold text-white bg-gradient-to-r from-red-500 to-red-600 shadow-[0_0_20px_rgba(239,68,68,0.5)] hover:shadow-[0_0_30px_rgba(239,68,68,0.7)] hover:scale-105 transition-all duration-300 overflow-hidden"
                                >
                                    <Trash2 className="w-5 h-5 inline mr-1" />
                                    Xóa
                                </button>
                            </>
                        )}
                    </div>
                </div>

                {/* Xác nhận xóa */}
                {showDeleteModal && (
                    <div className="absolute inset-0 z-[100003] flex items-center justify-center bg-black/50">
                        <div className="relative bg-gradient-to-br from-[#2e3a57] to-[#1b2233] rounded-2xl p-6 max-w-md w-full mx-4 shadow-[0_0_50px_rgba(239,68,68,0.4)] border-2 border-red-500/50 animate-scale-in">
                            <div className="text-center mt-2 mb-6">
                                <h3 className="text-2xl font-bold bg-gradient-to-r from-red-400 to-red-500 bg-clip-text text-transparent mb-3">
                                    Xác nhận xóa sự kiện?
                                </h3>
                                <p className="text-white/80 leading-relaxed">
                                    Sự kiện <span className="font-bold text-[#EEDC9A]">"{event?.title}"</span> sẽ bị xóa vĩnh viễn.
                                </p>
                                <p className="text-red-400 text-sm mt-2 font-semibold">
                                    Hành động này không thể hoàn tác!
                                </p>
                            </div>
                            <div className="flex gap-3">
                                <button
                                    onClick={() => setShowDeleteModal(false)}
                                    className="flex-1 h-12 rounded-xl border-2 border-[#D1B066]/50 text-[#EEDC9A] font-semibold hover:bg-[#D1B066]/10 hover:border-[#D1B066] transition-all duration-300 hover:scale-105"
                                >
                                    Hủy bỏ
                                </button>
                                <button
                                    onClick={handleDelete}
                                    className="relative flex-1 h-12 rounded-xl bg-gradient-to-r from-red-500 to-red-600 text-white font-bold shadow-[0_0_20px_rgba(239,68,68,0.5)] hover:shadow-[0_0_30px_rgba(239,68,68,0.7)] hover:scale-105 transition-all duration-300 overflow-hidden"
                                >
                                    Xóa ngay
                                    <div className="absolute inset-0 bg-gradient-to-r from-white/0 via-white/30 to-white/0 translate-x-[-200%] hover:translate-x-[200%] transition-transform duration-700" />
                                </button>
                            </div>
                        </div>
                    </div>
                )}
            </div>
        </div>
    );
};

export default EventDetailModal;