import React, { useState, useEffect } from 'react';
import { X, Edit2, Trash2, Calendar, Bell, MapPin, Save, FileText } from 'lucide-react';
import { eventsApi } from '@/api/eventApi.ts';
import { Event, EventType, CalendarType } from '@/types/event.ts';

interface EventDetailModalProps {
    eventId: string;
    isOpen: boolean;
    onClose: () => void;
    onDelete?: () => void;
    onUpdate?: (updatedEvent?: Event) => void;
}

const EventDetailModal: React.FC<EventDetailModalProps> = ({ eventId, isOpen, onClose, onDelete, onUpdate }) => {
    const [event, setEvent] = useState<Event | null>(null);
    const [isEditing, setIsEditing] = useState(false);
    const [editedEvent, setEditedEvent] = useState<Partial<Event>>({});
    const [showDeleteModal, setShowDeleteModal] = useState(false);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);

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
        }
    }, [isOpen, eventId]);

    const fetchEvent = async () => {
        setLoading(true);
        setError(null);
        try {
            const eventData = await eventsApi.getEvent(eventId);
            setEvent(eventData);
            setEditedEvent({
                ...eventData,
                startDate: isoToLocalInput(eventData.startDate, eventData.isFullDay),
                endDate: isoToLocalInput(eventData.endDate, eventData.isFullDay),
            });
        } catch (err) {
            setError(err instanceof Error ? err.message : 'Failed to fetch event');
        } finally {
            setLoading(false);
        }
    };

    const handleDelete = async () => {
        try {
            await eventsApi.deleteEvent(eventId);
            onDelete?.();
            onClose();
        } catch (err) {
            setError(err instanceof Error ? err.message : 'Failed to delete event');
        }
    };

    const handleSave = async () => {
        try {
            const payload = {
                ...editedEvent,
                startDate: editedEvent.startDate ? localInputToIso(editedEvent.startDate, event?.isFullDay) : undefined,
                endDate: editedEvent.endDate ? localInputToIso(editedEvent.endDate, event?.isFullDay) : undefined,
            };
            await eventsApi.updateEvent(eventId, payload);

            const updatedEvent = { ...event, ...payload } as Event;
            setEvent(updatedEvent);
            onUpdate?.(updatedEvent);
            setIsEditing(false);
        } catch (err) {
            setError(err instanceof Error ? err.message : 'Failed to update event');
        }
    };

    const getEventTypeLabel = (type: EventType): string => {
        const labels: Record<EventType, string> = {
            [EventType.BIRTHDAY]: 'Sinh nhật',
            [EventType.DEATH_ANNIVERSARY]: 'Ngày giỗ',
            [EventType.WEDDING_ANNIVERSARY]: 'Ngày cưới',
            [EventType.WEDDING]: 'Đám cưới',
            [EventType.FUNERAL]: 'Tang lễ',
            [EventType.FAMILY_REUNION]: 'Họp mặt gia đình',
            [EventType.CEREMONY]: 'Nghi lễ',
            [EventType.OTHER]: 'Khác'
        };
        return labels[type] || type;
    };

    const getCalendarTypeLabel = (type: CalendarType): string => {
        return type === CalendarType.LUNAR ? 'Âm lịch' : 'Dương lịch';
    };

    const formatDateTime = (dateTime: string) => {
        return new Date(dateTime).toLocaleString('vi-VN', {
            year: 'numeric',
            month: 'long',
            day: 'numeric',
            hour: '2-digit',
            minute: '2-digit'
        });
    };

    const isoToLocalInput = (isoString?: string, isFullDay = false): string => {
        if (!isoString) return '';
        const date = new Date(isoString);
        const pad = (n: number) => String(n).padStart(2, '0');
        const y = date.getFullYear();
        const m = pad(date.getMonth() + 1);
        const d = pad(date.getDate());
        if (isFullDay) return `${y}-${m}-${d}`;
        const hh = pad(date.getHours());
        const mm = pad(date.getMinutes());
        return `${y}-${m}-${d}T${hh}:${mm}`;
    };

    const localInputToIso = (localString: string, isFullDay = false): string => {
        if (!localString) return '';
        if (!isFullDay) {
            const d = new Date(localString);
            return d.toISOString();
        }
        const [y, m, day] = localString.split('-').map(Number);
        const localMidnight = new Date(y, m - 1, day, 0, 0, 0);
        return localMidnight.toISOString();
    };

    const getMinDateTime = () => {
        const now = new Date();
        now.setSeconds(0, 0);
        const pad = (n: number) => String(n).padStart(2, '0');
        const y = now.getFullYear();
        const m = pad(now.getMonth() + 1);
        const d = pad(now.getDate());
        const hh = pad(now.getHours());
        const mm = pad(now.getMinutes());
        return `${y}-${m}-${d}T${hh}:${mm}`;
    };

    if (!isOpen) return null;

    return (
        <div className="fixed inset-0 z-[9999] flex items-center justify-center p-4">
            <div
                className="absolute inset-0"
                style={{
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

                {/* Body */}
                <div className="relative px-8 py-6 flex-1 overflow-y-auto">
                    {loading ? (
                        <div className="flex items-center justify-center py-20">
                            <div className="text-center">
                                <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#D1B066] mx-auto mb-4"></div>
                                <p className="text-[#D1B066] font-medium">Đang tải...</p>
                            </div>
                        </div>
                    ) : error || !event ? (
                        <div className="text-center py-20">
                            <p className="text-red-400 font-semibold text-lg">Lỗi: {error || 'Không tìm thấy sự kiện'}</p>
                        </div>
                    ) : (
                        <div className="space-y-5">
                            {/* Title */}
                            {isEditing ? (
                                <div className="flex gap-4">
                                    <Edit2 className="w-5 h-5 text-[#EEDC9A] mt-1" />
                                    <div>
                                        <div className="font-bold text-m text-[#EEDC9A] uppercase tracking-wider mb-3">Tiêu đề</div>
                                        <input
                                            type="text"
                                            value={editedEvent.title || ''}
                                            onChange={(e) => setEditedEvent({ ...editedEvent, title: e.target.value })}
                                            className="w-full bg-transparent border-b border-white/30 text-white outline-none"
                                            placeholder="Nhập tiêu đề sự kiện..."
                                        />
                                    </div>
                                </div>
                            ) : (
                                <div className="relative">
                                    <h3 className="text-3xl font-bold bg-gradient-to-r from-[#EEDC9A] to-[#D1B066] bg-clip-text text-transparent">
                                        {event.title}
                                    </h3>
                                </div>
                            )}

                            {/* Main Content */}
                            <div className="space-y-6">
                                {/* Date & Time */}
                                <div className="flex gap-4">
                                    <Calendar className="w-5 h-5 text-[#EEDC9A] mt-1" />
                                    <div>
                                        <div className="font-bold text-m text-[#EEDC9A] uppercase tracking-wider mb-3">Ngày & Giờ</div>
                                        {isEditing ? (
                                            <div className="space-y-3">
                                                <input
                                                    type="datetime-local"
                                                    min={getMinDateTime()}
                                                    value={editedEvent.startDate || ''}
                                                    onChange={e => setEditedEvent({...editedEvent, startDate: e.target.value})}
                                                    className="w-full bg-[#1b2233]/50 border-2 border-[#D1B066]/30 focus:border-[#D1B066] rounded-xl px-4 py-3 text-white outline-none transition-all duration-300"
                                                />
                                                <input
                                                    type="datetime-local"
                                                    min={getMinDateTime()}
                                                    value={editedEvent.endDate || ''}
                                                    onChange={e => setEditedEvent({...editedEvent, endDate: e.target.value})}
                                                    className="w-full bg-[#1b2233]/50 border-2 border-[#D1B066]/30 focus:border-[#D1B066] rounded-xl px-4 py-3 text-white outline-none transition-all duration-300"
                                                />
                                            </div>
                                        ) : (
                                            <>
                                                <p className="font-semibold text-white text-lg">{formatDateTime(event.startDate)}</p>
                                                {event.endDate && <p className="text-sm text-white/70 mt-1">đến {formatDateTime(event.endDate)}</p>}
                                                <div className="flex flex-wrap gap-2 mt-3">
                                                    <span className="px-4 py-2 rounded-xl text-xs font-bold bg-gradient-to-r from-[#EEDC9A] to-[#D1B066] text-[#1b2233] shadow-lg">
                                                        {getCalendarTypeLabel(event.calendarType)}
                                                    </span>
                                                    {event.isFullDay && (
                                                        <span className="px-4 py-2 rounded-xl text-xs font-bold bg-gradient-to-r from-emerald-500 to-emerald-600 text-white shadow-lg">
                                                            Cả ngày
                                                        </span>
                                                    )}
                                                </div>
                                            </>
                                        )}
                                    </div>
                                </div>

                                {/* Event Type */}
                                <div className="flex gap-4">
                                    <Bell className="w-5 h-5 text-[#EEDC9A] mt-1" />
                                    <div>
                                        <div className="font-bold text-m text-[#EEDC9A] uppercase tracking-wider mb-3">Loại sự kiện</div>
                                        {isEditing ? (
                                            <select
                                                value={editedEvent.eventType || ''}
                                                onChange={(e) => setEditedEvent({ ...editedEvent, eventType: e.target.value as EventType })}
                                                className="w-full bg-transparent border-b border-white/30 text-white outline-none"
                                            >
                                                {Object.values(EventType).map((type) => (
                                                    <option key={type} value={type} className="bg-[#2e3a57] text-white">
                                                        {getEventTypeLabel(type)}
                                                    </option>
                                                ))}
                                            </select>
                                        ) : (
                                            <>
                                                <p className="font-semibold text-white text-m">{getEventTypeLabel(event.eventType)}</p>
                                                {event.reminder && <p className="text-sm text-white/70 mt-1">Nhắc nhở {event.reminder.daysBefore} ngày trước</p>}
                                                {/* Privacy display removed: per-event public/private removed */}
                                            </>
                                        )}
                                    </div>
                                </div>

                                {/* Location */}
                                {(event.location || isEditing) && (
                                    <div className="flex gap-4">
                                        <MapPin className="w-5 h-5 text-[#EEDC9A] mt-1" />
                                        <div>
                                            <div className="font-bold text-m text-[#EEDC9A] uppercase tracking-wider mb-3">Địa điểm</div>
                                            {isEditing ? (
                                                <input
                                                    type="text"
                                                    value={editedEvent.location || ''}
                                                    onChange={(e) => setEditedEvent({ ...editedEvent, location: e.target.value })}
                                                    className="w-full bg-transparent border-b border-white/30 text-white outline-none"
                                                    placeholder="Nhập địa điểm..."
                                                />
                                            ) : (
                                                <p className="font-semibold text-white text-m">{event.location}</p>
                                            )}
                                        </div>
                                    </div>
                                )}

                                {/* Description */}
                                {(event.description || isEditing) && (
                                    <div className="flex gap-4">
                                        <FileText className="w-5 h-5 text-[#EEDC9A] mt-1" />
                                        <div>
                                            <div className="font-bold text-m text-[#EEDC9A] uppercase tracking-wider mb-3">Mô tả</div>
                                            {isEditing ? (
                                                <input
                                                    type="text"
                                                    value={editedEvent.description || ''}
                                                    onChange={(e) => setEditedEvent({ ...editedEvent, description: e.target.value })}
                                                    className="w-full bg-transparent border-b border-white/30 text-white outline-none"
                                                    placeholder="Nhập mô tả..."
                                                />
                                            ) : (
                                                <p className="text-white/90 whitespace-pre-wrap leading-relaxed">{event.description}</p>
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
                                    <div className="absolute inset-0 bg-gradient-to-r from-white/0 via-white/30 to-white/0 translate-x-[-200%] hover:translate-x-[200%] transition-transform duration-700" />
                                </button>
                                <button
                                    onClick={() => setShowDeleteModal(true)}
                                    className="relative flex items-center gap-2 px-6 h-10 rounded-lg font-bold text-white bg-gradient-to-r from-red-500 to-red-600 shadow-[0_0_20px_rgba(239,68,68,0.5)] hover:shadow-[0_0_30px_rgba(239,68,68,0.7)] hover:scale-105 transition-all duration-300 overflow-hidden"
                                >
                                    <Trash2 className="w-5 h-5 inline mr-1" />
                                    Xóa
                                    <div className="absolute inset-0 bg-gradient-to-r from-white/0 via-white/30 to-white/0 translate-x-[-200%] hover:translate-x-[200%] transition-transform duration-700" />
                                </button>
                            </>
                        )}
                    </div>
                </div>
            </div>

            {/* Delete Confirm Modal */}
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
    );
};

export default EventDetailModal;