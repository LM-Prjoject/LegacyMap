import React, { useState, useEffect } from 'react';
import { X, Edit2, Trash2, Calendar, Bell, MapPin, Save, FileText, Share2 } from 'lucide-react';
import { eventsApi } from '@/api/eventApi.ts';
import { Event, EventType, CalendarType } from '@/types/event.ts';

interface EventDetailModalProps {
    eventId: string;
    isOpen: boolean;
    onClose: () => void;
    onDelete?: () => void;
    onUpdate?: () => void;
}

const EventDetailModal: React.FC<EventDetailModalProps> = ({
                                                               eventId,
                                                               isOpen,
                                                               onClose,
                                                               onDelete,
                                                               onUpdate
                                                           }) => {
    const [event, setEvent] = useState<Event | null>(null);
    const [isEditing, setIsEditing] = useState(false);
    const [editedEvent, setEditedEvent] = useState<Partial<Event>>({});
    const [showDeleteModal, setShowDeleteModal] = useState(false);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);

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
            setEditedEvent(eventData);
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
            await eventsApi.updateEvent(eventId, editedEvent);
            setEvent({ ...event, ...editedEvent } as Event);
            setIsEditing(false);
            onUpdate?.();
        } catch (err) {
            setError(err instanceof Error ? err.message : 'Failed to update event');
        }
    };

    const handleShare = () => {
        if (event && navigator.share) {
            navigator.share({
                title: event.title,
                text: `Sự kiện: ${event.title} - ${new Date(event.startDate).toLocaleDateString('vi-VN')}`,
            });
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

    if (!isOpen) return null;

    return (
        <div className="fixed inset-0 bg-black/70 backdrop-blur-sm flex items-center justify-center z-[9999] p-6">
            <div className="bg-[#2a3548] rounded-2xl shadow-2xl w-full max-w-2xl max-h-[85vh] overflow-hidden flex flex-col">
                {/* Header */}
                <div className="bg-[#2a3548] px-6 py-4 flex justify-between items-center border-b border-[#ffd89b]/20">
                    <h2 className="text-xl font-bold text-[#ffd89b]">
                        {isEditing ? 'Chỉnh sửa sự kiện' : 'Chi tiết sự kiện'}
                    </h2>
                    <div className="flex items-center gap-2">
                        {!isEditing && (
                            <button
                                onClick={handleShare}
                                className="p-2 hover:bg-[#ffd89b]/10 rounded-lg transition-colors text-[#ffd89b]"
                                title="Chia sẻ"
                            >
                                <Share2 className="w-5 h-5" />
                            </button>
                        )}
                        <button
                            onClick={onClose}
                            className="p-2 hover:bg-[#ffd89b]/10 rounded-lg transition-colors text-[#ffd89b]"
                        >
                            <X className="w-6 h-6" />
                        </button>
                    </div>
                </div>

                {/* Content */}
                <div className="flex-1 overflow-y-auto bg-[#f0e6d2]">
                    {loading ? (
                        <div className="flex items-center justify-center py-20">
                            <div className="text-center">
                                <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#2a3548] mx-auto mb-4"></div>
                                <p className="text-[#2a3548] font-medium">Đang tải...</p>
                            </div>
                        </div>
                    ) : error || !event ? (
                        <div className="text-center py-20">
                            <p className="text-red-600 font-medium">Lỗi: {error || 'Event not found'}</p>
                        </div>
                    ) : (
                        <div className="p-8 space-y-6">
                            {/* Title */}
                            <div>
                                {isEditing ? (
                                    <input
                                        type="text"
                                        value={editedEvent.title || ''}
                                        onChange={(e) => setEditedEvent({ ...editedEvent, title: e.target.value })}
                                        className="w-full text-3xl font-bold text-[#2a3548] bg-white/60 border-2 border-[#ffd89b] rounded-xl px-4 py-3 focus:outline-none focus:border-[#2a3548] focus:bg-white"
                                        placeholder="Tiêu đề sự kiện"
                                    />
                                ) : (
                                    <h3 className="text-3xl font-bold text-[#2a3548]">{event.title}</h3>
                                )}
                            </div>

                            {/* Main Content Card */}
                            <div className="bg-white/60 backdrop-blur rounded-xl p-6 space-y-5 border border-[#ffd89b]/50">
                                {/* Date & Time Section */}
                                <div className="flex gap-4">
                                    <div className="flex-shrink-0">
                                        <div className="bg-[#2a3548] p-3 rounded-lg">
                                            <Calendar className="w-6 h-6 text-[#ffd89b]" />
                                        </div>
                                    </div>
                                    <div className="flex-1">
                                        <h4 className="font-semibold text-[#2a3548] mb-2 text-sm uppercase tracking-wide">Ngày & Giờ</h4>
                                        {isEditing ? (
                                            <div className="space-y-3">
                                                <div>
                                                    <label className="block text-sm text-[#2a3548]/70 mb-1">Bắt đầu</label>
                                                    <input
                                                        type="datetime-local"
                                                        value={editedEvent.startDate?.slice(0, 16) || ''}
                                                        onChange={(e) => setEditedEvent({ ...editedEvent, startDate: e.target.value })}
                                                        className="w-full px-3 py-2 rounded-lg border border-[#ffd89b] focus:border-[#2a3548] focus:outline-none bg-white"
                                                    />
                                                </div>
                                                <div>
                                                    <label className="block text-sm text-[#2a3548]/70 mb-1">Kết thúc</label>
                                                    <input
                                                        type="datetime-local"
                                                        value={editedEvent.endDate?.slice(0, 16) || ''}
                                                        onChange={(e) => setEditedEvent({ ...editedEvent, endDate: e.target.value })}
                                                        className="w-full px-3 py-2 rounded-lg border border-[#ffd89b] focus:border-[#2a3548] focus:outline-none bg-white"
                                                    />
                                                </div>
                                            </div>
                                        ) : (
                                            <div>
                                                <p className="text-[#2a3548] font-medium text-lg">
                                                    {formatDateTime(event.startDate)}
                                                </p>
                                                {event.endDate && (
                                                    <p className="text-[#2a3548]/70 mt-1">
                                                        đến {formatDateTime(event.endDate)}
                                                    </p>
                                                )}
                                                <div className="flex gap-2 mt-3">
                                                    <span className={`inline-block px-3 py-1 rounded-full text-xs font-semibold ${
                                                        event.calendarType === CalendarType.LUNAR
                                                            ? 'bg-[#ffd89b] text-[#2a3548]'
                                                            : 'bg-[#2a3548] text-[#ffd89b]'
                                                    }`}>
                                                        {getCalendarTypeLabel(event.calendarType)}
                                                    </span>
                                                    {event.isFullDay && (
                                                        <span className="inline-block px-3 py-1 rounded-full text-xs font-semibold bg-green-100 text-green-700">
                                                            Cả ngày
                                                        </span>
                                                    )}
                                                </div>
                                            </div>
                                        )}
                                    </div>
                                </div>

                                <div className="border-t border-[#ffd89b]/30"></div>

                                {/* Event Type Section */}
                                <div className="flex gap-4">
                                    <div className="flex-shrink-0">
                                        <div className="bg-[#ffd89b] p-3 rounded-lg">
                                            <Bell className="w-6 h-6 text-[#2a3548]" />
                                        </div>
                                    </div>
                                    <div className="flex-1">
                                        <h4 className="font-semibold text-[#2a3548] mb-2 text-sm uppercase tracking-wide">Loại sự kiện</h4>
                                        {isEditing ? (
                                            <select
                                                value={editedEvent.eventType || ''}
                                                onChange={(e) => setEditedEvent({ ...editedEvent, eventType: e.target.value as EventType })}
                                                className="w-full px-3 py-2 rounded-lg border border-[#ffd89b] focus:border-[#2a3548] focus:outline-none bg-white"
                                            >
                                                {Object.values(EventType).map((type) => (
                                                    <option key={type} value={type}>
                                                        {getEventTypeLabel(type)}
                                                    </option>
                                                ))}
                                            </select>
                                        ) : (
                                            <div>
                                                <p className="text-[#2a3548] font-medium text-lg mb-2">
                                                    {getEventTypeLabel(event.eventType)}
                                                </p>
                                                {event.reminder && (
                                                    <p className="text-[#2a3548]/70 text-sm">
                                                        Nhắc nhở {event.reminder.daysBefore} ngày trước
                                                    </p>
                                                )}
                                                <div className="flex gap-2 mt-3">
                                                    <span className={`inline-block px-3 py-1 rounded-full text-xs font-semibold ${
                                                        event.isPublic
                                                            ? 'bg-green-100 text-green-700'
                                                            : 'bg-gray-200 text-gray-700'
                                                    }`}>
                                                        {event.isPublic ? 'Công khai' : 'Riêng tư'}
                                                    </span>
                                                    <span className={`inline-block px-3 py-1 rounded-full text-xs font-semibold ${
                                                        event.status === 'active'
                                                            ? 'bg-[#2a3548] text-[#ffd89b]'
                                                            : event.status === 'completed'
                                                                ? 'bg-green-100 text-green-700'
                                                                : 'bg-red-100 text-red-700'
                                                    }`}>
                                                        {event.status === 'active' && 'Đang hoạt động'}
                                                        {event.status === 'completed' && 'Đã hoàn thành'}
                                                        {event.status === 'cancelled' && 'Đã hủy'}
                                                    </span>
                                                </div>
                                            </div>
                                        )}
                                    </div>
                                </div>

                                {/* Location Section */}
                                {(event.location || isEditing) && (
                                    <>
                                        <div className="border-t border-[#ffd89b]/30"></div>
                                        <div className="flex gap-4">
                                            <div className="flex-shrink-0">
                                                <div className="bg-[#2a3548] p-3 rounded-lg">
                                                    <MapPin className="w-6 h-6 text-[#ffd89b]" />
                                                </div>
                                            </div>
                                            <div className="flex-1">
                                                <h4 className="font-semibold text-[#2a3548] mb-2 text-sm uppercase tracking-wide">Địa điểm</h4>
                                                {isEditing ? (
                                                    <input
                                                        type="text"
                                                        value={editedEvent.location || ''}
                                                        onChange={(e) => setEditedEvent({ ...editedEvent, location: e.target.value })}
                                                        className="w-full px-3 py-2 rounded-lg border border-[#ffd89b] focus:border-[#2a3548] focus:outline-none bg-white"
                                                        placeholder="Nhập địa điểm..."
                                                    />
                                                ) : (
                                                    <p className="text-[#2a3548] font-medium">{event.location}</p>
                                                )}
                                            </div>
                                        </div>
                                    </>
                                )}

                                {/* Description Section */}
                                {(event.description || isEditing) && (
                                    <>
                                        <div className="border-t border-[#ffd89b]/30"></div>
                                        <div className="flex gap-4">
                                            <div className="flex-shrink-0">
                                                <div className="bg-[#ffd89b] p-3 rounded-lg">
                                                    <FileText className="w-6 h-6 text-[#2a3548]" />
                                                </div>
                                            </div>
                                            <div className="flex-1">
                                                <h4 className="font-semibold text-[#2a3548] mb-2 text-sm uppercase tracking-wide">Mô tả</h4>
                                                {isEditing ? (
                                                    <textarea
                                                        value={editedEvent.description || ''}
                                                        onChange={(e) => setEditedEvent({ ...editedEvent, description: e.target.value })}
                                                        className="w-full px-3 py-2 rounded-lg border border-[#ffd89b] focus:border-[#2a3548] focus:outline-none min-h-[100px] bg-white"
                                                        placeholder="Nhập mô tả..."
                                                    />
                                                ) : (
                                                    <p className="text-[#2a3548] leading-relaxed whitespace-pre-wrap">
                                                        {event.description}
                                                    </p>
                                                )}
                                            </div>
                                        </div>
                                    </>
                                )}

                                {/* Related Persons */}
                                {event.relatedPersons && event.relatedPersons.length > 0 && (
                                    <>
                                        <div className="border-t border-[#ffd89b]/30"></div>
                                        <div>
                                            <h4 className="font-semibold text-[#2a3548] mb-3 text-sm uppercase tracking-wide">Người liên quan</h4>
                                            <div className="flex flex-wrap gap-2">
                                                {event.relatedPersons.map((person, index) => (
                                                    <span key={index} className="bg-[#2a3548] text-[#ffd89b] px-4 py-2 rounded-lg text-sm font-medium">
                                                        {person.name}
                                                    </span>
                                                ))}
                                            </div>
                                        </div>
                                    </>
                                )}
                            </div>
                        </div>
                    )}
                </div>

                {/* Footer Actions */}
                {!loading && event && (
                    <div className="bg-[#2a3548] px-6 py-4 border-t border-[#ffd89b]/20">
                        {isEditing ? (
                            <div className="flex gap-3">
                                <button
                                    onClick={() => {
                                        setIsEditing(false);
                                        setEditedEvent(event);
                                    }}
                                    className="flex-1 bg-[#f0e6d2] text-[#2a3548] py-3 rounded-lg font-semibold hover:bg-[#ffd89b] transition-all"
                                >
                                    Hủy
                                </button>
                                <button
                                    onClick={handleSave}
                                    className="flex-1 bg-[#ffd89b] text-[#2a3548] py-3 rounded-lg font-semibold flex items-center justify-center gap-2 hover:bg-[#ffd89b]/90 transition-all"
                                >
                                    <Save className="w-5 h-5" />
                                    Lưu thay đổi
                                </button>
                            </div>
                        ) : (
                            <div className="flex gap-3">
                                <button
                                    onClick={() => setIsEditing(true)}
                                    className="flex-1 bg-[#ffd89b] text-[#2a3548] py-3 rounded-lg font-semibold flex items-center justify-center gap-2 hover:bg-[#ffd89b]/90 transition-all"
                                >
                                    <Edit2 className="w-5 h-5" />
                                    Chỉnh sửa
                                </button>
                                <button
                                    onClick={() => setShowDeleteModal(true)}
                                    className="flex-1 bg-red-600 text-white py-3 rounded-lg font-semibold flex items-center justify-center gap-2 hover:bg-red-700 transition-all"
                                >
                                    <Trash2 className="w-5 h-5" />
                                    Xóa sự kiện
                                </button>
                            </div>
                        )}
                    </div>
                )}
            </div>

            {/* Delete Confirmation Modal */}
            {showDeleteModal && (
                <div className="fixed inset-0 bg-black/80 backdrop-blur-sm flex items-center justify-center z-[10000] p-4">
                    <div className="bg-[#f0e6d2] rounded-xl shadow-2xl max-w-md w-full p-8 border-2 border-[#ffd89b]">
                        <div className="text-center mb-6">
                            <div className="bg-red-100 w-16 h-16 rounded-full flex items-center justify-center mx-auto mb-4">
                                <Trash2 className="w-8 h-8 text-red-600" />
                            </div>
                            <h3 className="text-2xl font-bold text-[#2a3548] mb-2">Xóa sự kiện?</h3>
                            <p className="text-[#2a3548]/70">
                                Bạn có chắc chắn muốn xóa sự kiện "<span className="font-semibold">{event?.title}</span>"? Hành động này không thể hoàn tác.
                            </p>
                        </div>
                        <div className="flex gap-3">
                            <button
                                onClick={() => setShowDeleteModal(false)}
                                className="flex-1 bg-gray-300 text-[#2a3548] py-3 rounded-lg font-semibold hover:bg-gray-400 transition-colors"
                            >
                                Hủy
                            </button>
                            <button
                                onClick={handleDelete}
                                className="flex-1 bg-red-600 text-white py-3 rounded-lg font-semibold hover:bg-red-700 transition-all"
                            >
                                Xóa
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
};

export default EventDetailModal;