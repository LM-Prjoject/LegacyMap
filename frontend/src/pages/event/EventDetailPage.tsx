import React, { useState, useEffect } from 'react';
import { useNavigate, useParams } from 'react-router-dom';
import { ArrowLeft, Edit2, Trash2, Calendar, Bell, FileText, Share2 } from 'lucide-react';
import { eventsApi } from '@/api/eventApi';
import { Event, EventType, CalendarType } from '@/types/event';

const EventDetailPage: React.FC = () => {
    const navigate = useNavigate();
    const { id } = useParams<{ id: string }>();
    const [event, setEvent] = useState<Event | null>(null);
    const [showDeleteModal, setShowDeleteModal] = useState(false);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);

    useEffect(() => {
        const fetchEvent = async () => {
            if (!id) return;

            setLoading(true);
            setError(null);
            try {
                const eventData = await eventsApi.getEvent(id);
                setEvent(eventData);
            } catch (err) {
                setError(err instanceof Error ? err.message : 'Failed to fetch event');
            } finally {
                setLoading(false);
            }
        };

        fetchEvent();
    }, [id]);


    const handleDelete = async () => {
        if (!id) return;

        try {
            await eventsApi.deleteEvent(id);
            navigate('/events');
        } catch (err) {
            setError(err instanceof Error ? err.message : 'Failed to delete event');
        }
    };

    const handleEdit = () => {
        navigate(`/events/${id}/edit`);
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

    if (loading) {
        return (
            <div className="min-h-screen bg-gradient-to-br from-blue-50 via-amber-50 to-blue-100 flex items-center justify-center">
                <div className="text-center">
                    <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto mb-4"></div>
                    <p className="text-blue-900">Đang tải...</p>
                </div>
            </div>
        );
    }

    if (error || !event) {
        return (
            <div className="min-h-screen bg-gradient-to-br from-blue-50 via-amber-50 to-blue-100 flex items-center justify-center">
                <div className="text-center">
                    <p className="text-red-600">Lỗi: {error || 'Event not found'}</p>
                    <button
                        onClick={() => navigate('/events')}
                        className="mt-4 bg-blue-600 text-white px-4 py-2 rounded-lg"
                    >
                        Quay lại
                    </button>
                </div>
            </div>
        );
    }

    return (
        <div className="min-h-screen bg-gradient-to-br from-blue-50 via-amber-50 to-blue-100">
            {/* Header */}
            <div className="bg-gradient-to-r from-blue-600 to-blue-700 text-white sticky top-0 z-10 shadow-lg">
                <div className="max-w-4xl mx-auto px-6 py-4">
                    <div className="flex justify-between items-center">
                        <button
                            onClick={() => navigate('/events')}
                            className="p-2 hover:bg-white/20 rounded-full transition-colors"
                        >
                            <ArrowLeft className="w-6 h-6" />
                        </button>
                        <h1 className="text-xl font-bold">Chi tiết sự kiện</h1>
                        <div className="flex items-center gap-2">
                            <button
                                onClick={handleShare}
                                className="p-2 hover:bg-white/20 rounded-full transition-colors"
                            >
                                <Share2 className="w-5 h-5" />
                            </button>
                            <button
                                onClick={handleEdit}
                                className="p-2 hover:bg-white/20 rounded-full transition-colors"
                            >
                                <Edit2 className="w-5 h-5" />
                            </button>
                            <button
                                onClick={() => setShowDeleteModal(true)}
                                className="p-2 hover:bg-red-500/30 rounded-full transition-colors"
                            >
                                <Trash2 className="w-5 h-5" />
                            </button>
                        </div>
                    </div>
                </div>
            </div>

            <div className="max-w-4xl mx-auto px-6 py-8">
                {/* Event Title Card */}
                <div className="bg-gradient-to-br from-white to-amber-50 rounded-3xl shadow-2xl p-8 mb-6 border border-blue-100">
                    <h2 className="text-3xl font-bold text-blue-900 mb-4">{event.title}</h2>

                    <div className="grid md:grid-cols-2 gap-6">
                        {/* Date & Time */}
                        <div className="bg-white/80 backdrop-blur rounded-2xl p-6 shadow-md border border-blue-100">
                            <div className="flex items-start gap-4">
                                <div className="bg-gradient-to-br from-blue-500 to-blue-600 p-3 rounded-xl shadow-lg">
                                    <Calendar className="w-6 h-6 text-white" />
                                </div>
                                <div className="flex-1">
                                    <h3 className="font-semibold text-blue-900 mb-2">Ngày & Giờ</h3>
                                    <p className="text-blue-700 font-medium">
                                        {formatDateTime(event.startDate)}
                                    </p>
                                    {event.endDate && (
                                        <p className="text-blue-600 mt-1">
                                            <strong>Kết thúc:</strong> {formatDateTime(event.endDate)}
                                        </p>
                                    )}
                                    <div className="mt-2">
                                        <span className={`inline-block px-3 py-1 rounded-full text-xs font-semibold ${
                                            event.calendarType === CalendarType.LUNAR
                                                ? 'bg-amber-100 text-amber-700'
                                                : 'bg-blue-100 text-blue-700'
                                        }`}>
                                            {getCalendarTypeLabel(event.calendarType)}
                                        </span>
                                        {event.isFullDay && (
                                            <span className="inline-block px-3 py-1 rounded-full text-xs font-semibold bg-green-100 text-green-700 ml-2">
                                                Cả ngày
                                            </span>
                                        )}
                                    </div>
                                </div>
                            </div>
                        </div>

                        {/* Event Type & Reminder */}
                        <div className="bg-white/80 backdrop-blur rounded-2xl p-6 shadow-md border border-amber-100">
                            <div className="flex items-start gap-4">
                                <div className="bg-gradient-to-br from-amber-400 to-amber-500 p-3 rounded-xl shadow-lg">
                                    <Bell className="w-6 h-6 text-white" />
                                </div>
                                <div className="flex-1">
                                    <h3 className="font-semibold text-blue-900 mb-2">Thông tin</h3>
                                    <p className="text-blue-700 font-medium">
                                        {getEventTypeLabel(event.eventType)}
                                    </p>
                                    {event.reminder && (
                                        <p className="text-blue-600 mt-1">
                                            <strong>Nhắc nhở:</strong> {event.reminder.daysBefore} ngày trước
                                        </p>
                                    )}
                                    <div className="mt-2">
                                        <span className={`inline-block px-3 py-1 rounded-full text-xs font-semibold ${
                                            event.isPublic
                                                ? 'bg-green-100 text-green-700'
                                                : 'bg-gray-100 text-gray-700'
                                        }`}>
                                            {event.isPublic ? 'Công khai' : 'Riêng tư'}
                                        </span>
                                        <span className={`inline-block px-3 py-1 rounded-full text-xs font-semibold ml-2 ${
                                            event.status === 'ACTIVE'
                                                ? 'bg-blue-100 text-blue-700'
                                                : event.status === 'COMPLETED'
                                                    ? 'bg-green-100 text-green-700'
                                                    : 'bg-red-100 text-red-700'
                                        }`}>
                                            {event.status === 'ACTIVE' && 'Đang hoạt động'}
                                            {event.status === 'COMPLETED' && 'Đã hoàn thành'}
                                            {event.status === 'CANCELLED' && 'Đã hủy'}
                                        </span>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>

                    {/* Location */}
                    {event.location && (
                        <div className="bg-white/80 backdrop-blur rounded-2xl p-6 shadow-md border border-blue-100 mt-6">
                            <div className="flex items-start gap-4">
                                <div className="bg-gradient-to-br from-green-500 to-green-600 p-3 rounded-xl shadow-lg">
                                    <FileText className="w-6 h-6 text-white" />
                                </div>
                                <div className="flex-1">
                                    <h3 className="font-semibold text-blue-900 mb-2">Địa điểm</h3>
                                    <p className="text-blue-700">{event.location}</p>
                                </div>
                            </div>
                        </div>
                    )}
                </div>

                {/* Description Card */}
                {event.description && (
                    <div className="bg-white rounded-3xl shadow-xl p-8 border border-blue-100">
                        <div className="flex items-center gap-3 mb-4">
                            <div className="bg-gradient-to-br from-blue-500 to-blue-600 p-3 rounded-xl shadow-lg">
                                <FileText className="w-6 h-6 text-white" />
                            </div>
                            <h3 className="text-xl font-bold text-blue-900">Mô tả</h3>
                        </div>
                        <div className="bg-gradient-to-br from-blue-50 to-amber-50 rounded-2xl p-6 border border-blue-100">
                            <p className="text-blue-900 leading-relaxed whitespace-pre-wrap">
                                {event.description}
                            </p>
                        </div>
                    </div>
                )}

                {/* Related Persons */}
                {event.relatedPersons && event.relatedPersons.length > 0 && (
                    <div className="bg-white rounded-3xl shadow-xl p-8 border border-blue-100 mt-6">
                        <h3 className="text-xl font-bold text-blue-900 mb-4">Người liên quan</h3>
                        <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                            {event.relatedPersons.map((person, index) => (
                                <div key={index} className="bg-gradient-to-br from-amber-50 to-blue-50 rounded-xl p-4 border border-amber-100">
                                    <p className="font-semibold text-blue-900">{person.name}</p>
                                </div>
                            ))}
                        </div>
                    </div>
                )}

                {/* Action Buttons */}
                <div className="mt-8 flex gap-4">
                    <button
                        onClick={handleEdit}
                        className="flex-1 bg-gradient-to-r from-blue-600 to-blue-700 text-white py-4 rounded-2xl font-semibold flex items-center justify-center gap-2 hover:shadow-xl transition-all duration-300 transform hover:scale-105"
                    >
                        <Edit2 className="w-5 h-5" />
                        Chỉnh sửa
                    </button>
                    <button
                        onClick={() => setShowDeleteModal(true)}
                        className="flex-1 bg-gradient-to-r from-red-600 to-red-700 text-white py-4 rounded-2xl font-semibold flex items-center justify-center gap-2 hover:shadow-xl transition-all duration-300 transform hover:scale-105"
                    >
                        <Trash2 className="w-5 h-5" />
                        Xóa sự kiện
                    </button>
                </div>
            </div>

            {/* Delete Confirmation Modal */}
            {showDeleteModal && (
                <div className="fixed inset-0 bg-black/50 backdrop-blur-sm flex items-center justify-center z-50 p-4">
                    <div className="bg-white rounded-3xl shadow-2xl max-w-md w-full p-8 transform animate-in">
                        <div className="text-center mb-6">
                            <div className="bg-red-100 w-16 h-16 rounded-full flex items-center justify-center mx-auto mb-4">
                                <Trash2 className="w-8 h-8 text-red-600" />
                            </div>
                            <h3 className="text-2xl font-bold text-blue-900 mb-2">Xóa sự kiện?</h3>
                            <p className="text-blue-600">
                                Bạn có chắc chắn muốn xóa sự kiện "{event.title}"? Hành động này không thể hoàn tác.
                            </p>
                        </div>
                        <div className="flex gap-4">
                            <button
                                onClick={() => setShowDeleteModal(false)}
                                className="flex-1 bg-gray-200 text-gray-700 py-3 rounded-xl font-semibold hover:bg-gray-300 transition-colors"
                            >
                                Hủy
                            </button>
                            <button
                                onClick={handleDelete}
                                className="flex-1 bg-gradient-to-r from-red-600 to-red-700 text-white py-3 rounded-xl font-semibold hover:shadow-lg transition-all duration-300"
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

export default EventDetailPage;