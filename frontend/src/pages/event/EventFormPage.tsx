import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { ArrowLeft, Save, ChevronDown, Calendar as CalendarIcon } from 'lucide-react';
import { eventsApi } from '@/api/eventApi';
import { EventCreateRequest, EventType, CalendarType, RecurrenceRule } from '@/types/event';
import { useEventContext } from '@/contexts/EventContext';
import { formatInTimeZone } from 'date-fns-tz';

const EventFormPage: React.FC = () => {
    const navigate = useNavigate();
    const {triggerEventsUpdate} = useEventContext();
    const [formData, setFormData] = useState<EventCreateRequest>({
        title: '',
        description: '',
        eventType: EventType.OTHER,
        startDate: new Date().toISOString().slice(0, 16),
        endDate: '',
        isFullDay: false,
        calendarType: CalendarType.SOLAR,
        isRecurring: false,
        recurrenceRule: RecurrenceRule.NONE,
        relatedPersons: [],
        location: '',
        locationCoordinates: {},
        reminder: {
            daysBefore: 3,
            methods: ['notification'] as ('notification' | 'email')[]
        },
        isPublic: true
    });

    const [loading, setLoading] = useState(false);
    const [error, setError] = useState<string | null>(null);
    const [isPersonalEvent, setIsPersonalEvent] = useState(false);

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        setLoading(true);
        setError(null);

        try {
            const vnZone = 'Asia/Ho_Chi_Minh';
            const startIso = formatInTimeZone(new Date(formData.startDate), vnZone, "yyyy-MM-dd'T'HH:mm:ssXXX");
            const endIso = formData.endDate
                ? formatInTimeZone(new Date(formData.endDate), vnZone, "yyyy-MM-dd'T'HH:mm:ssXXX")
                : undefined;

            const request: EventCreateRequest = {
                ...formData,
                startDate: startIso,
                endDate: endIso,
            };

            const userData = localStorage.getItem('user');
            let familyTreeId: string | null = null;

            if (userData) {
                const user = JSON.parse(userData);
                familyTreeId = user.familyTreeId || user.family_tree_id || user.familyTree?.id || null;
            }

            if (!isPersonalEvent && !familyTreeId) {
                throw new Error('Family tree ID not found. Please log in again or create a family tree first.');
            }

            await eventsApi.createEvent(request, isPersonalEvent ? undefined : familyTreeId!);

            triggerEventsUpdate();
            navigate('/events');
        } catch (err: any) {
            setError(err.response?.data?.message || err.message || 'Failed to create event');
        } finally {
            setLoading(false);
        }
    };

    const handleInputChange = <K extends keyof EventCreateRequest>(field: K, value: EventCreateRequest[K]) => {
        setFormData(prev => ({
            ...prev,
            [field]: value
        }));
    };

    const handleReminderChange = (field: 'daysBefore' | 'methods', value: number | string[]) => {
        setFormData(prev => ({
            ...prev,
            reminder: {
                ...prev.reminder!,
                [field]: field === 'daysBefore' ? value : value as ('notification' | 'email')[]
            }
        }));
    };

    const reminderOptions = [
        { value: 0, label: 'Đúng giờ' },
        { value: 1, label: '1 ngày trước' },
        { value: 3, label: '3 ngày trước' },
        { value: 7, label: '1 tuần trước' },
        { value: 14, label: '2 tuần trước' },
        { value: 30, label: '1 tháng trước' }
    ];

    const methodOptions = [
        { value: 'notification', label: 'Thông báo' },
        { value: 'email', label: 'Email' },
        { value: 'both', label: 'Cả hai' }
    ];

    return (
        <div className="min-h-screen bg-gradient-to-br from-amber-50 via-blue-50 to-amber-100">
            {/* Header */}
            <div className="bg-white border-b border-blue-100 sticky top-0 z-10 shadow-sm">
                <div className="max-w-3xl mx-auto px-6 py-4">
                    <div className="flex justify-between items-center">
                        <button
                            type="button"
                            onClick={() => navigate('/events')}
                            className="p-2 hover:bg-blue-50 rounded-full transition-colors"
                        >
                            <ArrowLeft className="w-6 h-6 text-blue-900" />
                        </button>
                        <h1 className="text-xl font-bold text-blue-900">Tạo sự kiện mới</h1>
                        <button
                            type="submit"
                            onClick={handleSubmit}
                            disabled={loading}
                            className="bg-gradient-to-r from-red-600 to-red-700 text-white px-6 py-2 rounded-full flex items-center gap-2 hover:shadow-lg transition-all duration-300 disabled:opacity-50"
                        >
                            <Save className="w-4 h-4" />
                            {loading ? 'Đang tạo...' : 'Lưu'}
                        </button>
                    </div>
                </div>
            </div>

            {error && (
                <div className="max-w-3xl mx-auto px-6 py-4">
                    <div className="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded">
                        {error}
                    </div>
                </div>
            )}

            {/* Main form */}
            <div className="max-w-3xl mx-auto px-6 py-8">
                <form onSubmit={handleSubmit} className="space-y-6">
                    {/* Event Type Selector */}
                    <div className="bg-white rounded-2xl shadow-lg p-6">
                        <label className="block text-sm font-semibold text-blue-900 mb-3">
                            Loại sự kiện
                        </label>
                        <div className="flex gap-4">
                            <label className="flex items-center gap-2 cursor-pointer">
                                <input
                                    type="radio"
                                    name="eventScope"
                                    value="family"
                                    checked={!isPersonalEvent}
                                    onChange={() => setIsPersonalEvent(false)}
                                    className="text-blue-600 focus:ring-blue-500"
                                />
                                <span className="text-blue-900">Sự kiện của cây gia phả</span>
                            </label>
                            <label className="flex items-center gap-2 cursor-pointer">
                                <input
                                    type="radio"
                                    name="eventScope"
                                    value="personal"
                                    checked={isPersonalEvent}
                                    onChange={() => setIsPersonalEvent(true)}
                                    className="text-blue-600 focus:ring-blue-500"
                                />
                                <span className="text-blue-900">Sự kiện cá nhân</span>
                            </label>
                        </div>
                    </div>
                    {/* Event Name */}
                    <div className="bg-white rounded-2xl shadow-lg p-6">
                        <label className="block text-sm font-semibold text-blue-900 mb-2">
                            Tên sự kiện<span className="text-red-500">*</span>
                        </label>
                        <input
                            type="text"
                            value={formData.title}
                            onChange={(e) => handleInputChange('title', e.target.value)}
                            placeholder="Nhập tên sự kiện"
                            className="w-full px-4 py-3 border border-blue-200 rounded-xl focus:outline-none focus:ring-2 focus:ring-blue-400 focus:border-transparent bg-blue-50/50"
                            required
                        />
                    </div>

                    {/* Event Type */}
                    <div className="bg-white rounded-2xl shadow-lg p-6">
                        <label className="block text-sm font-semibold text-blue-900 mb-2">
                            Loại sự kiện
                        </label>
                        <select
                            value={formData.eventType}
                            onChange={(e) => handleInputChange('eventType', e.target.value as EventType)}
                            className="w-full px-4 py-3 border border-blue-200 rounded-xl focus:outline-none focus:ring-2 focus:ring-blue-400 bg-blue-50/50 appearance-none cursor-pointer"
                        >
                            {Object.values(EventType).map(type => (
                                <option key={type} value={type}>
                                    {type === EventType.BIRTHDAY && 'Sinh nhật'}
                                    {type === EventType.DEATH_ANNIVERSARY && 'Ngày giỗ'}
                                    {type === EventType.WEDDING_ANNIVERSARY && 'Ngày cưới'}
                                    {type === EventType.WEDDING && 'Đám cưới'}
                                    {type === EventType.FUNERAL && 'Tang lễ'}
                                    {type === EventType.FAMILY_REUNION && 'Họp mặt gia đình'}
                                    {type === EventType.CEREMONY && 'Nghi lễ'}
                                    {type === EventType.OTHER && 'Khác'}
                                </option>
                            ))}
                        </select>
                    </div>

                    {/* Time Section */}
                    <div className="bg-white rounded-2xl shadow-lg p-6">
                        <label className="block text-sm font-semibold text-blue-900 mb-3">Thời gian</label>
                        <div className="grid grid-cols-2 gap-4">
                            {/* Start Date & Time */}
                            <div className="space-y-2">
                                <label className="block text-sm font-medium text-blue-700">Thời gian bắt đầu</label>
                                <div className="relative">
                                    <CalendarIcon className="absolute left-3 top-1/2 -translate-y-1/2 w-5 h-5 text-blue-400" />
                                    <input
                                        type="datetime-local"
                                        value={formData.startDate}
                                        onChange={(e) => handleInputChange('startDate', e.target.value)}
                                        className="w-full pl-10 pr-4 py-3 border border-blue-200 rounded-xl focus:outline-none focus:ring-2 focus:ring-blue-400 bg-blue-50/50"
                                        required
                                    />
                                </div>
                            </div>

                            {/* End Date & Time */}
                            <div className="space-y-2">
                                <label className="block text-sm font-medium text-blue-700">Thời gian kết thúc (tùy chọn)</label>
                                <div className="relative">
                                    <CalendarIcon className="absolute left-3 top-1/2 -translate-y-1/2 w-5 h-5 text-blue-400" />
                                    <input
                                        type="datetime-local"
                                        value={formData.endDate}
                                        onChange={(e) => handleInputChange('endDate', e.target.value)}
                                        className="w-full pl-10 pr-4 py-3 border border-blue-200 rounded-xl focus:outline-none focus:ring-2 focus:ring-blue-400 bg-blue-50/50"
                                    />
                                </div>
                            </div>
                        </div>

                        {/* Calendar Type */}
                        <div className="mt-4">
                            <label className="block text-sm font-medium text-blue-700 mb-2">Loại lịch</label>
                            <div className="flex gap-4">
                                <label className="flex items-center gap-2 cursor-pointer">
                                    <input
                                        type="radio"
                                        name="calendarType"
                                        value={CalendarType.SOLAR}
                                        checked={formData.calendarType === CalendarType.SOLAR}
                                        onChange={(e) => handleInputChange('calendarType', e.target.value as CalendarType)}
                                        className="text-blue-600 focus:ring-blue-500"
                                    />
                                    <span className="text-blue-900">Dương lịch</span>
                                </label>
                                <label className="flex items-center gap-2 cursor-pointer">
                                    <input
                                        type="radio"
                                        name="calendarType"
                                        value={CalendarType.LUNAR}
                                        checked={formData.calendarType === CalendarType.LUNAR}
                                        onChange={(e) => handleInputChange('calendarType', e.target.value as CalendarType)}
                                        className="text-blue-600 focus:ring-blue-500"
                                    />
                                    <span className="text-blue-900">Âm lịch</span>
                                </label>
                            </div>
                        </div>

                        {/* Full Day Option */}
                        <div className="mt-4">
                            <label className="flex items-center gap-2 cursor-pointer">
                                <input
                                    type="checkbox"
                                    checked={formData.isFullDay || false}
                                    onChange={(e) => handleInputChange('isFullDay', e.target.checked)}
                                    className="rounded text-blue-600 focus:ring-blue-500"
                                />
                                <span className="text-blue-900">Sự kiện cả ngày</span>
                            </label>
                        </div>
                    </div>

                    {/* Reminder Section */}
                    <div className="bg-white rounded-2xl shadow-lg p-6">
                        <label className="block text-sm font-semibold text-blue-900 mb-3">
                            Gửi thông báo nhắc nhở
                        </label>

                        <div className="grid grid-cols-2 gap-4">
                            {/* Reminder Time */}
                            <div>
                                <label className="block text-sm font-medium text-blue-700 mb-2">Thời gian nhắc nhở</label>
                                <div className="relative">
                                    <select
                                        value={formData.reminder?.daysBefore || 3}
                                        onChange={(e) => handleReminderChange('daysBefore', parseInt(e.target.value))}
                                        className="w-full px-4 py-3 border border-blue-200 rounded-xl focus:outline-none focus:ring-2 focus:ring-blue-400 bg-blue-50/50 appearance-none cursor-pointer"
                                    >
                                        {reminderOptions.map(option => (
                                            <option key={option.value} value={option.value}>
                                                {option.label}
                                            </option>
                                        ))}
                                    </select>
                                    <ChevronDown className="absolute right-3 top-1/2 -translate-y-1/2 w-5 h-5 text-blue-400 pointer-events-none" />
                                </div>
                            </div>

                            {/* Reminder Method */}
                            <div>
                                <label className="block text-sm font-medium text-blue-700 mb-2">Phương thức</label>
                                <div className="relative">
                                    <select
                                        value={formData.reminder?.methods?.[0] || 'notification'}
                                        onChange={(e) => handleReminderChange('methods', [e.target.value])}
                                        className="w-full px-4 py-3 border border-blue-200 rounded-xl focus:outline-none focus:ring-2 focus:ring-blue-400 bg-blue-50/50 appearance-none cursor-pointer"
                                    >
                                        {methodOptions.map(option => (
                                            <option key={option.value} value={option.value}>
                                                {option.label}
                                            </option>
                                        ))}
                                    </select>
                                    <ChevronDown className="absolute right-3 top-1/2 -translate-y-1/2 w-5 h-5 text-blue-400 pointer-events-none" />
                                </div>
                            </div>
                        </div>
                    </div>

                    {/* Description */}
                    <div className="bg-white rounded-2xl shadow-lg p-6">
                        <label className="block text-sm font-semibold text-blue-900 mb-2">Mô tả</label>
                        <textarea
                            value={formData.description}
                            onChange={(e) => handleInputChange('description', e.target.value)}
                            placeholder="Nhập mô tả sự kiện"
                            rows={4}
                            className="w-full px-4 py-3 border border-blue-200 rounded-xl focus:outline-none focus:ring-2 focus:ring-blue-400 focus:border-transparent bg-blue-50/50 resize-none"
                        />
                    </div>

                    {/* Location */}
                    <div className="bg-white rounded-2xl shadow-lg p-6">
                        <label className="block text-sm font-semibold text-blue-900 mb-2">Địa điểm</label>
                        <input
                            type="text"
                            value={formData.location || ''}
                            onChange={(e) => handleInputChange('location', e.target.value)}
                            placeholder="Nhập địa điểm sự kiện"
                            className="w-full px-4 py-3 border border-blue-200 rounded-xl focus:outline-none focus:ring-2 focus:ring-blue-400 focus:border-transparent bg-blue-50/50"
                        />
                    </div>

                    {/* Privacy */}
                    <div className="bg-white rounded-2xl shadow-lg p-6">
                        <label className="block text-sm font-semibold text-blue-900 mb-2">Quyền riêng tư</label>
                        <select
                            value={formData.isPublic ? 'public' : 'private'}
                            onChange={(e) => handleInputChange('isPublic', e.target.value === 'public')}
                            className="w-full px-4 py-3 border border-blue-200 rounded-xl focus:outline-none focus:ring-2 focus:ring-blue-400 bg-blue-50/50 appearance-none cursor-pointer"
                        >
                            <option value="public">Công khai</option>
                            <option value="private">Riêng tư</option>
                        </select>
                    </div>
                </form>
            </div>
        </div>
    );
};

export default EventFormPage;