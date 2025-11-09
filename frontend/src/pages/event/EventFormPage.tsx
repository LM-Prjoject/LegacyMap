import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { ArrowLeft, Save, ChevronDown, Users, FileText, Lock, MapPin, Calendar, Bell } from 'lucide-react';
import { eventsApi } from '@/api/eventApi';
import { EventCreateRequest, EventType, CalendarType, RecurrenceRule } from '@/types/event';
import { useEventContext } from '@/contexts/EventContext';
import { formatInTimeZone } from 'date-fns-tz';
import Navbar from "@/components/layout/Navbar.tsx";

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

    const handleReminderChange = <K extends keyof NonNullable<EventCreateRequest["reminder"]>>(
        field: K,
        value: NonNullable<EventCreateRequest["reminder"]>[K]
    ) => {
        setFormData(prev => {
            const reminder = {
                ...(prev.reminder ?? {}),
                [field]: value,
            } as NonNullable<EventCreateRequest["reminder"]>;

            return {
                ...prev,
                reminder,
            };
        });
    };

    const reminderOptions = [
        { value: 0, label: 'Đúng giờ' },
        { value: 1, label: '1 ngày trước' },
        { value: 3, label: '3 ngày trước' },
        { value: 7, label: '1 tuần trước' }
    ];

    const methodOptions = [
        { value: 'notification', label: 'Thông báo' },
        { value: 'email', label: 'Email' },
        { value: 'both', label: 'Cả hai' }
    ];

    return (
        <div className="min-h-screen" style={{
            background: 'linear-gradient(135deg, #1a1f2e 0%, #2a3548 50%, #1f2937 100%)',
        }}>
            <Navbar />
            {/* Header */}
            <div className="max-w-5xl mx-auto px-4 sm:px-6 py-8 relative z-10">
                <div className="flex items-center justify-between">
                    <button
                        type="button"
                        onClick={() => navigate('/events')}
                        className="flex items-center gap-2 px-3 py-2 rounded-lg transition-all duration-200 hover:bg-white/5"
                        style={{ color: 'rgb(255, 216, 155)' }}
                    >
                        <ArrowLeft className="w-5 h-5 text-[rgb(255,216,155)] group-hover:-translate-x-1 transition-transform" />
                        <span className="hidden sm:inline font-medium">Quay lại</span>
                    </button>
                    <div className="text-center flex-1">
                        <h1 className="text-3xl sm:text-4xl font-bold bg-gradient-to-r from-[rgb(255,216,155)] via-white to-[rgb(255,216,155)] bg-clip-text text-transparent">
                            Tạo Sự Kiện Mới
                        </h1>
                    </div>
                    <div className="w-20 sm:w-24"></div>
                </div>
            </div>

            {error && (
                <div className="max-w-5xl mx-auto px-4 sm:px-6 mt-6">
                    <div className="p-4 rounded-2xl border animate-pulse" style={{
                        background: 'linear-gradient(135deg, rgba(239, 68, 68, 0.15) 0%, rgba(239, 68, 68, 0.05) 100%)',
                        borderColor: 'rgba(239, 68, 68, 0.4)',
                    }}>
                        <p className="text-red-300 text-center font-medium">{error}</p>
                    </div>
                </div>
            )}

            {/* Main form */}
            <div className="max-w-4xl mx-auto px-4 sm:px-6 py-8 pb-20">
                <form onSubmit={handleSubmit} className="space-y-6">
                    {/* Event Scope Selector */}
                    <div className="group relative rounded-3xl p-6 transition-all duration-300 hover:scale-[1.02] hover:shadow-2xl" style={{
                        background: 'linear-gradient(135deg, rgba(255, 216, 155, 0.1) 0%, rgba(42, 53, 72, 0.6) 100%)',
                        border: '1px solid rgba(255, 216, 155, 0.2)',
                        boxShadow: '0 4px 32px rgba(255, 216, 155, 0.1)'
                    }}>
                        <div className="flex items-center gap-3 mb-4">
                            <Users className="w-5 h-5 text-[rgb(255,216,155)]" />
                            <label className="text-lg font-bold text-[rgb(255,216,155)]">
                                Phạm vi sự kiện
                            </label>
                        </div>
                        <div className="flex gap-4">
                            <label className="flex items-center gap-2 cursor-pointer group">
                                <input
                                    type="radio"
                                    name="eventScope"
                                    value="family"
                                    checked={!isPersonalEvent}
                                    onChange={() => setIsPersonalEvent(false)}
                                    className="w-4 h-4"
                                    style={{ accentColor: 'rgb(255, 216, 155)' }}
                                />
                                <span className="text-white/90 group-hover:text-white transition-colors">Sự kiện gia đình</span>
                            </label>
                            <label className="flex items-center gap-2 cursor-pointer group">
                                <input
                                    type="radio"
                                    name="eventScope"
                                    value="personal"
                                    checked={isPersonalEvent}
                                    onChange={() => setIsPersonalEvent(true)}
                                    className="w-4 h-4"
                                    style={{ accentColor: 'rgb(255, 216, 155)' }}
                                />
                                <span className="text-white/90 group-hover:text-white transition-colors">Sự kiện cá nhân</span>
                            </label>
                        </div>
                    </div>

                    {/* Event Name */}
                    <div className="group rounded-3xl p-6 transition-all duration-300 hover:scale-[1.02]" style={{
                        background: 'linear-gradient(135deg, rgba(42, 53, 72, 0.8) 0%, rgba(42, 53, 72, 0.6) 100%)',
                        border: '1px solid rgba(255, 216, 155, 0.2)',
                        boxShadow: '0 8px 32px rgba(0, 0, 0, 0.3)'
                    }}>
                        <label className="block text-lg font-bold text-[rgb(255,216,155)] mb-3">
                            Tên sự kiện <span className="text-red-400">*</span>
                        </label>
                        <input
                            type="text"
                            value={formData.title}
                            onChange={(e) => handleInputChange('title', e.target.value)}
                            placeholder="Nhập tên sự kiện"
                            className="w-full px-3 py-3 rounded-xl focus:outline-none focus:ring-2 focus:ring-[rgb(255,216,155)] transition-all"
                            style={{
                                background: 'rgba(255, 255, 255, 0.05)',
                                border: '1px solid rgba(255, 216, 155, 0.2)',
                                color: 'white'
                            }}
                            required
                        />
                    </div>

                    {/* Event Type */}
                    <div className="group rounded-3xl p-6 transition-all duration-300 hover:scale-[1.02]" style={{
                        background: 'linear-gradient(135deg, rgba(42, 53, 72, 0.8) 0%, rgba(42, 53, 72, 0.6) 100%)',
                        border: '1px solid rgba(255, 216, 155, 0.2)',
                        boxShadow: '0 8px 32px rgba(0, 0, 0, 0.3)'
                    }}>
                        <label className="block text-lg font-bold text-[rgb(255,216,155)] mb-3">
                            Loại sự kiện
                        </label>
                        <div className="relative">
                            <select
                                value={formData.eventType}
                                onChange={(e) => handleInputChange('eventType', e.target.value as EventType)}
                                className="w-full px-3 py-3 rounded-xl focus:outline-none focus:ring-2 focus:ring-[rgb(255,216,155)] appearance-none cursor-pointer transition-all"
                                style={{
                                    background: 'rgba(255, 255, 255, 0.05)',
                                    border: '1px solid rgba(255, 216, 155, 0.2)',
                                    color: 'white'
                                }}
                            >
                                {Object.values(EventType).map(type => (
                                    <option key={type} value={type} className="bg-[#2a3548]">
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
                            <ChevronDown className="absolute right-4 top-1/2 -translate-y-1/2 w-5 h-5 text-[rgb(255,216,155)] pointer-events-none" />
                        </div>
                    </div>


                    {/* Location */}
                    <div className="group rounded-3xl p-6 transition-all duration-300 hover:scale-[1.02]" style={{
                        background: 'linear-gradient(135deg, rgba(42, 53, 72, 0.8) 0%, rgba(42, 53, 72, 0.6) 100%)',
                        border: '1px solid rgba(255, 216, 155, 0.2)',
                        boxShadow: '0 8px 32px rgba(0, 0, 0, 0.3)'
                    }}>
                        <div className="flex items-center gap-3 mb-4">
                            <MapPin className="w-5 h-5 text-[rgb(255,216,155)]" />
                            <label className="text-lg font-bold text-[rgb(255,216,155)]">
                                Địa điểm
                            </label>
                        </div>
                        <input
                            type="text"
                            value={formData.location || ''}
                            onChange={(e) => handleInputChange('location', e.target.value)}
                            placeholder="Nhập địa điểm sự kiện"
                            className="w-full px-3 py-3 rounded-xl focus:outline-none focus:ring-2 focus:ring-[rgb(255,216,155)] transition-all"
                            style={{
                                background: 'rgba(255, 255, 255, 0.05)',
                                border: '1px solid rgba(255, 216, 155, 0.2)',
                                color: 'white'
                            }}
                        />
                    </div>

                    {/* Time Section */}
                    <div className="group rounded-3xl p-6 transition-all duration-300 hover:scale-[1.02]" style={{
                        background: 'linear-gradient(135deg, rgba(42, 53, 72, 0.8) 0%, rgba(42, 53, 72, 0.6) 100%)',
                        border: '1px solid rgba(255, 216, 155, 0.2)',
                        boxShadow: '0 8px 32px rgba(0, 0, 0, 0.3)'
                    }}>
                        <div className="flex items-center gap-3 mb-4">
                            <Calendar className="w-5 h-5 text-[rgb(255,216,155)]" />
                            <label className="text-lg font-bold text-[rgb(255,216,155)]">
                                Thời gian
                            </label>
                        </div>
                        <div className="space-y-4">
                            {/* Start Date & Time */}
                            <div>
                                <label className="block text-s font-medium text-white/70 mb-2">Thời gian bắt đầu</label>
                                <input
                                    type="datetime-local"
                                    value={formData.startDate}
                                    onChange={(e) => handleInputChange('startDate', e.target.value)}
                                    className="w-full px-4 py-3 rounded-xl focus:outline-none focus:ring-2 focus:ring-[rgb(255,216,155)] transition-all"
                                    style={{
                                        background: 'rgba(255, 255, 255, 0.05)',
                                        border: '1px solid rgba(255, 216, 155, 0.3)',
                                        color: 'white'
                                    }}
                                    required
                                />
                            </div>

                            {/* End Date & Time */}
                            <div>
                                <label className="block text-s font-medium text-white/70 mb-2">Thời gian kết thúc (tùy chọn)</label>
                                <input
                                    type="datetime-local"
                                    value={formData.endDate}
                                    onChange={(e) => handleInputChange('endDate', e.target.value)}
                                    className="w-full px-4 py-3 rounded-xl focus:outline-none focus:ring-2 focus:ring-[rgb(255,216,155)] transition-all"
                                    style={{
                                        background: 'rgba(255, 255, 255, 0.05)',
                                        border: '1px solid rgba(255, 216, 155, 0.3)',
                                        color: 'white'
                                    }}
                                />
                            </div>
                            {/* Full Day Option */}
                            <div className="mt-4">
                                <label className="flex items-center gap-2 mt-4 cursor-pointer group">
                                    <input
                                        type="checkbox"
                                        checked={formData.isFullDay || false}
                                        onChange={(e) => handleInputChange('isFullDay', e.target.checked)}
                                        className="w-4 h-4 rounded"
                                        style={{ accentColor: 'rgb(255, 216, 155)' }}
                                    />
                                    <span className="text-white/90 group-hover:text-white transition-colors">Sự kiện cả ngày</span>
                                </label>
                            </div>
                        </div>

                        {/* Calendar Type */}
                        <div className="mt-4">
                            <label className="block text-m font-semibold mb-3" style={{ color: 'rgb(255, 216, 155)' }}>Loại lịch</label>
                            <div className="flex gap-4">
                                <label className="flex items-center gap-2 cursor-pointer group">
                                    <input
                                        type="radio"
                                        name="calendarType"
                                        value={CalendarType.SOLAR}
                                        checked={formData.calendarType === CalendarType.SOLAR}
                                        onChange={(e) => handleInputChange('calendarType', e.target.value as CalendarType)}
                                        className="w-4 h-4"
                                        style={{ accentColor: 'rgb(255, 216, 155)' }}
                                    />
                                    <span className="text-white/90 group-hover:text-white transition-colors">Dương lịch</span>
                                </label>
                                <label className="flex items-center gap-2 cursor-pointer group">
                                    <input
                                        type="radio"
                                        name="calendarType"
                                        value={CalendarType.LUNAR}
                                        checked={formData.calendarType === CalendarType.LUNAR}
                                        onChange={(e) => handleInputChange('calendarType', e.target.value as CalendarType)}
                                        className="w-4 h-4"
                                        style={{ accentColor: 'rgb(255, 216, 155)' }}
                                    />
                                    <span className="text-white/90 group-hover:text-white transition-colors">Âm lịch</span>
                                </label>
                            </div>
                        </div>
                    </div>

                    {/* Reminder Section */}
                    <div className="group rounded-3xl p-6 transition-all duration-300 hover:scale-[1.02]" style={{
                        background: 'linear-gradient(135deg, rgba(42, 53, 72, 0.8) 0%, rgba(42, 53, 72, 0.6) 100%)',
                        border: '1px solid rgba(255, 216, 155, 0.2)',
                        boxShadow: '0 8px 32px rgba(0, 0, 0, 0.3)'
                    }}>
                        <div className="flex items-center gap-2 mb-4">
                            <Bell className="w-5 h-5 text-[rgb(255,216,155)]" />
                            <label className="text-lg font-bold text-[rgb(255,216,155)]">
                                Thông báo nhắc nhở
                            </label>
                        </div>

                        <div className="space-y-4">
                            {/* Reminder Time */}
                            <div>
                                <label className="block text-s font-medium text-white/70 mb-2">Thời gian nhắc nhở</label>
                                <div className="relative">
                                    <select
                                        value={String(formData.reminder?.daysBefore ?? 3)}
                                        onChange={(e) => handleReminderChange('daysBefore', Number(e.target.value))}
                                        className="w-full px-4 py-3 rounded-xl focus:outline-none focus:ring-2 focus:ring-[rgb(255,216,155)] appearance-none cursor-pointer transition-all"
                                        style={{
                                            background: 'rgba(255, 255, 255, 0.05)',
                                            border: '1px solid rgba(255, 216, 155, 0.3)',
                                            color: 'white'
                                        }}
                                    >
                                        {reminderOptions.map(option => (
                                            <option key={option.value} value={String(option.value)} className="bg-[#2a3548]">
                                                {option.label}
                                            </option>
                                        ))}
                                    </select>
                                    <ChevronDown className="absolute right-3 top-1/2 -translate-y-1/2 w-4 h-4 text-[rgb(255,216,155)] pointer-events-none" />
                                </div>
                            </div>

                            {/* Reminder Method */}
                            <div>
                                <label className="block text-s font-medium text-white/70 mb-2">Phương thức</label>
                                <div className="relative">
                                    <select
                                        value={formData.reminder?.methods?.[0] || 'notification'}
                                        onChange={(e) => handleReminderChange('methods', [e.target.value])}
                                        className="w-full px-4 py-3 rounded-xl focus:outline-none focus:ring-2 focus:ring-[rgb(255,216,155)] appearance-none cursor-pointer transition-all"
                                        style={{
                                            background: 'rgba(255, 255, 255, 0.05)',
                                            border: '1px solid rgba(255, 216, 155, 0.3)',
                                            color: 'white'
                                        }}
                                    >
                                        {methodOptions.map(option => (
                                            <option key={option.value} value={option.value} className="bg-[#2a3548]">
                                                {option.label}
                                            </option>
                                        ))}
                                    </select>
                                    <ChevronDown className="absolute right-3 top-1/2 -translate-y-1/2 w-4 h-4 text-[rgb(255,216,155)] pointer-events-none" />
                                </div>
                            </div>
                        </div>
                    </div>

                    {/* Description */}
                    <div className="group rounded-3xl p-6 transition-all duration-300 hover:scale-[1.01]" style={{
                        background: 'linear-gradient(135deg, rgba(42, 53, 72, 0.8) 0%, rgba(42, 53, 72, 0.6) 100%)',
                        border: '1px solid rgba(255, 216, 155, 0.2)',
                        boxShadow: '0 8px 32px rgba(0, 0, 0, 0.3)'
                    }}>
                        <div className="flex items-center gap-2 mb-4">
                            <FileText className="w-5 h-5 text-[rgb(255,216,155)]" />
                            <label className="text-lg font-bold text-[rgb(255,216,155)]">
                                Mô tả
                            </label>
                        </div>
                        <textarea
                            value={formData.description}
                            onChange={(e) => handleInputChange('description', e.target.value)}
                            placeholder="Nhập mô tả chi tiết về sự kiện..."
                            rows={4}
                            className="w-full px-3 py-3 rounded-xl focus:outline-none focus:ring-2 focus:ring-[rgb(255,216,155)] resize-none transition-all"
                            style={{
                                background: 'rgba(255, 255, 255, 0.05)',
                                border: '1px solid rgba(255, 216, 155, 0.2)',
                                color: 'white'
                            }}
                        />
                    </div>

                    {/* Privacy */}
                    <div className="group rounded-3xl p-6 transition-all duration-300 hover:scale-[1.01]" style={{
                        background: 'linear-gradient(135deg, rgba(42, 53, 72, 0.8) 0%, rgba(42, 53, 72, 0.6) 100%)',
                        border: '1px solid rgba(255, 216, 155, 0.2)',
                        boxShadow: '0 8px 32px rgba(0, 0, 0, 0.3)'
                    }}>
                        <div className="flex items-center gap-2 mb-4">
                            <Lock className="w-5 h-5 text-[rgb(255,216,155)]" />
                            <label className="text-lg font-bold text-[rgb(255,216,155)]">
                                Quyền riêng tư
                            </label>
                        </div>
                        <div className="relative">
                            <select
                                value={formData.isPublic ? 'public' : 'private'}
                                onChange={(e) => handleInputChange('isPublic', e.target.value === 'public')}
                                className="w-full px-3 py-3 rounded-xl focus:outline-none focus:ring-2 focus:ring-[rgb(255,216,155)] appearance-none cursor-pointer transition-all"
                                style={{
                                    background: 'rgba(255, 255, 255, 0.05)',
                                    border: '1px solid rgba(255, 216, 155, 0.2)',
                                    color: 'white'
                                }}
                            >
                                <option value="public" className="bg-[#2a3548]">Công khai</option>
                                <option value="private" className="bg-[#2a3548]">Riêng tư</option>
                            </select>
                            <ChevronDown className="absolute right-4 top-1/2 -translate-y-1/2 w-5 h-5 text-[rgb(255,216,155)] pointer-events-none" />
                        </div>
                    </div>

                    {/* Submit Button */}
                    <div className="flex justify-end">
                        <button
                            type="submit"
                            disabled={loading}
                            className="px-6 py-2.5 rounded-lg font-semibold text-sm flex items-center justify-center gap-2 transition-all duration-300 hover:shadow-lg disabled:opacity-50 disabled:cursor-not-allowed group"
                            style={{
                                background: 'linear-gradient(135deg, rgb(255, 216, 155) 0%, rgb(255, 196, 115) 100%)',
                                color: '#1a1f2e',
                                boxShadow: '0 4px 12px rgba(255, 216, 155, 0.3)',
                            }}
                        >
                            <Save className="w-4 h-4" />
                            <span>{loading ? 'Đang tạo...' : 'Lưu'}</span>
                        </button>
                    </div>
                </form>
            </div>
        </div>
    );
};

export default EventFormPage;