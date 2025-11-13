import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import { ArrowLeft, Save, ChevronDown, Users, FileText, Lock, MapPin, Calendar, Bell, X, ContactRound, ListTree } from 'lucide-react';
import { eventsApi } from '@/api/eventApi';
import api, {FamilyTree, Person} from '@/api/trees';
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
    const [familyTrees, setFamilyTrees] = useState<FamilyTree[]>([]);
    const [selectedTreeId, setSelectedTreeId] = useState<string>('');
    const [members, setMembers] = useState<Person[]>([]);
    const [searchMember, setSearchMember] = useState('');
    const [showMemberDropdown, setShowMemberDropdown] = useState(false);
    const [selectAll, setSelectAll] = useState(false);

    useEffect(() => {
        if (!isPersonalEvent) {
            const loadTrees = async () => {
                try {
                    const userData = localStorage.getItem('user');
                    if (!userData) throw new Error('Vui lòng đăng nhập lại');
                    const user = JSON.parse(userData);
                    const trees = await api.listTrees(user.id);
                    setFamilyTrees(trees);
                    if (trees.length > 0) {
                        setSelectedTreeId(trees[0].id);
                    }
                } catch (err) {
                    setError('Không tải được cây gia phả');
                }
            };
            loadTrees();
        } else {
            setFamilyTrees([]);
            setSelectedTreeId('');
        }
    }, [isPersonalEvent]);

    useEffect(() => {
        if (selectedTreeId && !isPersonalEvent) {
            const loadMembers = async () => {
                try {
                    const userData = localStorage.getItem('user');
                    const user = JSON.parse(userData!);
                    const list = await api.listMembers(user.id, selectedTreeId);
                    setMembers(list);
                } catch (err) {
                    console.error('Load members failed', err);
                }
            };
            loadMembers();
        } else {
            setMembers([]);
        }
    }, [selectedTreeId, isPersonalEvent]);

    useEffect(() => {
        if (selectAll && members.length > 0) {
            const all = members.map(m => ({ id: m.id, name: m.fullName }));
            handleInputChange('relatedPersons', all);
        } else if (!selectAll && formData.relatedPersons?.length === members.length) {
            handleInputChange('relatedPersons', []);
        }
    }, [selectAll, members]);

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

            if (!isPersonalEvent) {
                if (!selectedTreeId) {
                    throw new Error('Vui lòng chọn cây gia phả');
                }
                await eventsApi.createEvent(request, selectedTreeId);
            } else {
                await eventsApi.createEvent(request);
            }

            triggerEventsUpdate();
            navigate('/events');
        } catch (err: any) {
            setError(err.response?.data?.message || err.message || 'Tạo sự kiện thất bại');
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

                    {/* Chọn cây gia phả */}
                    {!isPersonalEvent && (
                        <div className="group rounded-3xl p-6 transition-all duration-300 hover:scale-[1.02]" style={{
                            background: 'linear-gradient(135deg, rgba(42, 53, 72, 0.8) 0%, rgba(42, 53, 72, 0.6) 100%)',
                            border: '1px solid rgba(255, 216, 155, 0.2)',
                        }}>
                            <div className="flex items-center gap-3 mb-4">
                                <ListTree className="w-5 h-5 text-[rgb(255,216,155)]" />
                                <label className="text-lg font-bold text-[rgb(255,216,155)]">
                                    Chọn cây gia phả <span className="text-red-400">*</span>
                                </label>
                            </div>
                            {familyTrees.length === 0 ? (
                                <p className="text-yellow-300">Bạn chưa có cây gia phả nào. Vui lòng tạo trước.</p>
                            ) : (
                                <div className="relative">
                                    <select
                                        value={selectedTreeId}
                                        onChange={(e) => setSelectedTreeId(e.target.value)}
                                        className="w-full px-3 py-3 rounded-xl focus:outline-none focus:ring-2 focus:ring-[rgb(255,216,155)] appearance-none cursor-pointer transition-all"
                                        style={{
                                            background: 'rgba(255, 255, 255, 0.05)',
                                            border: '1px solid rgba(255, 216, 155, 0.2)',
                                            color: 'white'
                                        }}
                                    >
                                        {familyTrees.map(tree => (
                                            <option key={tree.id} value={tree.id} className="bg-[#2a3548]">
                                                {tree.name}
                                            </option>
                                        ))}
                                    </select>
                                    <ChevronDown className="absolute right-4 top-1/2 -translate-y-1/2 w-5 h-5 text-[rgb(255,216,155)] pointer-events-none" />
                                </div>
                            )}
                        </div>
                    )}

                    {/* Người liên quan */}
                    {!isPersonalEvent && selectedTreeId && members.length > 0 && (
                        <div className="group rounded-3xl p-6 transition-all duration-300 hover:scale-[1.02]" style={{
                            background: 'linear-gradient(135deg, rgba(42, 53, 72, 0.8) 0%, rgba(42, 53, 72, 0.6) 100%)',
                            border: '1px solid rgba(255, 216, 155, 0.2)',
                        }}>
                            <div className="flex items-center gap-3 mb-4">
                                <ContactRound className="w-5 h-5 text-[rgb(255,216,155)]" />
                                <label className="text-lg font-bold text-[rgb(255,216,155)]">
                                    Người liên quan
                                </label>
                            </div>

                            <div className="relative mb-4">
                                <input
                                    type="text"
                                    placeholder="Tìm và chọn người..."
                                    value={searchMember}
                                    onChange={(e) => setSearchMember(e.target.value)}
                                    onFocus={() => setShowMemberDropdown(true)}
                                    className="w-full px-3 py-3 rounded-xl focus:outline-none focus:ring-2 focus:ring-[rgb(255,216,155)]"
                                    style={{
                                        background: 'rgba(255, 255, 255, 0.05)',
                                        border: '1px solid rgba(255, 216, 155, 0.2)',
                                        color: 'white'
                                    }}
                                />

                                {showMemberDropdown && (
                                    <div className="absolute top-full left-0 right-0 mt-1 bg-[#2a3548] border border-[rgba(255,216,155,0.3)] rounded-xl shadow-xl z-10 max-h-60 overflow-y-auto">
                                        {members
                                            .filter(m => m.fullName.toLowerCase().includes(searchMember.toLowerCase()))
                                            .map(member => {
                                                const isSelected = formData.relatedPersons?.some(p => p.id === member.id);
                                                return (
                                                    <div
                                                        key={member.id}
                                                        onClick={() => {
                                                            if (!isSelected) {
                                                                handleInputChange('relatedPersons', [
                                                                    ...(formData.relatedPersons || []),
                                                                    { id: member.id, name: member.fullName }
                                                                ]);
                                                            }
                                                            setSearchMember('');
                                                            setShowMemberDropdown(false);
                                                        }}
                                                        className="px-4 py-3 hover:bg-[rgba(255,216,155,0.1)] cursor-pointer flex items-center justify-between"
                                                    >
                                                        <span className="text-white">{member.fullName}</span>
                                                        {isSelected && <span className="text-[rgb(255,216,155)] text-xs">Đã chọn</span>}
                                                    </div>
                                                );
                                            })}
                                    </div>
                                )}
                            </div>

                            <label className="flex items-center gap-2 cursor-pointer">
                                <input
                                    type="checkbox"
                                    checked={selectAll}
                                    onChange={e => setSelectAll(e.target.checked)}
                                    className="w-4 h-4"
                                    style={{ accentColor: 'rgb(255, 216, 155)' }}
                                />
                                <span className="text-white/90">Chọn tất cả thành viên trong cây</span>
                            </label>

                            {formData.relatedPersons && formData.relatedPersons.length > 0 && (
                                <div className="mt-3 flex flex-wrap gap-2">
                                    {formData.relatedPersons.map((p, i) => (
                                        <div key={i} className="px-3 py-1 rounded-full text-sm flex items-center gap-2"
                                             style={{ background: 'rgba(255,216,155,0.15)', color: 'rgb(255,216,155)' }}>
                                            {p.name}
                                            <button type="button" onClick={() => {
                                                const newList = formData.relatedPersons!.filter((_, idx) => idx !== i);
                                                handleInputChange('relatedPersons', newList);
                                                if (newList.length < members.length) setSelectAll(false);
                                            }}>
                                                <X className="w-3 h-3" />
                                            </button>
                                        </div>
                                    ))}
                                </div>
                            )}
                        </div>
                    )}

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

                    {/* Time Section*/}
                    <div className="group rounded-3xl p-6 transition-all duration-300 hover:scale-[1.02]" style={{
                        background: 'linear-gradient(135deg, rgba(42, 53, 72, 0.8) 0%, rgba(42, 53, 72, 0.6) 100%)',
                        border: '1px solid rgba(255, 216, 155, 0.2)',
                        boxShadow: '0 8px 32px rgba(0, 0, 0, 0.3)'
                    }}>
                        <div className="flex items-center gap-3 mb-6">
                            <Calendar className="w-5 h-5 text-[rgb(255,216,155)]" />
                            <label className="text-lg font-bold text-[rgb(255,216,155)]">
                                Thời gian
                            </label>
                        </div>

                        <div className="space-y-5">
                            {/* Start Date & Time */}
                            <div>
                                <label className="block text-s font-medium text-white/70 mb-2">Thời gian bắt đầu <span className="text-red-400">*</span></label>
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

                            {/* Calendar Type */}
                            <div>
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

                            <hr className="border-[rgba(255,216,155,0.2)]" />

                            {/* Options */}
                            <div className="space-y-4">
                                {/* Full Day Option */}
                                <label className="flex items-center gap-2 cursor-pointer group">
                                    <input
                                        type="checkbox"
                                        checked={formData.isFullDay || false}
                                        onChange={(e) => handleInputChange('isFullDay', e.target.checked)}
                                        className="w-4 h-4 rounded"
                                        style={{ accentColor: 'rgb(255, 216, 155)' }}
                                    />
                                    <span className="text-white/90 group-hover:text-white transition-colors">Sự kiện cả ngày</span>
                                </label>

                                {/* Recurring Event */}
                                <div className="space-y-3">
                                    <label className="flex items-center gap-2 cursor-pointer group">
                                        <input
                                            type="checkbox"
                                            checked={formData.isRecurring || false}
                                            onChange={(e) => {
                                                handleInputChange('isRecurring', e.target.checked);
                                                if (e.target.checked && formData.recurrenceRule === RecurrenceRule.NONE) {
                                                    handleInputChange('recurrenceRule', RecurrenceRule.YEARLY);
                                                }
                                            }}
                                            className="w-4 h-4"
                                            style={{ accentColor: 'rgb(255, 216, 155)' }}
                                        />
                                        <span className="text-white/90 group-hover:text-white transition-colors">Sự kiện lặp lại</span>
                                    </label>

                                    {formData.isRecurring && (
                                        <div className="ml-6 pl-4 border-l-2 border-[rgba(255,216,155,0.3)] space-y-2">
                                            <label className="block text-sm font-medium text-white/60 mb-2">Tần suất lặp lại</label>
                                            <div className="flex gap-4">
                                                {Object.values(RecurrenceRule)
                                                    .filter(r => r !== RecurrenceRule.NONE)
                                                    .map(rule => (
                                                        <label key={rule} className="flex items-center gap-2 cursor-pointer">
                                                            <input
                                                                type="radio"
                                                                name="recurrence"
                                                                value={rule}
                                                                checked={formData.recurrenceRule === rule}
                                                                onChange={(e) => handleInputChange('recurrenceRule', e.target.value as RecurrenceRule)}
                                                                className="w-4 h-4"
                                                                style={{ accentColor: 'rgb(255, 216, 155)' }}
                                                            />
                                                            <span className="text-white/80 text-sm">
                                                                {rule === RecurrenceRule.YEARLY && 'Hàng năm'}
                                                                {rule === RecurrenceRule.MONTHLY && 'Hàng tháng'}
                                                            </span>
                                                        </label>
                                                    ))}
                                            </div>
                                        </div>
                                    )}
                                </div>
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
                            className="px-6 py-3 rounded-lg font-semibold text-sm flex items-center justify-center gap-2 transition-all duration-300 hover:shadow-lg disabled:opacity-50 disabled:cursor-not-allowed group"
                            style={{
                                background: 'linear-gradient(135deg, rgb(255, 216, 155) 0%, rgb(255, 196, 115) 100%)',
                                color: '#1a1f2e',
                                boxShadow: '0 4px 12px rgba(255, 216, 155, 0.3)',
                            }}
                        >
                            <Save className="w-4 h-4" />
                            <span>{loading ? 'Đang tạo...' : 'Tạo sự kiện'}</span>
                        </button>
                    </div>
                </form>
            </div>
        </div>
    );
};

export default EventFormPage;