import React, {useState, useMemo, useEffect} from 'react';
import { useNavigate } from 'react-router-dom';
import { Calendar, Plus, ChevronLeft, ChevronRight, Clock, Grid, List } from 'lucide-react';
import { useUpcomingEvents } from '@/hooks/useEvents';
import { EventType } from '@/types/event';
import lunisolar from 'lunisolar';
import Navbar from "@/components/layout/Navbar.tsx";
import { useEventContext } from '@/contexts/EventContext';
import { format, parseISO, isSameDay, startOfDay, endOfDay } from 'date-fns';
import { formatInTimeZone } from 'date-fns-tz';
import { eventsApi } from '@/api/eventApi';
import { getVietnameseLunarDay, getVietnameseLunarMonth, getLunarInfo, getVietnameseStemBranch } from '@/utils/lunarUtils';

type CalendarView = 'month' | 'week';

const EventsPage: React.FC = () => {
    const navigate = useNavigate();
    const [currentDate, setCurrentDate] = useState(new Date());
    const [currentTime, setCurrentTime] = useState(new Date());
    const [calendarView, setCalendarView] = useState<CalendarView>('month');
    const { eventsUpdated } = useEventContext();
    const userData = localStorage.getItem('user');
    const familyTreeId = userData
        ? JSON.parse(userData).familyTreeId
        || JSON.parse(userData).family_tree_id
        || JSON.parse(userData).familyTree?.id
        : null;
    const { events = [], loading, error, refetch: refetchEvents } = useUpcomingEvents(familyTreeId, 30);

    useEffect(() => {
        const timer = setInterval(() => setCurrentTime(new Date()), 1000);
        return () => clearInterval(timer);
    }, []);

    useEffect(() => {
        if (eventsUpdated) refetchEvents();
    }, [eventsUpdated]);

    const today = useMemo(() => currentTime, [currentTime]);
    const todayLunar = useMemo(() => getLunarInfo(today), [today]);

    const getDaysInMonth = (date: Date) => {
        const year = date.getFullYear();
        const month = date.getMonth();
        const firstDay = new Date(year, month, 1);
        const lastDay = new Date(year, month + 1, 0);
        const daysInMonth = lastDay.getDate();
        const startingDayOfWeek = firstDay.getDay();
        return { daysInMonth, startingDayOfWeek };
    };

    const getWeekDays = (date: Date) => {
        const start = new Date(date);
        start.setDate(date.getDate() - date.getDay() + 1);
        return Array.from({ length: 7 }, (_, i) => {
            const d = new Date(start);
            d.setDate(start.getDate() + i);
            return d;
        });
    };

    const { daysInMonth, startingDayOfWeek } = getDaysInMonth(currentDate);
    const weekDays = getWeekDays(currentDate);

    const previousPeriod = () => {
        if (calendarView === 'month') {
            setCurrentDate(new Date(currentDate.getFullYear(), currentDate.getMonth() - 1));
        } else {
            setCurrentDate(new Date(currentDate.getFullYear(), currentDate.getMonth(), currentDate.getDate() - 7));
        }
    };

    const nextPeriod = () => {
        if (calendarView === 'month') {
            setCurrentDate(new Date(currentDate.getFullYear(), currentDate.getMonth() + 1));
        } else {
            setCurrentDate(new Date(currentDate.getFullYear(), currentDate.getMonth(), currentDate.getDate() + 7));
        }
    };

    const isToday = (date: Date) => date.toDateString() === today.toDateString();

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

    const formatTime = (dateTime: string) => {
        try {
            return format(parseISO(dateTime), 'HH:mm');
        } catch {
            return '--:--';
        }
    };

    const getEventsForDate = (date: Date) => {
        return events.filter(e => {
            try {
                const eventDate = parseISO(e.startDate);
                return isSameDay(eventDate, date);
            } catch {
                return false;
            }
        });
    };

    const getTodayEvents = () => {
        const todayStart = startOfDay(today);
        const todayEnd = endOfDay(today);
        return events.filter(e => {
            const eventDate = parseISO(e.startDate);
            return eventDate >= todayStart && eventDate <= todayEnd;
        });
    };

    const getUpcomingEvents = () => {
        const now = new Date();
        return events
            .filter(e => parseISO(e.startDate) > now)
            .sort((a, b) => parseISO(a.startDate).getTime() - parseISO(b.startDate).getTime())
            .slice(0, 5);
    };

    const handleDateClick = async (date: Date) => {
        if (!familyTreeId) return;

        const start = formatInTimeZone(date, 'Asia/Ho_Chi_Minh', "yyyy-MM-dd'T'00:00:00XXX");
        const end = formatInTimeZone(date, 'Asia/Ho_Chi_Minh', "yyyy-MM-dd'T'23:59:59XXX");

        try {
            const dayEvents = await eventsApi.getEventsInDateRange(familyTreeId, start, end);
            console.log('Sự kiện trong ngày:', dayEvents);
            // Mở modal ở đây
        } catch (err) {
            console.error('Lỗi:', err);
        }
    };

    const getLunarDisplayForDate = (date: Date): string => {
        const lsr = lunisolar(date);
        const isLeap = lsr.lunar.isLeap;
        const dayStr = getVietnameseLunarDay(lsr.lunar.day);
        const monthStr = getVietnameseLunarMonth(lsr.lunar.month, isLeap);
        return `${dayStr} ${monthStr}`;
    };

    const monthNames = [
        'Tháng 1', 'Tháng 2', 'Tháng 3', 'Tháng 4', 'Tháng 5', 'Tháng 6',
        'Tháng 7', 'Tháng 8', 'Tháng 9', 'Tháng 10', 'Tháng 11', 'Tháng 12'
    ];

    if (loading) {
        return (
            <div className="min-h-screen bg-gradient-to-br from-blue-50 via-amber-50 to-blue-100 flex items-center justify-center">
                <div className="text-center">
                    <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto mb-4"></div>
                    <p className="text-blue-900">Đang tải sự kiện...</p>
                </div>
            </div>
        );
    }

    if (error) {
        return (
            <div className="min-h-screen bg-gradient-to-br from-blue-50 via-amber-50 to-blue-100 flex items-center justify-center">
                <div className="text-center">
                    <p className="text-red-600">Lỗi: {error}</p>
                    <button onClick={() => window.location.reload()} className="mt-4 bg-blue-600 text-white px-4 py-2 rounded-lg hover:bg-blue-700">
                        Thử lại
                    </button>
                </div>
            </div>
        );
    }

    return (
        <div className="min-h-screen bg-gradient-to-br from-blue-50 via-amber-50 to-blue-100">
            <Navbar />
            {/* Header */}
            <div className="bg-gradient-to-r from-blue-600 to-blue-700 text-white px-6 py-8 shadow-lg">
                <div className="max-w-7xl mx-auto">
                    <div className="flex justify-between items-center mb-6">
                        <h1 className="text-3xl font-bold">Sự kiện</h1>
                        <button
                            onClick={() => navigate('/events/create')}
                            className="bg-white/20 hover:bg-white/30 backdrop-blur px-6 py-3 rounded-full flex items-center gap-2 transition-all duration-300 shadow-lg"
                        >
                            <Plus className="w-5 h-5" />
                            <span className="font-medium">Tạo sự kiện</span>
                        </button>
                    </div>

                    {/* CURRENT DATE INFO */}
                    <div className="bg-amber-50 text-blue-900 rounded-2xl p-6 shadow-md">
                        <div className="grid grid-cols-4 gap-4 text-center space-y-1">
                            <div>
                                <div className="text-sm font-medium text-blue-600">Giờ</div>
                                <div className="text-2xl font-bold text-blue-800">
                                    {today.toLocaleTimeString('vi-VN', { hour: '2-digit', minute: '2-digit' })}
                                </div>
                                <div className="text-xs text-blue-600 mt-1">{todayLunar.hourCanChi}</div>
                            </div>
                            <div>
                                <div className="text-sm font-medium text-blue-600">Ngày</div>
                                <div className="text-2xl font-bold text-blue-800">{todayLunar.dayStr}</div>
                                <div className="text-xs text-blue-600 mt-1">{todayLunar.dayStemBranch}</div>
                            </div>
                            <div>
                                <div className="text-sm font-medium text-blue-600">Tháng</div>
                                <div className="text-2xl font-bold text-blue-800">{todayLunar.month}</div>
                                <div className="text-xs text-blue-600 mt-1">{getVietnameseStemBranch(lunisolar(today).format('cM'))}</div>
                            </div>
                            <div>
                                <div className="text-sm font-medium text-blue-600">Năm</div>
                                <div className="text-2xl font-bold text-blue-800">{todayLunar.year}</div>
                                <div className="text-xs text-blue-600 mt-1">{todayLunar.yearStemBranch}</div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>

            {/* Calendar */}
            <div className="max-w-7xl mx-auto px-6 py-8">
                <div className="bg-white rounded-2xl shadow-xl p-6 mb-8">
                    <div className="flex justify-between items-center mb-6">
                        <div className="flex items-center gap-4">
                            <button onClick={previousPeriod} className="p-2 hover:bg-blue-50 rounded-full">
                                <ChevronLeft className="w-6 h-6 text-blue-600" />
                            </button>
                            <h2 className="text-xl font-bold text-blue-900">
                                {calendarView === 'month'
                                    ? `${monthNames[currentDate.getMonth()]} / ${currentDate.getFullYear()}`
                                    : `Tuần ${Math.ceil(currentDate.getDate() / 7)} / ${currentDate.getFullYear()}`
                                }
                            </h2>
                            <button onClick={nextPeriod} className="p-2 hover:bg-blue-50 rounded-full">
                                <ChevronRight className="w-6 h-6 text-blue-600" />
                            </button>
                        </div>
                        <div className="flex gap-2 bg-blue-50 rounded-lg p-1">
                            <button
                                onClick={() => setCalendarView('month')}
                                className={`px-4 py-2 rounded-md flex items-center gap-2 transition-all ${
                                    calendarView === 'month' ? 'bg-blue-600 text-white shadow-md' : 'text-blue-600 hover:bg-blue-100'
                                }`}
                            >
                                <Grid className="w-4 h-4" /> Tháng
                            </button>
                            <button
                                onClick={() => setCalendarView('week')}
                                className={`px-4 py-2 rounded-md flex items-center gap-2 transition-all ${
                                    calendarView === 'week' ? 'bg-blue-600 text-white shadow-md' : 'text-blue-600 hover:bg-blue-100'
                                }`}
                            >
                                <List className="w-4 h-4" /> Tuần
                            </button>
                        </div>
                    </div>

                    <div className="grid grid-cols-7 gap-2">
                        {['T2', 'T3', 'T4', 'T5', 'T6', 'T7', 'CN'].map(day => (
                            <div key={day} className="text-center font-semibold text-blue-600 py-2">{day}</div>
                        ))}

                        {calendarView === 'month' ? (
                            <>
                                {Array.from({ length: startingDayOfWeek === 0 ? 6 : startingDayOfWeek - 1 }).map((_, i) => (
                                    <div key={`empty-${i}`} className="aspect-square p-2" />
                                ))}
                                {Array.from({ length: daysInMonth }).map((_, i) => {
                                    const day = i + 1;
                                    const date = new Date(currentDate.getFullYear(), currentDate.getMonth(), day);
                                    const eventsHere = getEventsForDate(date);
                                    const isTodayDate = isToday(date);

                                    return (
                                        <div
                                            key={day}
                                            className={`aspect-square p-2 rounded-xl cursor-pointer transition-all border ${
                                                isTodayDate
                                                    ? 'bg-gradient-to-br from-amber-400 to-amber-500 text-white shadow-lg scale-105 border-amber-300'
                                                    : eventsHere.length > 0
                                                        ? 'bg-blue-100 hover:bg-blue-200 border-blue-200'
                                                        : 'hover:bg-amber-50 border-transparent'
                                            }`}
                                            onClick={() => handleDateClick(date)}
                                        >
                                            <div className={`text-center font-medium ${isTodayDate ? 'text-white' : 'text-blue-900'}`}>
                                                {day}
                                            </div>
                                            <div className="text-xs text-amber-600 text-center mt-1">
                                                {getLunarDisplayForDate(date)}
                                            </div>
                                            {eventsHere.length > 0 && (
                                                <div className="flex justify-center mt-1">
                                                    <div className={`w-1.5 h-1.5 rounded-full ${isTodayDate ? 'bg-white' : 'bg-blue-600'}`} />
                                                </div>
                                            )}
                                        </div>
                                    );
                                })}
                            </>
                        ) : (
                            weekDays.map((date, i) => {
                                const eventsHere = getEventsForDate(date);
                                const isTodayDate = isToday(date);
                                return (
                                    <div
                                        key={i}
                                        className={`aspect-square p-2 rounded-xl cursor-pointer transition-all border ${
                                            isTodayDate
                                                ? 'bg-gradient-to-br from-amber-400 to-amber-500 text-white shadow-lg scale-105 border-amber-300'
                                                : eventsHere.length > 0
                                                    ? 'bg-blue-100 hover:bg-blue-200 border-blue-200'
                                                    : 'hover:bg-amber-50 border-transparent'
                                        }`}
                                    >
                                        <div className={`text-center font-medium ${isTodayDate ? 'text-white' : 'text-blue-900'}`}>
                                            {date.getDate()}
                                        </div>
                                        <div className="text-xs text-amber-600 text-center mt-1">
                                            {getLunarDisplayForDate(date)}
                                        </div>
                                        {eventsHere.length > 0 && (
                                            <div className="flex justify-center mt-1">
                                                <div className={`w-1.5 h-1.5 rounded-full ${isTodayDate ? 'bg-white' : 'bg-blue-600'}`} />
                                            </div>
                                        )}
                                    </div>
                                );
                            })
                        )}
                    </div>

                    {events.length > 0 && (
                        <div className="mt-4 text-center">
              <span className="inline-flex items-center gap-2 bg-blue-50 text-blue-700 px-4 py-2 rounded-full text-sm font-medium">
                <Calendar className="w-4 h-4" />
                  {events.length} sự kiện
              </span>
                        </div>
                    )}
                </div>

                {/* Events Today & Upcoming */}
                <div className="grid md:grid-cols-2 gap-8">
                    <div className="bg-white rounded-2xl shadow-xl p-6">
                        <h3 className="text-xl font-bold text-blue-900 mb-4">Sự kiện hôm nay</h3>
                        {getTodayEvents().length > 0 ? (
                            <div className="space-y-3">
                                {getTodayEvents().map(event => (
                                    <div
                                        key={event.id}
                                        onClick={() => navigate(`/events/${event.id}`)}
                                        className="bg-gradient-to-r from-blue-50 to-amber-50 p-4 rounded-xl cursor-pointer hover:shadow-md transition-shadow border border-blue-100"
                                    >
                                        <h4 className="font-semibold text-blue-900 mb-1">{event.title}</h4>
                                        <div className="flex items-center gap-2 text-sm text-blue-600">
                                            <Clock className="w-4 h-4" />
                                            <span>{formatTime(event.startDate)}</span>
                                        </div>
                                        <p className="text-xs text-amber-600 mt-1">{getEventTypeLabel(event.eventType)}</p>
                                    </div>
                                ))}
                            </div>
                        ) : (
                            <p className="text-blue-400 text-center py-8">Không có sự kiện nào hôm nay</p>
                        )}
                    </div>

                    <div className="bg-white rounded-2xl shadow-xl p-6">
                        <h3 className="text-xl font-bold text-blue-900 mb-4">Sự kiện sắp tới</h3>
                        {getUpcomingEvents().length > 0 ? (
                            <div className="space-y-3">
                                {getUpcomingEvents().map(event => (
                                    <div
                                        key={event.id}
                                        onClick={() => navigate(`/events/${event.id}`)}
                                        className="bg-gradient-to-r from-amber-50 to-blue-50 p-4 rounded-xl cursor-pointer hover:shadow-md transition-shadow border border-amber-100"
                                    >
                                        <h4 className="font-semibold text-blue-900 mb-1">{event.title}</h4>
                                        <div className="flex items-center gap-2 text-sm text-blue-600">
                                            <Calendar className="w-4 h-4" />
                                            <span>{new Date(event.startDate).toLocaleDateString('vi-VN')}</span>
                                            <span className="mx-1">•</span>
                                            <span>{formatTime(event.startDate)}</span>
                                        </div>
                                        <p className="text-xs text-amber-600 mt-1">{getEventTypeLabel(event.eventType)}</p>
                                    </div>
                                ))}
                            </div>
                        ) : (
                            <p className="text-blue-400 text-center py-8">Không có sự kiện sắp tới</p>
                        )}
                    </div>
                </div>
            </div>
        </div>
    );
};

export default EventsPage;