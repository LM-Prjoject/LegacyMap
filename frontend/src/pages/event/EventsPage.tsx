import React, {useState, useMemo, useEffect} from 'react';
import { useNavigate } from 'react-router-dom';
import { Calendar, Plus, ChevronLeft, ChevronRight, Clock, Grid, List } from 'lucide-react';
import { useUpcomingEvents } from '@/hooks/useEvents';
import {useCalendarEvents} from "@/hooks/useCalendarEvents";
import { EventType, Event } from '@/types/event';
import lunisolar from 'lunisolar';
import Navbar from "@/components/layout/Navbar";
import EventDetailModal from "@/components/eventModal/EventDetailModal";
import { format, parseISO, isSameDay } from 'date-fns';
import { getVietnameseLunarDay, getVietnameseLunarMonth, getLunarInfo, getVietnameseStemBranch } from '@/utils/lunarUtils';

type CalendarView = 'month' | 'week';

const EventsPage: React.FC = () => {
    const navigate = useNavigate();
    const [currentDate, setCurrentDate] = useState(new Date());
    const [currentTime, setCurrentTime] = useState(new Date());
    const [calendarView, setCalendarView] = useState<CalendarView>('month');
    const [selectedEventId, setSelectedEventId] = useState<string | null>(null);
    const [isDetailModalOpen, setIsDetailModalOpen] = useState(false);
    const [selectedDayEvents, setSelectedDayEvents] = useState<Event[]>([]);
    const [isDayEventsModalOpen, setIsDayEventsModalOpen] = useState(false);
    const [selectedDate, setSelectedDate] = useState<Date | null>(null);
    const userData = localStorage.getItem('user');
    const familyTreeId = userData
        ? JSON.parse(userData).familyTreeId
        || JSON.parse(userData).family_tree_id
        || JSON.parse(userData).familyTree?.id
        : null;
    const {
        events: calendarEvents = [],
        loading: calendarLoading,
        refetch: refetchCalendar
    } = useCalendarEvents(familyTreeId, currentDate);

    const {
        events: upcomingEventsList = [],
        loading: upcomingLoading,
        error,
        refetch: refetchUpcoming
    } = useUpcomingEvents(familyTreeId, 90);

    const isLoading = calendarLoading || upcomingLoading;

    const handleRefetch = () => {
        refetchCalendar();
        refetchUpcoming();
    };

    useEffect(() => {
        const timer = setInterval(() => setCurrentTime(new Date()), 1000);
        return () => clearInterval(timer);
    }, []);

    const today = useMemo(() => currentTime, [currentTime]);
    const todayLunar = useMemo(() => getLunarInfo(today), [today]);

    const [currentWeekStart, setCurrentWeekStart] = useState<Date>(() => {
        const now = new Date();
        const start = new Date(now);
        start.setDate(now.getDate() - now.getDay() + 1);
        start.setHours(0, 0, 0, 0);
        return start;
    });

    const getISOWeek = (date: Date): number => {
        const d = new Date(date);
        d.setHours(0, 0, 0, 0);
        // Thứ 5 của tuần hiện tại
        d.setDate(d.getDate() + 3 - (d.getDay() + 6) % 7);
        // Thứ 5 của tuần 1 năm
        const week1 = new Date(d.getFullYear(), 0, 4);
        week1.setDate(week1.getDate() + 3 - (week1.getDay() + 6) % 7);
        // Tính số tuần
        return Math.round(((d.getTime() - week1.getTime()) / 86400000) / 7) + 1;
    };

    const getDaysInMonth = (date: Date) => {
        const year = date.getFullYear();
        const month = date.getMonth();
        const firstDay = new Date(year, month, 1);
        const lastDay = new Date(year, month + 1, 0);
        const daysInMonth = lastDay.getDate();
        const startingDayOfWeek = firstDay.getDay();
        return { daysInMonth, startingDayOfWeek };
    };

    const getWeekDays = (startDate: Date) => {
        return Array.from({ length: 7 }, (_, i) => {
            const d = new Date(startDate);
            d.setDate(startDate.getDate() + i);
            return d;
        });
    };

    useEffect(() => {
        if (calendarView === 'week') {
            setCurrentDate(new Date(currentWeekStart));
        }
    }, [calendarView, currentWeekStart]);

    const { daysInMonth, startingDayOfWeek } = getDaysInMonth(currentDate);
    const weekDays = getWeekDays(currentWeekStart);

    const previousPeriod = () => {
        if (calendarView === 'month') {
            setCurrentDate(prev => new Date(prev.getFullYear(), prev.getMonth() - 1));
        } else {
            setCurrentWeekStart(prev => {
                const newStart = new Date(prev);
                newStart.setDate(prev.getDate() - 7);
                return newStart;
            });
        }
    };

    const nextPeriod = () => {
        if (calendarView === 'month') {
            setCurrentDate(prev => new Date(prev.getFullYear(), prev.getMonth() + 1));
        } else {
            setCurrentWeekStart(prev => {
                const newStart = new Date(prev);
                newStart.setDate(prev.getDate() + 7);
                return newStart;
            });
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
        return calendarEvents.filter(e => isSameDay(parseISO(e.startDate), date));
    };

    const todayEvents = upcomingEventsList.filter(e => isSameDay(parseISO(e.startDate), new Date()));
    const upcomingEvents = upcomingEventsList.slice(0, 5);

    const handleDateClick = (date: Date) => {
        const eventsOnDate = getEventsForDate(date);
        if (eventsOnDate.length === 0) return;

        if (eventsOnDate.length === 1) {
            setSelectedEventId(eventsOnDate[0].id);
            setSelectedDate(date);
            setIsDetailModalOpen(true);
        } else {
            setSelectedDayEvents(eventsOnDate);
            setIsDayEventsModalOpen(true);
        }
    };

    const getLunarDisplayForDate = (date: Date): string => {
        const lsr = lunisolar(date);
        const isLeap = lsr.lunar.isLeapMonth;
        const dayStr = getVietnameseLunarDay(lsr.lunar.day);
        const monthStr = getVietnameseLunarMonth(lsr.lunar.month, isLeap);
        return `${dayStr} / ${monthStr}`;
    };

    const monthNames = [
        'Tháng 1', 'Tháng 2', 'Tháng 3', 'Tháng 4', 'Tháng 5', 'Tháng 6',
        'Tháng 7', 'Tháng 8', 'Tháng 9', 'Tháng 10', 'Tháng 11', 'Tháng 12'
    ];

    if (isLoading) {
        return (
            <>
                <Navbar />
                <div className="min-h-screen flex items-center justify-center" style={{backgroundColor: '#2a3548'}}>
                    <div className="text-center">
                        <div className="animate-spin rounded-full h-12 w-12 border-b-2 mx-auto mb-4" style={{borderColor: 'rgb(255, 216, 155)'}}></div>
                        <p style={{color: 'rgb(255, 216, 155)'}}>Đang tải sự kiện...</p>
                    </div>
                </div>
            </>
        );
    }

    if (error) {
        return (
            <>
                <Navbar />
                <div className="min-h-screen flex items-center justify-center" style={{backgroundColor: '#2a3548'}}>
                    <div className="text-center">
                        <p className="text-red-600">Lỗi: {error}</p>
                        <button onClick={() => window.location.reload()} className="mt-4 px-4 py-2 rounded-lg transition-colors" style={{backgroundColor: 'rgb(255, 216, 155)', color: '#2a3548'}}>
                            Thử lại
                        </button>
                    </div>
                </div>
            </>
        );
    }

    return (
        <div className="min-h-screen " style={{background: 'linear-gradient(135deg, #2a3548 0%, #3d4a5f 25%, #4a5970 50%, #3d4a5f 75%, #2a3548 100%)'}}>
            <Navbar />
            {/* Header */}

            <div className="px-6 pb-6 pt-8 shadow-2xl relative overflow-hidden" style={{
                background: 'linear-gradient(135deg, #2a3548 0%, #1f2937 50%, #2a3548 100%)',
                borderBottom: '3px solid rgba(255, 216, 155, 0.3)',
                position: 'relative',
            }}>
                <div className="max-w-7xl mx-auto relative z-10">
                    <div className="flex justify-between items-center mb-8">
                        <h1 className="text-4xl font-black tracking-tight" style={{
                            color: 'rgb(255, 216, 155)',
                            textShadow: '0 4px 20px rgba(255, 216, 155, 0.6), 0 0 40px rgba(255, 216, 155, 0.4), 0 0 60px rgba(255, 216, 155, 0.2)'
                        }}>
                            LỊCH SỰ KIỆN
                        </h1>
                        <button
                            onClick={() => navigate('/events/create')}
                            className="px-6 py-3 rounded-xl flex items-center gap-2 transition-all duration-300 shadow-lg hover:shadow-xl font-semibold"
                            style={{background: 'linear-gradient(135deg, rgb(255, 216, 155) 0%, rgb(255, 230, 190) 100%)',
                                color: '#2a3548'}}
                        >
                            <Plus className="w-5 h-5" />
                            <span className="font-medium">Tạo sự kiện</span>
                        </button>
                    </div>

                    {/* CURRENT DATE INFO */}
                    <div className="rounded-3xl p-8 shadow-2xl backdrop-blur" style={{
                        background: 'linear-gradient(135deg, rgba(255, 216, 155, 0.25) 0%, rgba(255, 230, 190, 0.15) 50%, rgba(255, 216, 155, 0.25) 100%)',
                        border: '2px solid rgba(255, 216, 155, 0.5)'}}
                    >
                        <div className="grid grid-cols-4 gap-8 text-center space-y-1">
                            <div>
                                <div className="text-sm font-medium mb-2" style={{color: 'rgb(255, 216, 155)'}}>Giờ</div>
                                <div className="text-2xl font-bold text-white mb-1">
                                    {today.toLocaleTimeString('vi-VN', { hour: '2-digit', minute: '2-digit' })}
                                </div>
                                <div className="text-xs" style={{color: 'rgba(255, 216, 155, 0.6)'}}>{todayLunar.hourCanChi}</div>
                            </div>
                            <div>
                                <div className="text-sm font-medium mb-2" style={{color: 'rgb(255, 216, 155)'}}>Ngày</div>
                                <div className="text-2xl font-bold text-white mb-1">{todayLunar.dayStr}</div>
                                <div className="text-xs" style={{color: 'rgba(255, 216, 155, 0.6)'}}>{todayLunar.dayStemBranch}</div>
                            </div>
                            <div>
                                <div className="text-sm font-medium mb-2" style={{color: 'rgb(255, 216, 155)'}}>Tháng</div>
                                <div className="text-2xl font-bold text-white mb-1">{todayLunar.month}</div>
                                <div className="text-xs" style={{color: 'rgba(255, 216, 155, 0.6)'}}>{getVietnameseStemBranch(lunisolar(today).format('cM'))}</div>
                            </div>
                            <div>
                                <div className="text-sm font-medium mb-2" style={{color: 'rgb(255, 216, 155)'}}>Năm</div>
                                <div className="text-2xl font-bold text-white mb-1">{todayLunar.year}</div>
                                <div className="text-xs" style={{color: 'rgba(255, 216, 155, 0.6)'}}>{todayLunar.yearStemBranch}</div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>

            {/* Calendar */}
            <div className="max-w-7xl mx-auto px-6 py-10" style={{ position: 'relative' }}>
                <div className="rounded-3xl shadow-2xl p-10 mb-10" style={{
                    background: 'linear-gradient(135deg, rgba(255, 245, 220, 0.95) 0%, rgba(255, 235, 200, 0.9) 25%, rgba(255, 245, 220, 0.95) 50%, rgba(255, 235, 200, 0.9) 75%, rgba(255, 245, 220, 0.95) 100%)',
                    border: '3px solid rgba(255, 216, 155, 0.6)',
                    boxShadow: '0 20px 60px rgba(42, 53, 72, 0.3), inset 0 0 100px rgba(255, 255, 255, 0.5)'
                }}>
                    <div className="flex justify-between items-center mb-10">
                        <div className="flex items-center gap-5">
                            <button onClick={previousPeriod}
                                    className="p-3 rounded-xl transition-all hover:scale-110 shadow-lg"
                                    style={{
                                        background: 'linear-gradient(135deg, #2a3548 0%, #3d4a5f 100%)',
                                        border: '2px solid rgba(255, 216, 155, 0.5)'
                                    }}
                            >
                                <ChevronLeft className="w-6 h-6" style={{color: 'rgb(255, 216, 155)'}} />
                            </button>
                            <h2 className="text-3xl font-black min-w-[320px] text-center tracking-wide" style={{
                                color: '#2a3548',
                                textShadow: '0 3px 15px rgba(42, 53, 72, 0.3)'
                            }}>
                                {calendarView === 'month'
                                    ? `${monthNames[currentDate.getMonth()]} / ${currentDate.getFullYear()}`
                                    : `Tuần ${getISOWeek(currentWeekStart)} / ${currentWeekStart.getFullYear()}`
                                }
                            </h2>
                            <button
                                onClick={nextPeriod}
                                className="p-3 rounded-xl transition-all hover:scale-110 shadow-lg"
                                style={{
                                    background: 'linear-gradient(135deg, #2a3548 0%, #3d4a5f 100%)',
                                    border: '2px solid rgba(255, 216, 155, 0.5)'
                                }}
                            >
                                <ChevronRight className="w-6 h-6" style={{color: 'rgb(255, 216, 155)'}} />
                            </button>
                        </div>
                        <div className="flex gap-2 p-1 rounded-xl" style={{background: 'linear-gradient(135deg, rgba(42, 53, 72, 0.1) 0%, rgba(42, 53, 72, 0.05) 100%)'}}>
                            <button
                                onClick={() => setCalendarView('month')}
                                className={`px-5 py-2.5 rounded-lg flex items-center gap-2 transition-all font-medium ${calendarView === 'month' ? 'shadow-lg' : ''}`}
                                style={calendarView === 'month'
                                    ? {background: 'linear-gradient(135deg, #2a3548 0%, #3d4a5f 100%)', color: 'rgb(255, 216, 155)', boxShadow: '0 5px 20px rgba(42, 53, 72, 0.4)'}
                                    : {color: '#2a3548'}}
                            >
                                <Grid className="w-4 h-4" /> Tháng
                            </button>
                            <button
                                onClick={() => setCalendarView('week')}
                                className={`px-5 py-2.5 rounded-lg flex items-center gap-2 transition-all font-medium ${calendarView === 'week' ? 'shadow-lg' : ''}`}
                                style={calendarView === 'week'
                                    ? {background: 'linear-gradient(135deg, #2a3548 0%, #3d4a5f 100%)', color: 'rgb(255, 216, 155)', boxShadow: '0 5px 20px rgba(42, 53, 72, 0.4)'}
                                    : {color: '#2a3548'}}
                            >
                                <List className="w-4 h-4" /> Tuần
                            </button>
                        </div>
                    </div>

                    <div className="grid grid-cols-7 gap-3">
                        {['T2', 'T3', 'T4', 'T5', 'T6', 'T7', 'CN'].map(day => (
                            <div key={day} className="text-center font-bold py-3 text-sm" style={{
                                color: '#2a3548',
                                background: 'linear-gradient(135deg, rgba(42, 53, 72, 0.08) 0%, rgba(42, 53, 72, 0.03) 100%)',
                                textShadow: '0 1px 3px rgba(0,0,0,0.1)'
                            }}>
                                {day}
                            </div>
                        ))}

                        {calendarView === 'month' ? (
                            <>
                                {Array.from({ length: startingDayOfWeek === 0 ? 6 : startingDayOfWeek - 1 }).map((_, i) => (
                                    <div key={`empty-${i}`} className="aspect-square" />
                                ))}
                                {Array.from({ length: daysInMonth }).map((_, i) => {
                                    const day = i + 1;
                                    const date = new Date(currentDate.getFullYear(), currentDate.getMonth(), day);
                                    const eventsHere = getEventsForDate(date);
                                    const isTodayDate = isToday(date);

                                    return (
                                        <div
                                            key={day}
                                            className="aspect-square p-3 rounded-2xl cursor-pointer transition-all duration-300 hover:scale-105"
                                            style={{
                                                background: isTodayDate
                                                    ? 'linear-gradient(135deg, #2a3548 0%, #3d4a5f 50%, #2a3548 100%)'
                                                    : eventsHere.length > 0
                                                        ? 'linear-gradient(135deg, rgba(42, 53, 72, 0.15) 0%, rgba(42, 53, 72, 0.08) 50%, rgba(42, 53, 72, 0.15) 100%)'
                                                        : 'linear-gradient(135deg, rgba(255, 255, 255, 0.6) 0%, rgba(255, 255, 255, 0.3) 100%)',
                                                border: `3px solid ${isTodayDate ? 'rgb(255, 216, 155)' : eventsHere.length > 0 ? 'rgba(42, 53, 72, 0.4)' : 'transparent'}`,
                                                boxShadow: isTodayDate ? '0 10px 30px rgba(255, 216, 155, 0.6), 0 0 60px rgba(255, 216, 155, 0.3)' : eventsHere.length > 0 ? '0 5px 20px rgba(42, 53, 72, 0.2)' : '0 2px 10px rgba(0,0,0,0.05)'
                                            }}
                                            onClick={() => handleDateClick(date)}
                                        >
                                            <div className={`text-center font-bold text-lg mb-1`} style={{
                                                color: isTodayDate ? 'rgb(255, 216, 155)' : '#2a3548',
                                                textShadow: isTodayDate ? '0 2px 10px rgba(255, 216, 155, 0.5)' : '0 1px 3px rgba(0,0,0,0.1)'
                                            }}>
                                                {day}
                                            </div>
                                            <div className="text-xs text-center mb-2" style={{
                                                color: isTodayDate ? 'rgba(255, 216, 155, 0.8)' : 'rgba(42, 53, 72, 0.7)'
                                            }}>
                                                {getLunarDisplayForDate(date)}
                                            </div>
                                            {eventsHere.length > 0 && (
                                                <div className="flex justify-center gap-1">
                                                    {eventsHere.slice(0, 3).map((_, idx) => (
                                                        <div
                                                            key={idx}
                                                            className="w-1.5 h-1.5 rounded-full"
                                                            style={{
                                                                background: isTodayDate ? 'rgb(255, 216, 155)' : 'linear-gradient(135deg, #2a3548 0%, #4a5970 100%)',
                                                                boxShadow: '0 2px 8px rgba(0,0,0,0.3)'
                                                            }}
                                                        />
                                                    ))}
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
                                        className="aspect-square p-3 rounded-2xl cursor-pointer transition-all duration-300 hover:scale-105"
                                        style={{
                                            background: isTodayDate
                                                ? 'linear-gradient(135deg, #2a3548 0%, #3d4a5f 50%, #2a3548 100%)'
                                                : eventsHere.length > 0
                                                    ? 'linear-gradient(135deg, rgba(42, 53, 72, 0.15) 0%, rgba(42, 53, 72, 0.08) 50%, rgba(42, 53, 72, 0.15) 100%)'
                                                    : 'linear-gradient(135deg, rgba(255, 255, 255, 0.6) 0%, rgba(255, 255, 255, 0.3) 100%)',
                                            border: `3px solid ${isTodayDate ? 'rgb(255, 216, 155)' : eventsHere.length > 0 ? 'rgba(42, 53, 72, 0.4)' : 'transparent'}`,
                                            boxShadow: isTodayDate ? '0 10px 30px rgba(255, 216, 155, 0.6), 0 0 60px rgba(255, 216, 155, 0.3)' : eventsHere.length > 0 ? '0 5px 20px rgba(42, 53, 72, 0.2)' : '0 2px 10px rgba(0,0,0,0.05)'
                                        }}
                                        onClick={() => handleDateClick(date)}
                                    >
                                        <div className={`text-center font-bold text-lg mb-1`} style={{
                                            color: isTodayDate ? 'rgb(255, 216, 155)' : '#2a3548',
                                            textShadow: isTodayDate ? '0 2px 10px rgba(255, 216, 155, 0.5)' : '0 1px 3px rgba(0,0,0,0.1)'
                                        }}>
                                            {date.getDate()}
                                        </div>
                                        <div className="text-xs text-center mb-2" style={{
                                            color: isTodayDate ? 'rgba(255, 216, 155, 0.8)' : 'rgba(42, 53, 72, 0.7)'
                                        }}>
                                            {getLunarDisplayForDate(date)}
                                        </div>
                                        {eventsHere.length > 0 && (
                                            <div className="flex justify-center gap-1">
                                                {eventsHere.slice(0, 3).map((_, idx) => (
                                                    <div
                                                        key={idx}
                                                        className="w-1.5 h-1.5 rounded-full"
                                                        style={{
                                                            background: isTodayDate ? 'rgb(255, 216, 155)' : 'linear-gradient(135deg, #2a3548 0%, #4a5970 100%)',
                                                            boxShadow: '0 2px 8px rgba(0,0,0,0.3)'
                                                        }}
                                                    />
                                                ))}
                                            </div>
                                        )}
                                    </div>
                                );
                            })
                        )}
                    </div>
                </div>

                {/* Events Today & Upcoming */}
                <div className="grid md:grid-cols-2 gap-6">
                    <div className="rounded-3xl shadow-2xl p-6"
                         style={{
                             background: 'linear-gradient(135deg, rgba(255, 245, 220, 0.95) 0%, rgba(255, 235, 200, 0.9) 50%, rgba(255, 245, 220, 0.95) 100%)',
                             border: '3px solid rgba(255, 216, 155, 0.6)',
                             boxShadow: '0 15px 50px rgba(42, 53, 72, 0.25)'
                         }}>
                        <h3 className="text-xl font-bold mb-5 flex items-center gap-2" style={{
                            color: '#2a3548',
                            textShadow: '0 2px 10px rgba(42, 53, 72, 0.2)'
                        }}>
                            Sự kiện hôm nay
                        </h3>
                        {todayEvents.length > 0 ? (
                            <div className="space-y-3">
                                {todayEvents.map(event => (
                                    <div
                                        key={event.id}
                                        onClick={() => {
                                            setSelectedEventId(event.id);
                                            setSelectedDate(new Date(event.startDate));
                                            setIsDetailModalOpen(true);
                                        }}
                                        className="p-4 rounded-xl cursor-pointer transition-all hover:scale-[1.02] hover:shadow-xl"
                                        style={{
                                            background: 'linear-gradient(135deg, rgba(42, 53, 72, 0.1) 0%, rgba(42, 53, 72, 0.05) 100%)',
                                            border: '2px solid rgba(42, 53, 72, 0.2)',
                                            boxShadow: '0 5px 20px rgba(42, 53, 72, 0.15)'
                                        }}
                                    >
                                        <h4 className="font-black text-lg mb-2" style={{
                                            color: '#2a3548',
                                            textShadow: '0 1px 3px rgba(0,0,0,0.1)'
                                        }}>{event.title}</h4>
                                        <div className="flex items-center gap-2 text-sm mb-1" style={{color: 'rgba(42, 53, 72, 0.8)'}}>
                                            <Clock className="w-4 h-4" />
                                            <span className="font-medium">{formatTime(event.startDate)}</span>
                                        </div>
                                        <p className="text-xs px-2.5 py-1 rounded-full inline-block" style={{
                                            background: 'linear-gradient(135deg, rgba(255, 216, 155, 0.4) 0%, rgba(255, 216, 155, 0.2) 100%)',
                                            color: '#2a3548',
                                            border: '1px solid rgba(255, 216, 155, 0.5)'
                                        }}>
                                            {getEventTypeLabel(event.eventType)}
                                        </p>
                                    </div>
                                ))}
                            </div>
                        ) : (
                            <p className="text-center py-16 text-base font-semibold" style={{color: 'rgba(42, 53, 72, 0.5)'}}>
                                Không có sự kiện nào hôm nay
                            </p>
                        )}
                    </div>

                    <div className="rounded-3xl shadow-2xl p-6"
                         style={{background: 'linear-gradient(135deg, rgba(255, 216, 155, 0.08) 0%, rgba(255, 216, 155, 0.03) 100%)', border: '1px solid rgba(255, 216, 155, 0.2)'}}>
                        <h3 className="text-xl font-bold mb-5 flex items-center gap-2" style={{color: 'rgb(255, 216, 155)'}}>
                            Sự kiện sắp tới
                        </h3>
                        {upcomingEvents.length > 0 ? (
                            <div className="space-y-3">
                                {upcomingEvents.map(event => (
                                    <div
                                        key={event.id}
                                        onClick={() => {
                                            setSelectedEventId(event.id);
                                            setSelectedDate(new Date(event.startDate));
                                            setIsDetailModalOpen(true);
                                        }}
                                        className="p-4 rounded-xl cursor-pointer transition-all hover:scale-[1.02] hover:shadow-xl"
                                        style={{backgroundColor: 'rgba(255, 216, 155, 0.15)', border: '1px solid rgba(255, 216, 155, 0.3)'}}
                                    >
                                        <h4 className="font-bold text-white mb-2 text-lg">{event.title}</h4>
                                        <div className="flex items-center gap-2 text-sm mb-1" style={{color: 'rgba(255, 216, 155, 0.9)'}}>
                                            <Calendar className="w-4 h-4" />
                                            <span className="font-medium">{new Date(event.startDate).toLocaleDateString('vi-VN')}</span>
                                            <span className="mx-1">•</span>
                                            <span className="font-medium">{formatTime(event.startDate)}</span>
                                        </div>
                                        <p className="text-xs px-2.5 py-1 rounded-full inline-block" style={{backgroundColor: 'rgba(255, 216, 155, 0.2)', color: 'rgba(255, 216, 155, 0.8)'}}>
                                            {getEventTypeLabel(event.eventType)}
                                        </p>
                                    </div>
                                ))}
                            </div>
                        ) : (
                            <p className="text-center py-12 text-sm" style={{color: 'rgba(255, 216, 155, 0.5)'}}>
                                Không có sự kiện sắp tới
                            </p>
                        )}
                    </div>
                </div>

                {isDayEventsModalOpen && (
                    <div
                        className="fixed inset-0 flex items-center justify-center z-50 p-4"
                        onClick={() => setIsDayEventsModalOpen(false)}
                    >
                        <div
                            className="absolute inset-0"
                            style={{
                                backdropFilter: 'blur(8px)',
                                WebkitBackdropFilter: 'blur(8px)',
                                backgroundColor: 'rgba(0, 0, 0, 0.4)'
                            }}
                        />
                        <div
                            className="relative bg-gradient-to-br from-[#1a2332] via-[#2a3548] to-[#1f2937] rounded-2xl p-6 max-w-md w-full shadow-2xl border border-[#D4AF37]/40 relative"
                            onClick={(e) => e.stopPropagation()}
                        >

                            <button
                                onClick={() => setIsDayEventsModalOpen(false)}
                                className="absolute top-4 right-4 w-8 h-8 flex items-center justify-center rounded-full bg-[#D4AF37]/20 hover:bg-[#D4AF37]/30 transition-all text-[#D4AF37] font-bold"
                            >
                                ✕
                            </button>

                            <h3 className="text-xl font-bold text-[#D4AF37] mb-4 pr-8 text-center">
                                Sự kiện ngày {format(selectedDayEvents[0].startDate, 'dd/MM/yyyy')}
                            </h3>
                            <div className="space-y-2 max-h-[60vh] overflow-y-auto pr-2">
                                {selectedDayEvents.map(event => (
                                    <div
                                        key={event.id}
                                        onClick={() => {
                                            setSelectedEventId(event.id);
                                            setSelectedDate(new Date(event.startDate));
                                            setIsDetailModalOpen(true);
                                            setIsDayEventsModalOpen(false);
                                        }}
                                        className="p-4 rounded-xl bg-gradient-to-r from-[#D4AF37]/10 to-[#4a7c59]/10 border border-[#D4AF37]/30 cursor-pointer hover:border-[#D4AF37]/50 hover:shadow-lg hover:shadow-[#D4AF37]/20 transition-all group"
                                    >
                                        <div className="flex items-start justify-between gap-3">
                                            <div className="flex-1 min-w-0">
                                                <h4 className="font-bold text-[#D4AF37] text-base group-hover:text-[#ffd700] transition-colors truncate">
                                                    {event.title}
                                                </h4>
                                                <p className="text-xs text-white/70 mt-1">
                                                    {formatTime(event.startDate)} • {getEventTypeLabel(event.eventType)}
                                                    {event.isRecurring && ' (Lặp lại)'}
                                                </p>
                                            </div>
                                        </div>
                                    </div>
                                ))}
                            </div>
                        </div>
                    </div>
                )}
                {/* Event Detail Modal */}
                {isDetailModalOpen && selectedEventId && (
                    <EventDetailModal
                        eventId={selectedEventId}
                        isOpen={isDetailModalOpen}
                        onClose={() => {
                            setIsDetailModalOpen(false);
                            setSelectedEventId(null);
                            setSelectedDate(null);
                        }}
                        onDelete={() => {
                            handleRefetch();
                            setIsDetailModalOpen(false);
                            setSelectedEventId(null);
                            setSelectedDate(null);
                        }}
                        onUpdate={() => {
                            handleRefetch();
                        }}
                        selectedDate={selectedDate}
                    />
                )}
            </div>
        </div>
    );
};

export default EventsPage;