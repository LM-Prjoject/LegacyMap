import { useState, useEffect, useCallback, useRef } from 'react';
import { eventsApi } from '@/api/eventApi';
import type { Event } from '@/types/event';
import { startOfMonth, endOfMonth, subMonths, addMonths, formatISO, isWithinInterval } from 'date-fns';

export const useCalendarEvents = (familyTreeId: string | undefined, currentDate = new Date()) => {
    const [events, setEvents] = useState<Event[]>([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);
    const cachedRange = useRef<{ start: Date; end: Date } | null>(null);

    const isDateInCachedRange = (date: Date): boolean => {
        if (!cachedRange.current) return false;
        return isWithinInterval(date, { 
            start: cachedRange.current.start, 
            end: cachedRange.current.end 
        });
    };

    const fetchEvents = useCallback(async (forceRefresh = false) => {
        const viewStart = startOfMonth(currentDate);
        const viewEnd = endOfMonth(currentDate);

        if (!forceRefresh && cachedRange.current && 
            isDateInCachedRange(viewStart) && 
            isDateInCachedRange(viewEnd)) {
            return;
        }

        setLoading(true);
        try {
            const fromDate = subMonths(viewStart, 12);
            const toDate = addMonths(viewEnd, 12);

            const start = formatISO(fromDate, { representation: 'complete' });
            const end = formatISO(toDate, { representation: 'complete' });

            const data = await eventsApi.getEventsInDateRange(familyTreeId, start, end);

            cachedRange.current = { start: fromDate, end: toDate };
            setEvents(data || []);
            setError(null);
        } catch (err: any) {
            console.error('Error fetching events:', err);
            setError('Không thể tải sự kiện. Vui lòng thử lại sau.');
        } finally {
            setLoading(false);
        }
    }, [familyTreeId, currentDate.getFullYear(), currentDate.getMonth()]);

    // Initial fetch
    useEffect(() => {
        fetchEvents();
    }, [fetchEvents]);

    // Function to force refresh events
    const refetch = useCallback(() => {
        fetchEvents(true);
    }, [fetchEvents]);

    return { 
        events, 
        loading, 
        error, 
        refetch 
    };
};