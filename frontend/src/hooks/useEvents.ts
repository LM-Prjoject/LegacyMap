import { useState, useEffect, useCallback } from 'react';
import { eventsApi } from '@/api/eventApi';
import type { Event, EventCreateRequest, EventUpdateRequest } from '@/types/event';
import {useEventContext} from "@/contexts/EventContext.tsx";
import { addDays, formatISO } from 'date-fns';

export const useEvents = (familyTreeId?: string) => {
    const [events, setEvents] = useState<Event[]>([]);
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState<string | null>(null);
    const { triggerEventsUpdate } = useEventContext();

    const fetchEvents = async () => {
        setLoading(true);
        setError(null);
        try {
            const data = familyTreeId
                ? await eventsApi.getFamilyTreeEvents(familyTreeId)
                : await eventsApi.getPersonalEvents();
            setEvents(data);
        } catch (err) {
            setError(err instanceof Error ? err.message : 'Failed to fetch events');
        } finally {
            setLoading(false);
        }
    };

    const createEvent = async (request: EventCreateRequest): Promise<Event | null> => {
        setError(null);
        try {
            const newEvent = await eventsApi.createEvent(request, familyTreeId);
            setEvents(prev => [...prev, newEvent]);
            triggerEventsUpdate();
            return newEvent;
        } catch (err) {
            setError(err instanceof Error ? err.message : 'Failed to create event');
            return null;
        }
    };

    const updateEvent = async (eventId: string, request: EventUpdateRequest): Promise<Event | null> => {
        setError(null);
        try {
            const updatedEvent = await eventsApi.updateEvent(eventId, request);
            setEvents(prev => prev.map(event =>
                event.id === eventId ? updatedEvent : event
            ));
            triggerEventsUpdate();
            return updatedEvent;
        } catch (err) {
            setError(err instanceof Error ? err.message : 'Failed to update event');
            return null;
        }
    };

    const deleteEvent = async (eventId: string): Promise<boolean> => {
        setError(null);
        try {
            await eventsApi.deleteEvent(eventId);
            setEvents(prev => prev.filter(event => event.id !== eventId));
            triggerEventsUpdate();
            return true;
        } catch (err) {
            setError(err instanceof Error ? err.message : 'Failed to delete event');
            return false;
        }
    };

    useEffect(() => {
        if (familyTreeId) {
            fetchEvents();
        }
    }, [familyTreeId]);

    return {
        events,
        loading,
        error,
        createEvent,
        updateEvent,
        deleteEvent,
        refetch: fetchEvents
    };
};

export const useUpcomingEvents = (familyTreeId?: string, daysAhead = 30) => {
    const [events, setEvents] = useState<Event[]>([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);

    const fetchEvents = useCallback(async () => {
        try {
            setLoading(true);
            setError(null);

            const now = new Date();
            const start = formatISO(now);
            const end = formatISO(addDays(now, daysAhead));

            const data = familyTreeId
                ? await eventsApi.getEventsInDateRange(familyTreeId, start, end)
                : await eventsApi.getUpcomingEvents();
            setEvents(data);
        } catch (err: any) {
            setError(err.message || 'Lỗi tải sự kiện');
        } finally {
            setLoading(false);
        }
    }, [familyTreeId, daysAhead]);

    useEffect(() => {
        fetchEvents();
    }, [fetchEvents]);

    return { events, loading, error, refetch: fetchEvents };
};