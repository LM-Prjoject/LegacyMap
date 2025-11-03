import { useState, useEffect } from 'react';
import { eventsApi } from '@/api/eventApi';
import type { Event, EventCreateRequest, EventUpdateRequest } from '@/types/event';

export const useEvents = (familyTreeId?: string) => {
    const [events, setEvents] = useState<Event[]>([]);
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState<string | null>(null);

    const fetchEvents = async () => {
        if (!familyTreeId) return;

        setLoading(true);
        setError(null);
        try {
            const data = await eventsApi.getFamilyTreeEvents(familyTreeId);
            setEvents(data);
        } catch (err) {
            setError(err instanceof Error ? err.message : 'Failed to fetch events');
        } finally {
            setLoading(false);
        }
    };

    const createEvent = async (request: EventCreateRequest): Promise<Event | null> => {
        if (!familyTreeId) return null;

        setError(null);
        try {
            const newEvent = await eventsApi.createEvent(familyTreeId, request);
            setEvents(prev => [...prev, newEvent]);
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

export const useUpcomingEvents = (limit: number = 10) => {
    const [events, setEvents] = useState<Event[]>([]);
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState<string | null>(null);

    const fetchUpcomingEvents = async () => {
        setLoading(true);
        setError(null);
        try {
            const data = await eventsApi.getUpcomingEvents(limit);
            setEvents(data);
        } catch (err) {
            setError(err instanceof Error ? err.message : 'Failed to fetch upcoming events');
        } finally {
            setLoading(false);
        }
    };

    useEffect(() => {
        fetchUpcomingEvents();
    }, [limit]);

    return { events, loading, error, refetch: fetchUpcomingEvents };
};