    import {http} from '@/api/http';
    import type {Event, EventCreateRequest, EventUpdateRequest} from '@/types/event';

    export const eventsApi = {
        // Create a new event
        async createEvent(request: EventCreateRequest, familyTreeId?: string): Promise<Event> {
            let response;
            if (familyTreeId) {
                // Sự kiện thuộc cây gia phả
                response = await http.post<Event>(`/events/family-tree/${familyTreeId}`, request);
            } else {
                // Sự kiện cá nhân
                response = await http.post<Event>(`/events/personal`, request);
            }

            return response.data;
        },

        // Update an event
        async updateEvent(eventId: string, request: EventUpdateRequest): Promise<Event> {
            const response = await http.put<Event>(`/events/${eventId}`, request);
            return response.data;
        },

        // Delete an event
        async deleteEvent(eventId: string): Promise<void> {
            await http.delete(`/events/${eventId}`);
        },

        // Get event by ID
        async getEvent(eventId: string): Promise<Event> {
            const response = await http.get<Event>(`/events/${eventId}`);
            return response.data;
        },

        // Get events by family tree
        async getFamilyTreeEvents(familyTreeId: string): Promise<Event[]> {
            const response = await http.get<Event[]>(`/events/family-tree/${familyTreeId}`);
            return response.data;
        },

        // Get upcoming events
        async getUpcomingEvents(limit: number = 10): Promise<Event[]> {
            const response = await http.get<Event[]>(`/events/upcoming?limit=${limit}`);
            return response.data;
        },

        // Get events in date range
        async getEventsInDateRange(familyTreeId: string, start: string, end: string): Promise<Event[]> {
            const response = await http.get<Event[]>(
                `/events/family-tree/${familyTreeId}/range?start=${start}&end=${end}`
            );
            return response.data;
        }
    };