import React, { createContext, useContext, useState, useCallback } from 'react';

interface EventContextType {
    triggerEventsUpdate: () => void;
    eventsUpdated: number;
}

const EventContext = createContext<EventContextType | null>(null);

export const EventProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
    const [eventsUpdated, setEventsUpdated] = useState(0);

    const triggerEventsUpdate = useCallback(() => {
        setEventsUpdated(prev => prev + 1);
    }, []);

    return (
        <EventContext.Provider value={{ triggerEventsUpdate, eventsUpdated }}>
            {children}
        </EventContext.Provider>
    );
};

export const useEventContext = () => {
    const context = useContext(EventContext);
    if (!context) throw new Error('useEventContext must be used within EventProvider');
    return context;
};