export const sseService = {
    connect: (userId: string, onMessage: (data: any) => void) => {
        const baseUrl = import.meta.env.VITE_API_BASE_URL;
        const url = `${baseUrl}/notifications/stream?userId=${userId}`;

        const eventSource = new EventSource(url, {
            withCredentials: true  // <-- Quan trọng: gửi cookie JWT
        });

        eventSource.onmessage = (event) => {
            try {
                const data = JSON.parse(event.data);
                onMessage(data);
            } catch (e) {
                console.error('Invalid SSE data', e);
            }
        };

        eventSource.onerror = (error) => {
            console.error('SSE error:', error);
            eventSource.close();
            // Reconnect sau 5s
            setTimeout(() => sseService.connect(userId, onMessage), 5000);
        };

        return eventSource;
    },

    disconnect: (eventSource: EventSource | null) => {
        if (eventSource) eventSource.close();
    }
};