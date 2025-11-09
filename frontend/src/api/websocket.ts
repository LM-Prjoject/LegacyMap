import SockJS from 'sockjs-client';
import { Client } from '@stomp/stompjs';
import type { NotificationResponse } from '@/types/notification';

class WebSocketService {
    private client: Client | null = null;
    private userId: string | null = null;
    private onMessage?: (notif: NotificationResponse) => void;

    connect(userId: string, onNotification: (notif: NotificationResponse) => void) {
        if (this.client?.active) return;
        if (!userId) {
            console.error('WebSocket connect failed: missing userId');
            return;
        }

        this.userId = userId;
        this.onMessage = onNotification;

        const socket = new SockJS(import.meta.env.VITE_API_URL + '/ws');
        this.client = new Client({
            webSocketFactory: () => socket as any,
            reconnectDelay: 5000,
            onConnect: () => {
                console.log('WebSocket connected:', this.userId);
                this.client?.subscribe('/user/queue/notifications', (msg) => {
                    try {
                        const notification: NotificationResponse = JSON.parse(msg.body);
                        this.onMessage?.(notification);
                    } catch (err) {
                        console.error('Parse error:', err);
                    }
                });
            },
            onStompError: (frame) => console.error('STOMP Error:', frame),
        });

        this.client.activate();
    }

    disconnect() {
        this.client?.deactivate();
        this.client = null;
    }
}

export const webSocketService = new WebSocketService();
