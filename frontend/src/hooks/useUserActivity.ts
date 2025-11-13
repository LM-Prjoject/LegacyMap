// src/hooks/useUserActivity.ts
import { useEffect, useRef } from 'react';
import { http } from '../api/http';

/**
 * ✅ Hook tự động gửi heartbeat để track user activity
 * Gọi API mỗi 30 giây khi user đang active
 */
export const useUserActivity = () => {
    const intervalRef = useRef<NodeJS.Timeout | null>(null);
    const lastActivityRef = useRef<number>(Date.now());

    useEffect(() => {
        const sendHeartbeat = async () => {
            try {
                await http.post('/user/heartbeat');
                console.log('💓 Heartbeat sent');
            } catch (error) {
                console.error('❌ Heartbeat failed:', error);
            }
        };

        // Track user activity (mouse, keyboard, touch)
        const updateActivity = () => {
            lastActivityRef.current = Date.now();
        };

        // Event listeners
        const events = ['mousedown', 'keydown', 'scroll', 'touchstart'];
        events.forEach(event => {
            window.addEventListener(event, updateActivity);
        });

        // ✅ Gửi heartbeat mỗi 30 giây
        intervalRef.current = setInterval(() => {
            const now = Date.now();
            const timeSinceLastActivity = now - lastActivityRef.current;

            // Chỉ gửi nếu user có activity trong 2 phút gần đây
            if (timeSinceLastActivity < 2 * 60 * 1000) {
                sendHeartbeat();
            }
        }, 30000); // 30 seconds

        // Gửi heartbeat ngay khi mount
        sendHeartbeat();

        // Cleanup
        return () => {
            if (intervalRef.current) {
                clearInterval(intervalRef.current);
            }
            events.forEach(event => {
                window.removeEventListener(event, updateActivity);
            });
        };
    }, []);
};