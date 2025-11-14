// src/hooks/useUserActivity.ts
import { useEffect, useRef } from 'react';

const API_BASE_URL =
    import.meta.env.VITE_API_BASE_URL ||
    (import.meta.env.DEV
        ? 'http://localhost:8080/legacy/api'
        : 'https://legacymap.onrender.com/legacy/api');

/**
 * ✅ Hook tự động gửi heartbeat để track user activity
 * Gọi API /auth/heartbeat mỗi 2 phút
 */
export const useUserActivity = () => {
    const intervalRef = useRef<NodeJS.Timeout | null>(null);

    useEffect(() => {
        const token = localStorage.getItem('authToken');

        if (!token) {
            console.log('⚠️ No auth token, skipping heartbeat');
            return;
        }

        console.log('💓 Starting heartbeat service');

        const sendHeartbeat = async () => {
            try {
                const response = await fetch(`${API_BASE_URL}/auth/heartbeat`, {
                    method: 'POST',
                    headers: {
                        'Authorization': `Bearer ${token}`,
                        'Content-Type': 'application/json'
                    },
                    credentials: 'include'
                });

                if (response.ok) {
                    console.log('💓 Heartbeat sent successfully at', new Date().toLocaleTimeString());
                } else {
                    console.error('❌ Heartbeat failed:', response.status);
                }
            } catch (error) {
                console.error('❌ Heartbeat error:', error);
            }
        };

        // ✅ Gửi heartbeat ngay khi mount
        sendHeartbeat();

        // ✅ Gửi heartbeat mỗi 2 phút (120000ms)
        intervalRef.current = setInterval(sendHeartbeat, 2 * 60 * 1000);

        // Cleanup khi unmount
        return () => {
            console.log('🛑 Stopping heartbeat service');
            if (intervalRef.current) {
                clearInterval(intervalRef.current);
            }
        };
    }, []);
};