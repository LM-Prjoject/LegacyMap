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
    const failureCountRef = useRef<number>(0);

    useEffect(() => {
        const token = localStorage.getItem('authToken');

        if (!token) {
            console.log('⚠️ No auth token, skipping heartbeat');
            return;
        }

        console.log('💓 Starting heartbeat service');

        const sendHeartbeat = async () => {
            if (document.visibilityState === 'hidden') {
                return; // skip when tab hidden
            }
            if (!navigator.onLine) {
                console.warn('⚠️ Offline, skip heartbeat');
                return;
            }
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
                    failureCountRef.current = 0;
                } else {
                    console.error('❌ Heartbeat failed:', response.status);
                    failureCountRef.current += 1;
                    // Stop after 3 consecutive failures or on auth errors
                    if (response.status === 401 || response.status === 403 || failureCountRef.current >= 3) {
                        if (intervalRef.current) clearInterval(intervalRef.current);
                        intervalRef.current = null;
                        console.warn('🛑 Stopping heartbeat due to repeated failures or auth error');
                    }
                }
            } catch (error) {
                console.error('❌ Heartbeat error:', error);
                failureCountRef.current += 1;
                if (failureCountRef.current >= 3) {
                    if (intervalRef.current) clearInterval(intervalRef.current);
                    intervalRef.current = null;
                    console.warn('🛑 Stopping heartbeat due to network errors');
                }
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