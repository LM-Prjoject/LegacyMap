import { useEffect } from 'react';
import { showToast } from '@/lib/toast';

export function useAutoLogout(timeoutMinutes: number = 30): void {
    useEffect(() => {
        let timer: ReturnType<typeof setTimeout>;

        const resetTimer = (): void => {
            clearTimeout(timer);
            timer = setTimeout(logout, timeoutMinutes * 60 * 1000);
        };

        const logout = (): void => {
            localStorage.removeItem('authToken');
            localStorage.removeItem('user');
            showToast.info('Phiên đăng nhập đã hết hạn, vui lòng đăng nhập lại.');
            window.location.href = '/';
        };

        const events: Array<keyof WindowEventMap> = ['mousemove', 'keydown', 'click', 'scroll'];

        events.forEach((eventName) => window.addEventListener(eventName, resetTimer));
        resetTimer();

        return () => {
            clearTimeout(timer);
            events.forEach((eventName) => window.removeEventListener(eventName, resetTimer));
        };
    }, [timeoutMinutes]);
}
