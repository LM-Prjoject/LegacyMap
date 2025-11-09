import { useState, useEffect } from 'react';
import { authApi } from '@/api/auth';
import type {User} from '@/types/ts_user';

export const useCurrentUser = () => {
    const [userId, setUserId] = useState<string | null>(null);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);

    useEffect(() => {
        const loadUser = async () => {
            try {
                const user = await (authApi.getMe() as Promise<User>);
                setUserId(user.id);
            } catch (err: any) {
                console.error('Failed to load current user:', err);
                setError('Unauthorized');
                authApi.logout();
            } finally {
                setLoading(false);
            }
        };

        const token = localStorage.getItem('authToken');
        if (token) {
            loadUser();
        } else {
            setLoading(false);
        }
    }, []);

    return { userId, loading, error };
};