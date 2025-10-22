// src/hooks/useUsers.ts
import { useState, useEffect, useCallback } from 'react';
import { User, UseUsersReturn } from '../types/ts_user';

// ✅ THÊM: API base configuration
const API_CONFIG = {
    BASE_URL: '/api', // Sử dụng proxy
    // Hoặc direct: 'http://localhost:8080/legacy/api'
};

export const useUsers = (): UseUsersReturn => {
    const [users, setUsers] = useState<User[]>([]);
    const [loading, setLoading] = useState<boolean>(true);
    const [error, setError] = useState<string | null>(null);

    const fetchUsers = useCallback(async () => {
        try {
            setLoading(true);
            setError(null);

            // ✅ FIX: Thống nhất dùng 'authToken'
            const token = localStorage.getItem('authToken');
            if (!token) {
                throw new Error('No authentication token found');
            }

            console.log('🔍 Fetching users with token:', token.substring(0, 20) + '...');

            const response = await fetch(`${API_CONFIG.BASE_URL}/admin/users`, {
                method: 'GET',
                headers: {
                    'Authorization': `Bearer ${token}`,
                    'Content-Type': 'application/json',
                },
                credentials: 'include',
            });

            console.log('🔍 Response status:', response.status);

            if (response.status === 403) {
                const errorData = await response.text();
                console.error('🔍 403 Forbidden details:', errorData);
                throw new Error('Access denied: Admin role required. Please check if your account has ADMIN privileges.');
            }

            if (response.status === 401) {
                throw new Error('Authentication failed. Please login again.');
            }

            if (!response.ok) {
                throw new Error(`Failed to fetch users: ${response.status} ${response.statusText}`);
            }

            const data = await response.json();
            console.log('🔍 Users data received:', data);
            setUsers(data);
        } catch (err) {
            const errorMessage = err instanceof Error ? err.message : 'An error occurred';
            setError(errorMessage);
            console.error('❌ Error fetching users:', err);
        } finally {
            setLoading(false);
        }
    }, []);

    const banUser = useCallback(async (userId: string) => {
        try {
            // ✅ FIX: Thống nhất dùng 'authToken'
            const token = localStorage.getItem('authToken');
            if (!token) throw new Error('No authentication token');

            const response = await fetch(`${API_CONFIG.BASE_URL}/admin/users/${userId}/ban`, {
                method: 'POST',
                headers: {
                    'Authorization': `Bearer ${token}`,
                    'Content-Type': 'application/json',
                },
            });

            if (!response.ok) {
                throw new Error('Failed to ban user');
            }

            setUsers(prevUsers =>
                prevUsers.map(u =>
                    u.id === userId ? { ...u, isBanned: true } : u
                )
            );
        } catch (err) {
            console.error('❌ Error banning user:', err);
            throw err;
        }
    }, []);

    const unbanUser = useCallback(async (userId: string) => {
        try {
            // ✅ FIX: Thống nhất dùng 'authToken'
            const token = localStorage.getItem('authToken');
            if (!token) throw new Error('No authentication token');

            const response = await fetch(`${API_CONFIG.BASE_URL}/admin/users/${userId}/unban`, {
                method: 'POST',
                headers: {
                    'Authorization': `Bearer ${token}`,
                    'Content-Type': 'application/json',
                },
            });

            if (!response.ok) {
                throw new Error('Failed to unban user');
            }

            setUsers(prevUsers =>
                prevUsers.map(u =>
                    u.id === userId ? { ...u, isBanned: false } : u
                )
            );
        } catch (err) {
            console.error('❌ Error unbanning user:', err);
            throw err;
        }
    }, []);

    const refreshUsers = useCallback(async () => {
        await fetchUsers();
    }, [fetchUsers]);

    useEffect(() => {
        fetchUsers();
    }, [fetchUsers]);

    return {
        users,
        loading,
        error,
        banUser,
        unbanUser,
        refreshUsers,
    };
};