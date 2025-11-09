import { useState, useEffect, useCallback } from 'react';
import { User, UseUsersReturn } from '../types/ts_user';

// Base URL tự động chọn giữa local và Render
const API_BASE_URL =
  import.meta.env.VITE_API_BASE_URL ||
  (import.meta.env.DEV
    ? 'http://localhost:8080/legacy/api' // chạy local
    : 'https://legacymap.onrender.com/legacy/api'); // deploy Render

export const useUsers = (): UseUsersReturn => {
  const [users, setUsers] = useState<User[]>([]);
  const [loading, setLoading] = useState<boolean>(true);
  const [error, setError] = useState<string | null>(null);

  const fetchUsers = useCallback(async () => {
    try {
      setLoading(true);
      setError(null);

      const token = localStorage.getItem('authToken');
      if (!token) throw new Error('No authentication token found');

      console.log('Fetching users from:', `${API_BASE_URL}/admin/users`);
      console.log('Using token:', token.substring(0, 15) + '...');

      const response = await fetch(`${API_BASE_URL}/admin/users`, {
        method: 'GET',
        headers: {
          Authorization: `Bearer ${token}`,
          'Content-Type': 'application/json',
        },
        credentials: 'include',
      });

      console.log('📡 Response status:', response.status);

      // --- Error handling ---
      if (response.status === 403) {
        const html = await response.text();
        console.warn('403 Forbidden (HTML returned):', html.slice(0, 100));
        throw new Error('Access denied: Admin role required.');
      }
      if (response.status === 401) {
        throw new Error('Authentication failed. Please login again.');
      }
      if (!response.ok) {
        const text = await response.text();
        console.warn('Unexpected response:', text.slice(0, 100));
        throw new Error(`Failed to fetch users: ${response.status}`);
      }

      // Parse JSON safely
      const data = await response.json();
      console.log('Users data received:', data);
      setUsers(data);
    } catch (err) {
      const message = err instanceof Error ? err.message : 'An error occurred';
      console.error('Error fetching users:', err);
      setError(message);
    } finally {
      setLoading(false);
    }
  }, []);

  const banUser = useCallback(async (userId: string) => {
    try {
      const token = localStorage.getItem('authToken');
      if (!token) throw new Error('No authentication token');

      const res = await fetch(`${API_BASE_URL}/admin/users/${userId}/ban`, {
        method: 'POST',
        headers: {
          Authorization: `Bearer ${token}`,
          'Content-Type': 'application/json',
        },
      });
      if (!res.ok) throw new Error('Failed to ban user');
      setUsers(prev => prev.map(u => (u.id === userId ? { ...u, isBanned: true } : u)));
    } catch (err) {
      console.error('Error banning user:', err);
      throw err;
    }
  }, []);

  const unbanUser = useCallback(async (userId: string) => {
    try {
      const token = localStorage.getItem('authToken');
      if (!token) throw new Error('No authentication token');

      const res = await fetch(`${API_BASE_URL}/admin/users/${userId}/unban`, {
        method: 'POST',
        headers: {
          Authorization: `Bearer ${token}`,
          'Content-Type': 'application/json',
        },
      });
      if (!res.ok) throw new Error('Failed to unban user');
      setUsers(prev => prev.map(u => (u.id === userId ? { ...u, isBanned: false } : u)));
    } catch (err) {
      console.error('Error unbanning user:', err);
      throw err;
    }
  }, []);

  const refreshUsers = useCallback(fetchUsers, [fetchUsers]);

  useEffect(() => {
    fetchUsers();
  }, [fetchUsers]);

  return { users, loading, error, banUser, unbanUser, refreshUsers };
};
