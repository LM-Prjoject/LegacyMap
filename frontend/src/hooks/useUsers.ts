// src/hooks/useUsers.ts
import { useState, useEffect, useCallback } from 'react';
import { User, UseUsersReturn } from '../types/ts_user';

const API_BASE_URL =
    import.meta.env.VITE_API_BASE_URL ||
    (import.meta.env.DEV
        ? 'http://localhost:8080/legacy/api'
        : 'https://legacymap.onrender.com/legacy/api');

export const useUsers = (): UseUsersReturn => {
  const [users, setUsers] = useState<User[]>([]);
  const [loading, setLoading] = useState<boolean>(true);
  const [error, setError] = useState<string | null>(null);

  const fetchUsers = useCallback(async () => {
    try {
      setLoading(true);
      setError(null);

      const token = localStorage.getItem('authToken');
      if (!token) {
        throw new Error('Vui lòng đăng nhập để tiếp tục');
      }

      console.log('🌐 Fetching users from:', `${API_BASE_URL}/admin/users`);

      const response = await fetch(`${API_BASE_URL}/admin/users`, {
        method: 'GET',
        headers: {
          Authorization: `Bearer ${token}`,
          'Content-Type': 'application/json',
        },
        credentials: 'include',
      });

      console.log('📡 Response status:', response.status);

      // ===== FIX: Kiểm tra Content-Type trước khi parse =====
      const contentType = response.headers.get('content-type');
      console.log('📄 Content-Type:', contentType);

      // Nếu không phải JSON, đừng parse
      if (!contentType || !contentType.includes('application/json')) {
        const text = await response.text();
        console.error('❌ Received HTML instead of JSON:', text.substring(0, 200));

        if (response.status === 403) {
          throw new Error('Bạn không có quyền truy cập trang này. Cần quyền Admin.');
        } else if (response.status === 401) {
          throw new Error('Phiên đăng nhập hết hạn. Vui lòng đăng nhập lại.');
        } else {
          throw new Error(`Lỗi server: ${response.status} ${response.statusText}`);
        }
      }

      // Xử lý lỗi status
      if (!response.ok) {
        if (response.status === 403) {
          throw new Error('Bạn không có quyền truy cập. Cần quyền Admin.');
        }
        if (response.status === 401) {
          localStorage.removeItem('authToken');
          localStorage.removeItem('user');
          window.location.href = '/signin';
          throw new Error('Phiên đăng nhập hết hạn.');
        }
        throw new Error(`Không thể tải danh sách người dùng: ${response.status}`);
      }

      // Parse JSON an toàn
      const data = await response.json();
      console.log('✅ Users data received:', data);

      // Backend có thể trả về { result: [...] } hoặc trực tiếp array
      const usersList = Array.isArray(data) ? data : (data.result || []);
      setUsers(usersList);
    } catch (err) {
      const message = err instanceof Error ? err.message : 'Đã xảy ra lỗi không xác định';
      console.error('❌ Error fetching users:', err);
      setError(message);
    } finally {
      setLoading(false);
    }
  }, []);

  const banUser = useCallback(async (userId: string) => {
    try {
      const token = localStorage.getItem('authToken');
      if (!token) throw new Error('Không tìm thấy token xác thực');

      const res = await fetch(`${API_BASE_URL}/admin/users/${userId}/ban`, {
        method: 'POST',
        headers: {
          Authorization: `Bearer ${token}`,
          'Content-Type': 'application/json',
        },
      });

      if (res.status === 401 || res.status === 403) {
        throw new Error('Không có quyền thực hiện thao tác này');
      }

      if (!res.ok) {
        const text = await res.text();
        console.error('Ban user failed:', text);
        throw new Error('Không thể khóa người dùng');
      }

      setUsers(prev => prev.map(u => (u.id === userId ? { ...u, isBanned: true } : u)));
    } catch (err) {
      console.error('Error banning user:', err);
      throw err;
    }
  }, []);

  const unbanUser = useCallback(async (userId: string) => {
    try {
      const token = localStorage.getItem('authToken');
      if (!token) throw new Error('Không tìm thấy token xác thực');

      const res = await fetch(`${API_BASE_URL}/admin/users/${userId}/unban`, {
        method: 'POST',
        headers: {
          Authorization: `Bearer ${token}`,
          'Content-Type': 'application/json',
        },
      });

      if (res.status === 401 || res.status === 403) {
        throw new Error('Không có quyền thực hiện thao tác này');
      }

      if (!res.ok) {
        throw new Error('Không thể mở khóa người dùng');
      }

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