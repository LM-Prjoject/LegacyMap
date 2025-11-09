// src/hooks/useFamilyTrees.ts
import { useState, useEffect, useCallback } from 'react';

export interface FamilyTree {
    id: string;
    name: string;
    description?: string;
    createdBy: string;
    isPublic: boolean;
    coverImageUrl?: string;
    createdAt: string;
    updatedAt: string;
    memberCount?: number;
}

interface UseFamilyTreesReturn {
    familyTrees: FamilyTree[];
    loading: boolean;
    error: string | null;
    refreshTrees: () => Promise<void>;
}

// ===== FIX: Thêm API_BASE_URL giống useUsers.ts =====
const API_BASE_URL =
    import.meta.env.VITE_API_BASE_URL ||
    (import.meta.env.DEV
        ? 'http://localhost:8080/legacy/api'
        : 'https://legacymap.onrender.com/legacy/api');

export const useFamilyTrees = (): UseFamilyTreesReturn => {
    const [familyTrees, setFamilyTrees] = useState<FamilyTree[]>([]);
    const [loading, setLoading] = useState<boolean>(true);
    const [error, setError] = useState<string | null>(null);

    const fetchFamilyTrees = useCallback(async () => {
        try {
            setLoading(true);
            setError(null);

            const token = localStorage.getItem('authToken');
            if (!token) {
                throw new Error('Vui lòng đăng nhập để tiếp tục');
            }

            // ===== FIX: Dùng full URL với API_BASE_URL =====
            const url = `${API_BASE_URL}/admin/family-trees`;
            console.log('🌐 Fetching family trees from:', url);

            const response = await fetch(url, {
                method: 'GET',
                headers: {
                    'Authorization': `Bearer ${token}`,
                    'Content-Type': 'application/json',
                },
                credentials: 'include',
            });

            console.log('📡 Family Trees Response status:', response.status);

            // ===== FIX: Kiểm tra Content-Type trước khi parse JSON =====
            const contentType = response.headers.get('content-type');
            console.log('📄 Content-Type:', contentType);

            if (!contentType || !contentType.includes('application/json')) {
                const text = await response.text();
                console.error('❌ Received HTML instead of JSON:', text.substring(0, 200));

                if (response.status === 403) {
                    throw new Error('Bạn không có quyền truy cập. Cần quyền Admin.');
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
                throw new Error(`Không thể tải danh sách gia phả: ${response.status}`);
            }

            // Parse JSON an toàn
            const data = await response.json();
            console.log('✅ Family Trees data received:', data);

            // Backend có thể trả về { result: [...] } hoặc trực tiếp array
            const treesList = Array.isArray(data) ? data : (data.result || []);
            setFamilyTrees(treesList);
        } catch (err) {
            const errorMessage = err instanceof Error ? err.message : 'Đã xảy ra lỗi không xác định';
            setError(errorMessage);
            console.error('❌ Error fetching family trees:', err);
        } finally {
            setLoading(false);
        }
    }, []);

    const refreshTrees = useCallback(async () => {
        await fetchFamilyTrees();
    }, [fetchFamilyTrees]);

    useEffect(() => {
        fetchFamilyTrees();
    }, [fetchFamilyTrees]);

    return {
        familyTrees,
        loading,
        error,
        refreshTrees,
    };
};