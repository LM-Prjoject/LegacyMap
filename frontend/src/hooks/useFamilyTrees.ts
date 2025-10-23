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
    memberCount?: number; // ✅ THÊM DÒNG NÀY
}

interface UseFamilyTreesReturn {
    familyTrees: FamilyTree[];
    loading: boolean;
    error: string | null;
    refreshTrees: () => Promise<void>;
}

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
                throw new Error('No authentication token found');
            }

            const response = await fetch('/api/admin/family-trees', {
                method: 'GET',
                headers: {
                    'Authorization': `Bearer ${token}`,
                    'Content-Type': 'application/json',
                },
            });

            console.log('📊 Family Trees Response status:', response.status);

            if (response.status === 403) {
                throw new Error('Access denied: Admin role required');
            }

            if (!response.ok) {
                throw new Error(`Failed to fetch family trees: ${response.status}`);
            }

            const data = await response.json();
            console.log('📊 Family Trees data received:', data);
            setFamilyTrees(data);
        } catch (err) {
            const errorMessage = err instanceof Error ? err.message : 'An error occurred';
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