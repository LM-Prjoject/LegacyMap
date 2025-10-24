// src/api/ts_admin.ts
import { http } from './http'

// ============================================
// 📋 TYPE DEFINITIONS
// ============================================

export interface User {
    id: string;
    email: string;
    username: string;
    roleName: string;
    isVerified: boolean;
    isActive: boolean;
    isBanned: boolean;
    bannedAt?: string;
    lastLogin?: string;
    createdAt: string;
    updatedAt?: string;
    provider?: string;
}

export interface UserDetail extends User {
    // Có thể thêm fields khác nếu cần
}

export interface FamilyTree {
    id: string;
    name: string;
    description?: string;
    isPublic: boolean;
    createdBy: string;  // ✅ Backend trả "createdBy" không phải "createdByUserId"
    createdByEmail?: string;
    createdByUsername?: string;
    createdAt: string;
    updatedAt?: string;
}

export interface AdminStats {
    totalUsers: number;
    adminUsers: number;
    bannedUsers: number;
    activeUsers: number;
    totalFamilyTrees: number;
    adminUserEmails: string[];
}

export interface ApiResponse<T> {
    success: boolean;
    code?: number;
    message?: string;
    result: T;
}

// ============================================
// 📌 ADMIN API CLIENT
// ============================================

export const adminApi = {
    // 👥 USER MANAGEMENT
    async getAllUsers(): Promise<User[]> {
        try {
            console.log('📡 Fetching all users...');
            const response = await http.get<ApiResponse<User[]>>('/admin/users');

            console.log('✅ Users response:', response.data);

            // Check if response is wrapped or direct array
            const users = Array.isArray(response.data)
                ? response.data
                : response.data.result || [];

            console.log('✅ Users fetched:', users.length);
            return users;
        } catch (error: any) {
            console.error('❌ Error fetching users:', error.response?.data || error.message);
            throw error;
        }
    },

    async getUserDetail(userId: string): Promise<UserDetail> {
        try {
            console.log('📡 Fetching user detail:', userId);
            const response = await http.get<ApiResponse<UserDetail>>(`/admin/users/${userId}`);

            const user = response.data.result || response.data;
            console.log('✅ User detail fetched:', user);
            return user as UserDetail;
        } catch (error: any) {
            console.error('❌ Error fetching user detail:', error.response?.data || error.message);
            throw error;
        }
    },

    async banUser(userId: string): Promise<void> {
        try {
            console.log('🚫 Banning user:', userId);
            await http.post(`/admin/users/${userId}/ban`);
            console.log('✅ User banned successfully');
        } catch (error: any) {
            console.error('❌ Error banning user:', error.response?.data || error.message);
            throw error;
        }
    },

    async unbanUser(userId: string): Promise<void> {
        try {
            console.log('✅ Unbanning user:', userId);
            await http.post(`/admin/users/${userId}/unban`);
            console.log('✅ User unbanned successfully');
        } catch (error: any) {
            console.error('❌ Error unbanning user:', error.response?.data || error.message);
            throw error;
        }
    },

    // 🌳 FAMILY TREE MANAGEMENT
    async getAllFamilyTrees(): Promise<FamilyTree[]> {
        try {
            console.log('🌳 Fetching all family trees...');

            const token = localStorage.getItem('authToken');
            console.log('🔑 Token status:', {
                exists: !!token,
                length: token?.length || 0,
                preview: token?.substring(0, 20) + '...'
            });

            const response = await http.get<FamilyTree[]>('/admin/family-trees');

            console.log('📦 Full response object:', response);
            console.log('📦 Response.data:', response.data);
            console.log('📦 Response.data type:', Array.isArray(response.data) ? 'Array' : typeof response.data);

            // ✅ FIX: Backend trả trực tiếp array, không wrap trong object
            const trees = Array.isArray(response.data) ? response.data : [];

            console.log('✅ Family trees fetched:', {
                count: trees.length,
                trees: trees,
                firstTree: trees[0] || null
            });

            return trees;
        } catch (error: any) {
            console.error('❌ Error fetching family trees:', {
                status: error.response?.status,
                statusText: error.response?.statusText,
                data: error.response?.data,
                message: error.message
            });
            throw error;
        }
    },

    // 📊 STATISTICS
    async getAdminStats(): Promise<AdminStats> {
        try {
            console.log('📊 Fetching admin stats...');
            const response = await http.get<ApiResponse<AdminStats>>('/admin/stats');

            const stats = response.data.result || response.data;
            console.log('✅ Admin stats fetched:', stats);
            return stats as AdminStats;
        } catch (error: any) {
            console.error('❌ Error fetching admin stats:', error.response?.data || error.message);
            throw error;
        }
    }
};

// ============================================
// 🛠️ HELPER FUNCTIONS
// ============================================

export const getAuthToken = (): string | null => {
    return localStorage.getItem('authToken');
};

export const isAuthenticated = (): boolean => {
    return !!getAuthToken();
};

export const hasAdminRole = (): boolean => {
    try {
        const userStr = localStorage.getItem('user');
        if (!userStr) return false;

        const user = JSON.parse(userStr);
        return user?.roleName === 'admin';
    } catch (error) {
        console.error('Error checking admin role:', error);
        return false;
    }
};