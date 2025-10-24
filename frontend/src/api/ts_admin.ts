// src/api/ts_admin.ts
import { http } from './http'

// ============================================
// 📝 TYPE DEFINITIONS
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
    createdByUserId: string;
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
// 🔌 ADMIN API CLIENT
// ============================================

export const adminApi = {
    // 👥 USER MANAGEMENT
    async getAllUsers(): Promise<User[]> {
        try {
            console.log('📡 Fetching all users...');
            const { data } = await http.get<ApiResponse<User[]>>('/admin/users');
            console.log('✅ Users fetched:', data.result?.length || 0);
            return data.result;
        } catch (error: any) {
            console.error('❌ Error fetching users:', error.response?.data || error.message);
            throw error;
        }
    },

    async getUserDetail(userId: string): Promise<UserDetail> {
        try {
            console.log('📡 Fetching user detail:', userId);
            const { data } = await http.get<ApiResponse<UserDetail>>(`/admin/users/${userId}`);
            console.log('✅ User detail fetched:', data.result);
            return data.result;
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

            // 🔍 Debug: Log token trước khi gửi request
            const token = localStorage.getItem('authToken');
            console.log('🔑 Token status:', {
                exists: !!token,
                length: token?.length || 0,
                preview: token?.substring(0, 20) + '...'
            });

            const { data } = await http.get<ApiResponse<FamilyTree[]>>('/admin/family-trees');

            console.log('✅ Family trees fetched:', {
                count: data.result?.length || 0,
                trees: data.result
            });

            return data.result;
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
            const { data } = await http.get<ApiResponse<AdminStats>>('/admin/stats');
            console.log('✅ Admin stats fetched:', data.result);
            return data.result;
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