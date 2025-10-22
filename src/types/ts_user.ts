// src/types/ts_user.ts

export interface User {
    id: string;
    email: string;
    username: string; // 🔥 THÊM: từ database
    roleName: string; // 🔥 SỬA: 'role' -> 'roleName' (đồng bộ với database)
    isVerified: boolean; // 🔥 THÊM: từ database
    isActive: boolean; // 🔥 THÊM: từ database
    isBanned: boolean;
    bannedAt: string | null;
    lastLogin?: string; // 🔥 THÊM: từ database
    createdAt: string;
    updatedAt?: string; // 🔥 THÊM: từ database
    provider?: string; // 🔥 THÊM: từ database

    // Optional fields for display (có thể không có trong response)
    firstName?: string;
    lastName?: string;
    role?: string; // 🔥 GIỮ: để backward compatibility
}

export interface UserProfile {
    id: string;
    userId: string;
    fullName?: string; // 🔥 SỬA: từ database
    clanName?: string; // 🔥 THÊM: từ database
    phone?: string; // 🔥 THÊM: từ database
    gender?: string; // 🔥 THÊM: từ database
    dateOfBirth?: string;
    address?: any; // 🔥 THÊM: từ database (jsonb)
    bio?: string;
    avatarUrl?: string;
    description?: string; // 🔥 THÊM: từ database
    createdAt?: string; // 🔥 THÊM: từ database
    updatedAt?: string; // 🔥 THÊM: từ database
}

export interface UserWithProfile extends User {
    profile?: UserProfile;
}

export interface UserFilters {
    role?: string;
    isBanned?: boolean;
    searchQuery?: string;
}

export interface UserStats {
    totalUsers: number;
    activeUsers: number;
    bannedUsers: number;
    newUsersThisMonth: number;
    adminUsers: number;
    moderatorUsers: number;
}

// Response interfaces từ backend
export interface UserListResponse {
    id: string;
    email: string;
    username: string;
    roleName: string;
    isVerified: boolean;
    isActive: boolean;
    isBanned: boolean;
    bannedAt: string | null;
    lastLogin: string | null;
    createdAt: string;
}

export interface UserDetailResponse {
    id: string;
    email: string;
    username: string;
    roleName: string;
    isVerified: boolean;
    isActive: boolean;
    isBanned: boolean;
    bannedAt: string | null;
    lastLogin: string | null;
    createdAt: string;
    updatedAt: string;
    provider: string;
}

// Thêm interface cho hook return
export interface UseUsersReturn {
    users: User[];
    loading: boolean;
    error: string | null;
    banUser: (userId: string) => Promise<void>;
    unbanUser: (userId: string) => Promise<void>;
    refreshUsers: () => Promise<void>;
}
export interface FamilyTree {
    id: string;
    name: string;
    description?: string;
    createdBy: string; // User ID
    createdByEmail?: string; // 🔥 THÊM: Email của người tạo
    isPublic: boolean;
    shareToken?: string;
    coverImageUrl?: string;
    createdAt: string;
    updatedAt: string;
}