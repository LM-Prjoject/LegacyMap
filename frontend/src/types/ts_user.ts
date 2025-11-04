export interface User {
    id: string;
    email: string;
    username: string;
    roleName: string;
    isVerified: boolean;
    isActive: boolean;
    isBanned: boolean;
    bannedAt: string | null;
    lastLogin?: string;
    createdAt: string;
    updatedAt?: string;
    provider?: string;
    passwordChangedAt?: string | null;

    // Optional fields for display (có thể không có trong response)
    firstName?: string;
    lastName?: string;
    role?: string;
}

export interface UserProfile {
    id: string;
    userId: string;
    fullName?: string;
    clanName?: string;
    phone?: string;
    gender?: string;
    dateOfBirth?: string;
    address?: any;
    bio?: string;
    avatarUrl?: string;
    description?: string;
    createdAt?: string;
    updatedAt?: string;
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
    createdBy: string;
    createdByEmail?: string;
    isPublic: boolean;
    shareToken?: string;
    coverImageUrl?: string;
    createdAt: string;
    updatedAt: string;
}