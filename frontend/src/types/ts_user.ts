export interface User {
    id: string;
    email: string;
    username: string;
    roleName: string;
    isVerified: boolean;
    isActive: boolean;
    isBanned: boolean;
    bannedAt: string | null;
    lastLogin?: string;  // ✅ Đã có
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
    memberCount?: number;  // ✅ Thêm field này
}

// ============================================
// ✅ HELPER FUNCTIONS (THÊM VÀO CUỐI FILE)
// ============================================

/**
 * Lấy tên hiển thị của user
 */
export const getUserDisplayName = (user: User | null): string => {
    if (!user) return 'User';

    if (user.firstName && user.lastName) {
        return `${user.firstName} ${user.lastName}`;
    }
    if (user.firstName) return user.firstName;
    if (user.lastName) return user.lastName;

    return user.username || user.email?.split('@')[0] || 'User';
};

/**
 * Lấy chữ cái đầu của tên user
 */
export const getUserInitials = (user: User | null): string => {
    if (!user) return 'U';

    const first = user.firstName?.charAt(0) || '';
    const last = user.lastName?.charAt(0) || '';

    if (first || last) return (first + last).toUpperCase();

    return user.email?.charAt(0).toUpperCase() || 'U';
};

/**
 * Kiểm tra user có đang online không (đăng nhập trong 5 phút gần đây)
 */
export const isUserOnline = (user: User | null): boolean => {
    if (!user || !user.lastLogin) return false;

    const lastLoginTime = new Date(user.lastLogin).getTime();
    const now = new Date().getTime();
    const oneMinute = 1 * 60 * 1000; // 1 phút

    return (now - lastLoginTime) < oneMinute;
};

/**
 * Format ngày tháng theo định dạng Việt Nam
 */
export const formatUserDate = (dateString: string | undefined | null): string => {
    if (!dateString) return 'N/A';

    try {
        return new Date(dateString).toLocaleDateString('vi-VN', {
            year: 'numeric',
            month: 'long',
            day: 'numeric',
        });
    } catch {
        return 'N/A';
    }
};

/**
 * Lấy thời gian đăng nhập gần nhất (relative time)
 */
export const getLastLoginText = (user: User | null): string => {
    if (!user || !user.lastLogin) return 'Chưa đăng nhập';

    const lastLoginTime = new Date(user.lastLogin).getTime();
    const now = new Date().getTime();
    const diffMinutes = Math.floor((now - lastLoginTime) / (60 * 1000));

    if (diffMinutes < 5) return '🟢 Đang online';
    if (diffMinutes < 60) return `${diffMinutes} phút trước`;

    const diffHours = Math.floor(diffMinutes / 60);
    if (diffHours < 24) return `${diffHours} giờ trước`;

    const diffDays = Math.floor(diffHours / 24);
    if (diffDays < 7) return `${diffDays} ngày trước`;

    return formatUserDate(user.lastLogin);
};

/**
 * Kiểm tra user có phải admin không
 */
export const isAdmin = (user: User | null): boolean => {
    if (!user) return false;
    return user.roleName?.toLowerCase() === 'admin' || user.role?.toLowerCase() === 'admin';
};

/**
 * Kiểm tra user có phải moderator không
 */
export const isModerator = (user: User | null): boolean => {
    if (!user) return false;
    return user.roleName?.toLowerCase() === 'moderator' || user.role?.toLowerCase() === 'moderator';
};