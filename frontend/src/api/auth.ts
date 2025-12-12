import { http } from './http'

export interface LoginRequest {
    identifier: string;
    password: string;
}

export interface UserCreateRequest {
    username: string;
    email: string;
    password: string;
    fullName: string;
    clanName?: string;
    gender?: string;
    phone?: string;
    dob?: string;
}

export interface LoginResponse {
    user: any;
    token: string;
}

export interface VerifyEmailResult {
    user: any;
    token: string;
    message?: string;
    action?: string;
}

export interface ApiResponse<T> {
    success: boolean;
    code?: number;
    message?: string;
    result: T;
}

export interface Address {
    city?: string;
    ward?: string;
    houseNumber?: string;
}
export interface UserProfile {
    fullName?: string;
    clanName?: string;
    gender?: 'male' | 'female' | 'other' | '';
    phone?: string;
    dob?: string;
    address?: Address | null;
    avatarUrl?: string;
    description?: string; // Thêm dòng này
}
export interface User {
    id: string;
    email: string;
    username: string;
    roleName?: string;
    isActive?: boolean;
    isVerified?: boolean;
    profile?: UserProfile;
    provider?: string;
}

export const authApi = {
    // Trả về data (ApiResponse<LoginResponse>), KHÔNG trả AxiosResponse
    async login(payload: LoginRequest): Promise<ApiResponse<LoginResponse>> {
        try {
            console.log('Đang đăng nhập...', {
                identifier: payload.identifier,
                password_length: payload.password.length // Thêm để debug
            });

            // THÊM: Log full URL
            const fullUrl = http.defaults.baseURL + '/auth/login';
            console.log('URL:', fullUrl);

            const response = await http.post<ApiResponse<LoginResponse>>('/auth/login', payload);

            console.log('Đăng nhập thành công:', response.data);
            return response.data;
        } catch (error: any) {
            console.error('Lỗi đăng nhập chi tiết:', {
                message: error.message,
                status: error.response?.status,
                data: error.response?.data, // Chi tiết lỗi từ backend
                config: {
                    url: error.config?.url,
                    method: error.config?.method,
                    data: error.config?.data
                }
            });
            throw error;
        }
    },

    // Cũng trả về data đã bóc
    async register(payload: UserCreateRequest): Promise<ApiResponse<any>> {
        try {
            console.log('Đang gửi request đăng ký...', {
                username: payload.username,
                email: payload.email,
                fullName: payload.fullName,
            })
            const { data } = await http.post<ApiResponse<any>>('/users/register', payload)
            console.log('Response từ server:', data)
            return data
        } catch (error: any) {
            console.error('Lỗi đăng ký:', {
                message: error.message,
                response: error.response?.data,
                status: error.response?.status,
            })
            throw error
        }
    },

    async verifyEmail(token: string): Promise<ApiResponse<VerifyEmailResult>> {
        console.log('Đang xác minh email với token:', token.substring(0, 10) + '...');
        const { data } = await http.get<ApiResponse<VerifyEmailResult>>(`/auth/verify?token=${encodeURIComponent(token)}`);
        console.log('Xác minh thành công:', data);
        return data;
    },

    logout() {
        localStorage.removeItem('authToken');
        localStorage.removeItem('user');
    },

    saveAuthData(token: string, user: any) {
        localStorage.setItem('authToken', token);
        localStorage.setItem('user', JSON.stringify(user));
        console.log('Đã lưu thông tin đăng nhập vào localStorage');
    },
    async getMe(): Promise<User> {
        const { data } = await http.get<User>('/auth/me');
        return data;
    },

    async getUser(id: string): Promise<User> {
        const { data } = await http.get<ApiResponse<User>>(`/users/${id}`);
        return data.result;
    },

    async updateUser(id: string, profile: UserProfile): Promise<UserProfile> {
        const { data } = await http.put<ApiResponse<UserProfile>>(`/users/${id}`, profile);
        return data.result;
    },

    async changePassword(payload: { currentPassword: string; newPassword: string }) {
        const { data: res } = await http.post<ApiResponse<null>>("/auth/change-password", payload);

        if (!res.success) {
            throw new Error(res.message || "Đổi mật khẩu thất bại");
        }

        return res;
    },

    async getLoginStatus(identifier: string): Promise<ApiResponse<any>> {
        const { data } = await http.get<ApiResponse<any>>(
            `/auth/status?identifier=${encodeURIComponent(identifier)}`
        );
        return data;
    },

    async createUnbanRequest(identifier: string, reason: string): Promise<ApiResponse<any>> {
        const { data } = await http.post<ApiResponse<any>>('/auth/unban-requests', {
            identifier,
            reason,
        });
        return data;
    }
};