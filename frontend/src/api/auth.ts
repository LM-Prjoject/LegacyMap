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

export const authApi = {
    // ⛳ Trả về data (ApiResponse<LoginResponse>), KHÔNG trả AxiosResponse
    async login(payload: LoginRequest): Promise<ApiResponse<LoginResponse>> {
        try {
            console.log('🔐 Đang đăng nhập...', { identifier: payload.identifier })
            const { data } = await http.post<ApiResponse<LoginResponse>>('/auth/login', payload)
            console.log('✅ Đăng nhập thành công:', data)
            return data
        } catch (error: any) {
            console.error('❌ Lỗi đăng nhập:', {
                message: error.message,
                response: error.response?.data,
                status: error.response?.status,
            })
            throw error
        }
    },

    // ⛳ Cũng trả về data đã bóc
    async register(payload: UserCreateRequest): Promise<ApiResponse<any>> {
        try {
            console.log('📝 Đang gửi request đăng ký...', {
                username: payload.username,
                email: payload.email,
                fullName: payload.fullName,
            })
            const { data } = await http.post<ApiResponse<any>>('/users/register', payload)
            console.log('✅ Response từ server:', data)
            return data
        } catch (error: any) {
            console.error('❌ Lỗi đăng ký:', {
                message: error.message,
                response: error.response?.data,
                status: error.response?.status,
            })
            throw error
        }
    },

    // 🔥 Cập nhật để trả về thông tin đăng nhập tự động
    async verifyEmail(token: string): Promise<ApiResponse<VerifyEmailResult>> {
        console.log('🔍 Đang xác minh email với token:', token.substring(0, 10) + '...');
        const { data } = await http.get<ApiResponse<VerifyEmailResult>>(`/api/auth/verify?token=${encodeURIComponent(token)}`);
        console.log('✅ Xác minh thành công:', data);
        return data;
    },

    // 🔑 Thống nhất key token với chỗ bạn set trong SignIn.tsx là 'authToken'
    logout() {
        localStorage.removeItem('authToken');
        localStorage.removeItem('user');
    },

    // Helper function để lưu token và user
    saveAuthData(token: string, user: any) {
        localStorage.setItem('authToken', token);
        localStorage.setItem('user', JSON.stringify(user));
        console.log('💾 Đã lưu thông tin đăng nhập vào localStorage');
    }
}