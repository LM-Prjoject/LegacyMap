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

export interface ApiResponse<T> {
    success: boolean;
    code: number;
    message: string;
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

    async verifyEmail(token: string): Promise<ApiResponse<any>> {
        const { data } = await http.get<ApiResponse<any>>(`/auth/verify?token=${encodeURIComponent(token)}`)
        return data
    },

    // 🔑 Thống nhất key token với chỗ bạn set trong SignIn.tsx là 'auth_token'
    logout() {
        localStorage.removeItem('auth_token')
        localStorage.removeItem('user')
    },
}
