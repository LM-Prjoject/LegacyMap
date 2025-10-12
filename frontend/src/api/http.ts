import axios from 'axios'

// Sửa baseURL để match với context-path /legacy
export const http = axios.create({
    baseURL: import.meta.env.VITE_API_BASE_URL || 'http://localhost:8080/legacy/api'
})

// Interceptor cho Spring Boot JWT token
http.interceptors.request.use((config) => {
    const token = localStorage.getItem('authToken') // Token từ Spring Boot
    if (token) {
        config.headers.Authorization = `Bearer ${token}`
    }
    return config
})

// Response interceptor để xử lý lỗi
http.interceptors.response.use(
    (response) => response,
    (error) => {
        if (error.response?.status === 401) {
            localStorage.removeItem('authToken')
            localStorage.removeItem('user')
            window.location.href = '/signin'
        }
        return Promise.reject(error)
    }
)