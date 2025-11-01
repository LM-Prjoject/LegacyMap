import axios from 'axios'

export const http = axios.create({
    baseURL: import.meta.env.VITE_API_BASE_URL || 'http://localhost:8080/legacy/api',
})

// Interceptor cho Spring Boot JWT token
http.interceptors.request.use((config) => {
    const token = localStorage.getItem('authToken');

    // Debug logging
    console.log('HTTP Request:', {
        method: config.method?.toUpperCase(),
        url: `${config.baseURL}${config.url}`,
        hasToken: !!token,
        tokenPreview: token ? `${token.substring(0, 20)}...` : 'NO TOKEN'
    });

    if (token) {
        config.headers.Authorization = `Bearer ${token}`;
        console.log('Token added to Authorization header');
    } else {
        console.warn('No token found - request will be anonymous');
    }

    return config;
});

// Response interceptor để xử lý lỗi
http.interceptors.response.use(
    (response) => {
        console.log('HTTP Response:', {
            status: response.status,
            url: response.config.url,
            data: response.data
        });
        return response;
    },
    (error) => {
        console.error('HTTP Error:', {
            url: error?.config?.url,
            fullUrl: `${error?.config?.baseURL}${error?.config?.url}`,
            method: error?.config?.method?.toUpperCase(),
            status: error?.response?.status,
            statusText: error?.response?.statusText,
            data: error?.response?.data,
            headers: error?.config?.headers
        });

        // Xử lý 401 Unauthorized
        if (error?.response?.status === 401) {
            console.error('Unauthorized - Clearing auth data and redirecting to login');
            localStorage.removeItem('authToken');
            localStorage.removeItem('user');

            // Redirect to login nếu không phải đang ở trang login
            if (!window.location.pathname.includes('/signin')) {
                window.location.href = '/signin';
            }
        }

        // Xử lý 403 Forbidden
        if (error?.response?.status === 403) {
            console.error('Forbidden - User does not have required role (ADMIN)');
            alert('You do not have permission to perform this action. Admin role required.');
        }

        return Promise.reject(error);
    }
);

export default http;