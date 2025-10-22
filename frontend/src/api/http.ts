import axios from 'axios'


const API_BASE_URL =
  import.meta.env.VITE_API_BASE_URL ??
  (window.location.hostname === "localhost"
    ? "http://localhost:8080/legacy/api"
    : "https://legacymap.onrender.com/legacy/api");

// Sửa baseURL để match với context-path /legacy
export const http = axios.create({
  baseURL: API_BASE_URL,
  headers: {
    "Content-Type": "application/json",
  },
});

// Interceptor cho Spring Boot JWT token
http.interceptors.request.use((config) => {
    const token = localStorage.getItem('authToken');
    if (token) {
        config.headers.Authorization = `Bearer ${token}`;
    }
    return config;
});

// Response interceptor để xử lý lỗi
http.interceptors.response.use(
    r => r,
    (error) => {
        console.error('401 detail:', {
            url: error?.config?.baseURL + error?.config?.url,
            status: error?.response?.status,
            data: error?.response?.data,
        });
        return Promise.reject(error);
    }
);