import axios from 'axios'
import {showToast} from "@/lib/toast.ts";

export const http = axios.create({
    baseURL: import.meta.env.VITE_API_BASE_URL || 'http://localhost:8080/legacy/api',
})

// Interceptor cho Spring Boot JWT token
http.interceptors.request.use((config) => {
    const token = localStorage.getItem('authToken');

    if (token) {
        config.headers.Authorization = `Bearer ${token}`;
    } else {
        console.warn('No token found - request will be anonymous');
    }

    return config;
});

// Response interceptor để xử lý lỗi
http.interceptors.response.use(
    (response) => {
        const data = response.data;

        // Nếu backend dùng chuẩn ApiResponse { success, code, message, result }
        if (data && typeof data === "object" && "success" in data && data.success === false) {
            const err: any = new Error(data.message || "Request failed");
            err.code = data.code;
            err.apiResponse = data;
            err.response = response; // giữ để debug giống axios error
            return Promise.reject(err);
        }

        console.log("HTTP Response:", {
            status: response.status,
            url: response.config.url,
            data: response.data,
        });

        return response;
    },
    (error) => {
        console.error("HTTP Error:", {
            url: error?.config?.url,
            fullUrl: `${error?.config?.baseURL}${error?.config?.url}`,
            method: error?.config?.method?.toUpperCase(),
            status: error?.response?.status,
            statusText: error?.response?.statusText,
            data: error?.response?.data,
            headers: error?.config?.headers,
        });

        if (error?.response?.status === 401) {
            localStorage.removeItem("authToken");
            localStorage.removeItem("user");
            if (!window.location.pathname.includes("/signin")) {
                window.location.href = "/signin";
            }
        }

        if (error?.response?.status === 403) {
            showToast.warning("Không đủ quyền truy cập.");
        }

        return Promise.reject(error);
    }
);


export default http;