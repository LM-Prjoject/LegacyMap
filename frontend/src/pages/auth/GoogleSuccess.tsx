import { useEffect } from "react";
import { useNavigate } from "react-router-dom";
import { toast } from "react-toastify";

export default function GoogleSuccess() {
    const navigate = useNavigate();

    useEffect(() => {
        const params = new URLSearchParams(window.location.search);
        const token = params.get("token");
        const returnUrl = params.get("returnUrl"); // Có thể có từ backend

        if (token) {
            localStorage.setItem("authToken", token);

            // Nếu backend gửi kèm returnUrl → ưu tiên dùng nó
            if (returnUrl) {
                localStorage.setItem('redirectAfterLogin', returnUrl);
            }

            // Gọi API để lấy thông tin user
            fetch(`${import.meta.env.VITE_API_BASE_URL || "http://localhost:8080/legacy"}/auth/me`, {
                headers: { Authorization: `Bearer ${token}` },
            })
                .then(res => {
                    if (!res.ok) throw new Error(`HTTP error! status: ${res.status}`);
                    return res.json();
                })
                .then(user => {
                    localStorage.setItem("user", JSON.stringify(user));
                    toast.success("Đăng nhập Google thành công!");

                    // ƯU TIÊN: redirectAfterLogin (do SharedTreeView hoặc backend set)
                    const savedRedirectUrl = localStorage.getItem('redirectAfterLogin');

                    if (savedRedirectUrl) {
                        localStorage.removeItem('redirectAfterLogin');

                        // Validate URL để tránh open redirect
                        try {
                            const url = new URL(savedRedirectUrl, window.location.origin);
                            if (url.origin === window.location.origin) {
                                navigate(savedRedirectUrl);
                            } else {
                                console.warn('Invalid redirect URL origin');
                                navigate("/");
                            }
                        } catch {
                            console.warn('Invalid redirect URL format');
                            navigate("/");
                        }
                    } else {
                        navigate("/");
                    }
                })
                .catch((error) => {
                    console.error("Error fetching user info:", error);
                    toast.error("Lỗi khi lấy thông tin người dùng!");

                    // Dù lỗi → vẫn đã có token → vẫn redirect nếu có redirectAfterLogin
                    const savedRedirectUrl = localStorage.getItem('redirectAfterLogin');

                    if (savedRedirectUrl) {
                        localStorage.removeItem('redirectAfterLogin');
                        try {
                            const url = new URL(savedRedirectUrl, window.location.origin);
                            if (url.origin === window.location.origin) {
                                navigate(savedRedirectUrl);
                            } else {
                                navigate("/");
                            }
                        } catch {
                            navigate("/");
                        }
                    } else {
                        navigate("/signin");
                    }
                });
        } else {
            toast.error("Không nhận được token đăng nhập.");
            navigate("/signin");
        }
    }, [navigate]);

    return null ;
}