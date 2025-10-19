import { useEffect } from "react";
import { useNavigate } from "react-router-dom";
import { toast } from "react-toastify";

export default function GoogleSuccess() {
    const navigate = useNavigate();

    useEffect(() => {
        const params = new URLSearchParams(window.location.search);
        const token = params.get("token");

        if (token) {
            localStorage.setItem("authToken", token);

            // Gọi API backend để lấy thông tin user
            fetch(`${import.meta.env.VITE_API_BASE_URL || "http://localhost:8080/legacy"}/auth/me`, {
                headers: { Authorization: `Bearer ${token}` },
            })
                .then(res => res.json())
                .then(user => {
                    localStorage.setItem("user", JSON.stringify(user));
                    toast.success("Đăng nhập Google thành công!");
                    navigate("/");
                })
                .catch(() => {
                    toast.error("Lỗi khi lấy thông tin người dùng!");
                    navigate("/signin");
                });
        } else {
            toast.error("Không nhận được token đăng nhập.");
            navigate("/signin");
        }
    }, [navigate]);

    return <p>Đang xử lý đăng nhập Google...</p>;
}