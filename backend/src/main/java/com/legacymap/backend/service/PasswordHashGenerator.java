package com.legacymap.backend.service;

import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;

public class PasswordHashGenerator {
    public static void main(String[] args) {
        BCryptPasswordEncoder encoder = new BCryptPasswordEncoder(12);

        // Password bạn muốn hash
        String rawPassword = "admin123";

        // Tạo hash mới
        String hashedPassword = encoder.encode(rawPassword);

        System.out.println("================================");
        System.out.println("Raw Password: " + rawPassword);
        System.out.println("Hashed Password: " + hashedPassword);
        System.out.println("================================");
        System.out.println("\nSQL để update:");
        System.out.println("UPDATE users SET password_hash = '" + hashedPassword + "' WHERE email = 'admin@legacymap.com';");
        System.out.println("================================");

        // Verify hash có hoạt động không
        boolean matches = encoder.matches(rawPassword, hashedPassword);
        System.out.println("Verification test: " + (matches ? "✅ SUCCESS" : "❌ FAILED"));
    }
}