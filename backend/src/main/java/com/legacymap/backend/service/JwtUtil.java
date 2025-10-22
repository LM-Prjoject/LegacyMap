package com.legacymap.backend.service;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.security.Keys;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.nio.charset.StandardCharsets;
import java.security.Key;
import java.util.Date;
import java.util.UUID;
import java.util.function.Function;

@Slf4j
@Service
public class JwtUtil {

    @Value("${app.jwt.secret:my-super-secret-key-for-jwt-must-be-at-least-256-bits-long-hs256-algorithm}")
    private String jwtSecret;

    @Value("${app.jwt.expiration:604800000}") // 7 days in milliseconds
    private long jwtExpiration;

    /**
     * Get signing key from secret
     */
    private Key getSigningKey() {
        byte[] keyBytes = jwtSecret.getBytes(StandardCharsets.UTF_8);
        log.debug("🔑 JWT Secret length: {} bytes", keyBytes.length);
        return Keys.hmacShaKeyFor(keyBytes);
    }

    /**
     * Validate JWT token and return userId
     */
    public UUID validateToken(String token) {
        try {
            Claims claims = Jwts.parserBuilder()
                    .setSigningKey(getSigningKey())
                    .build()
                    .parseClaimsJws(token)
                    .getBody();

            // Check expiration
            if (claims.getExpiration().before(new Date())) {
                log.warn("❌ Token expired at: {}", claims.getExpiration());
                return null;
            }

            // Get userId from 'sub' claim
            String subject = claims.getSubject();
            if (subject == null || subject.isEmpty()) {
                log.warn("❌ Token has no subject");
                return null;
            }

            UUID userId = UUID.fromString(subject);
            log.info("✅ JWT valid for userId: {}", userId);
            return userId;

        } catch (io.jsonwebtoken.ExpiredJwtException e) {
            log.error("❌ JWT expired: {}", e.getMessage());
            return null;
        } catch (io.jsonwebtoken.SignatureException e) {
            log.error("❌ Invalid JWT signature: {}", e.getMessage());
            return null;
        } catch (Exception e) {
            log.error("❌ JWT validation failed: {}", e.getMessage());
            return null;
        }
    }

    /**
     * Generate JWT token for user
     */
    public String generateToken(UUID userId, String email, String role) {
        Date now = new Date();
        Date expiryDate = new Date(now.getTime() + jwtExpiration);

        String token = Jwts.builder()
                .setSubject(userId.toString())
                .claim("email", email)
                .claim("role", role) // 🔥 GIỮ NGUYÊN để frontend hoạt động
                .claim("role_name", role) // 🔥 ĐỒNG BỘ với database
                .claim("purpose", "ACCESS")
                .setIssuedAt(now)
                .setExpiration(expiryDate)
                .signWith(getSigningKey(), SignatureAlgorithm.HS256)
                .compact();

        log.info("✅ Generated JWT for userId: {}, role: {}, expires: {}", userId, role, expiryDate);
        return token;
    }
    /**
     * 🔥 THÊM MỚI: Extract claim từ token
     */
    public <T> T extractClaim(String token, Function<Claims, T> claimsResolver) {
        try {
            final Claims claims = extractAllClaims(token);
            return claimsResolver.apply(claims);
        } catch (Exception e) {
            log.error("❌ Error extracting claim from token: {}", e.getMessage());
            return null;
        }
    }

    /**
     * 🔥 THÊM MỚI: Extract tất cả claims
     */
    private Claims extractAllClaims(String token) {
        return Jwts.parserBuilder()
                .setSigningKey(getSigningKey())
                .build()
                .parseClaimsJws(token)
                .getBody();
    }

    /**
     * 🔥 THÊM MỚI: Extract role từ token
     */
    public String extractRole(String token) {
        return extractClaim(token, claims -> {
            // Thử extract từ field "role"
            Object roleObj = claims.get("role");
            if (roleObj != null) {
                String role = roleObj.toString();
                log.debug("🔍 Extracted role from token: {}", role);
                return role;
            }

            // Fallback: thử từ field "role_name"
            Object roleNameObj = claims.get("role_name");
            if (roleNameObj != null) {
                String role = roleNameObj.toString();
                log.debug("🔍 Extracted role_name from token: {}", role);
                return role;
            }

            log.debug("🔍 No role claim found in token, using default USER");
            return "USER"; // Default role
        });
    }

    /**
     * 🔥 THÊM MỚI: Extract email từ token
     */
    public String extractEmail(String token) {
        return extractClaim(token, claims -> {
            Object emailObj = claims.get("email");
            return emailObj != null ? emailObj.toString() : null;
        });
    }

    /**
     * 🔥 THÊM MỚI: Kiểm tra nếu user có role ADMIN
     */
    public boolean isAdmin(String token) {
        String role = extractRole(token);
        return "ADMIN".equalsIgnoreCase(role);
    }

    /**
     * 🔥 THÊM MỚI: Debug token claims
     */
    public void debugToken(String token) {
        try {
            Claims claims = extractAllClaims(token);
            log.info("🔍 Token Debug - Subject: {}", claims.getSubject());
            log.info("🔍 Token Debug - Role: {}", claims.get("role"));
            log.info("🔍 Token Debug - Role Name: {}", claims.get("role_name"));
            log.info("🔍 Token Debug - Email: {}", claims.get("email"));
            log.info("🔍 Token Debug - Expiration: {}", claims.getExpiration());
            log.info("🔍 Token Debug - All Claims: {}", claims);
        } catch (Exception e) {
            log.error("❌ Token debug failed: {}", e.getMessage());
        }
    }

    /**
     * 🔥 THÊM MỚI: Validate token for admin access
     */
    public boolean validateAdminToken(String token) {
        try {
            UUID userId = validateToken(token);
            if (userId == null) {
                return false;
            }

            boolean isAdmin = isAdmin(token);
            log.info("🔍 Admin validation for user {}: {}", userId, isAdmin);

            if (!isAdmin) {
                log.warn("🚫 User {} does not have ADMIN role", userId);
            }

            return isAdmin;
        } catch (Exception e) {
            log.error("❌ Admin token validation failed: {}", e.getMessage());
            return false;
        }
    }
}