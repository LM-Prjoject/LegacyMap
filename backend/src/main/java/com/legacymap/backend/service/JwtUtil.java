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

@Slf4j
@Service
public class JwtUtil {

    // 🔥 SỬA: Đổi từ jwt.secret thành app.jwt.secret để match với application.properties
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
                .claim("role", role)
                .claim("purpose", "ACCESS")
                .setIssuedAt(now)
                .setExpiration(expiryDate)
                .signWith(getSigningKey(), SignatureAlgorithm.HS256)
                .compact();

        log.info("✅ Generated JWT for userId: {}, expires: {}", userId, expiryDate);
        return token;
    }
}