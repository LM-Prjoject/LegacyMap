package com.legacymap.backend.service;

import com.legacymap.backend.entity.User;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jws;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.security.Keys;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.crypto.SecretKey;
import java.time.Instant;
import java.util.Date;

@Service
public class AuthenticationService {
    private final SecretKey key;
    private final long accessTtlMillis;
    private final long setupTtlMillis;

    public AuthenticationService(
            @Value("${app.jwt.secret}") String secret,
            @Value("${app.jwt.access-ttl-ms:604800000}") long accessTtlMillis, // 7 ngày
            @Value("${app.jwt.setup-ttl-ms:900000}") long setupTtlMillis       // 15 phút
    ) {
        this.key = Keys.hmacShaKeyFor(secret.getBytes());
        this.accessTtlMillis = accessTtlMillis;
        this.setupTtlMillis = setupTtlMillis;
    }

    public String generateAccessToken(User user) {
        Instant now = Instant.now();
        return Jwts.builder()
                .setSubject(user.getId().toString())
                .claim("email", user.getEmail())
                .claim("role", user.getRoleName())
                .claim("purpose", "ACCESS")
                .setIssuedAt(Date.from(now))
                .setExpiration(Date.from(now.plusMillis(accessTtlMillis)))
                .signWith(key, SignatureAlgorithm.HS256)
                .compact();
    }

    public String generateSetupPasswordToken(User user) {
        Instant now = Instant.now();
        return Jwts.builder()
                .setSubject(user.getId().toString())
                .claim("email", user.getEmail())
                .claim("purpose", "SET_PASSWORD")
                .setIssuedAt(Date.from(now))
                .setExpiration(Date.from(now.plusMillis(setupTtlMillis)))
                .signWith(key, SignatureAlgorithm.HS256)
                .compact();
    }

    public Jws<Claims> parse(String jwt) {
        return Jwts.parserBuilder().setSigningKey(key).build().parseClaimsJws(jwt);
    }
}
