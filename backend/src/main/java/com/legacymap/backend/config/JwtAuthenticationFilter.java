package com.legacymap.backend.config;

import com.legacymap.backend.service.JwtUtil;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.lang.NonNull;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

@Slf4j
public class JwtAuthenticationFilter extends OncePerRequestFilter {

    private final JwtUtil jwtUtil;

    public JwtAuthenticationFilter(JwtUtil jwtUtil) {
        this.jwtUtil = jwtUtil;
        log.info("🔧 JwtAuthenticationFilter instantiated with JwtUtil");
    }

    @Override
    protected void doFilterInternal(
            @NonNull HttpServletRequest request,
            @NonNull HttpServletResponse response,
            @NonNull FilterChain filterChain
    ) throws ServletException, IOException {

        String path = request.getRequestURI();
        String method = request.getMethod();

        log.debug("🌐 JwtFilter: {} {}", method, path);

        String authHeader = request.getHeader("Authorization");

        if (authHeader == null) {
            log.debug("⚠️ No Authorization header");
            filterChain.doFilter(request, response);
            return;
        }

        if (!authHeader.startsWith("Bearer ")) {
            log.warn("⚠️ Authorization header does not start with Bearer");
            filterChain.doFilter(request, response);
            return;
        }

        try {
            String token = authHeader.substring(7);
            log.debug("🔍 Processing JWT token: {}...", token.substring(0, Math.min(20, token.length())));

            // 🔥 Validate JWT token (không cần database lookup)
            UUID userId = jwtUtil.validateToken(token);

            if (userId != null && SecurityContextHolder.getContext().getAuthentication() == null) {
                log.info("✅ JWT valid for userId: {}", userId);

                // Tạo authentication object
                UsernamePasswordAuthenticationToken authentication =
                        new UsernamePasswordAuthenticationToken(
                                userId.toString(), // principal
                                null, // credentials
                                List.of(new SimpleGrantedAuthority("ROLE_USER"))
                        );

                authentication.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
                SecurityContextHolder.getContext().setAuthentication(authentication);

                log.info("🔐 Authentication set for user: {}", userId);
            } else if (userId == null) {
                log.error("❌ JWT validation failed");
            }

        } catch (Exception e) {
            log.error("💥 JWT processing exception: {}", e.getMessage(), e);
        }

        filterChain.doFilter(request, response);
    }
}