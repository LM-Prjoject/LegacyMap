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
import java.util.ArrayList;
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

        // Skip JWT filter for public endpoints
        if (isPublicEndpoint(path, method)) {
            log.debug("✅ Public endpoint, skipping JWT filter: {}", path);
            filterChain.doFilter(request, response);
            return;
        }

        String authHeader = request.getHeader("Authorization");

        if (authHeader == null) {
            log.warn("⚠️ No Authorization header for protected endpoint: {} {}", method, path);
            filterChain.doFilter(request, response);
            return;
        }

        if (!authHeader.startsWith("Bearer ")) {
            log.warn("⚠️ Authorization header does not start with Bearer: {}", authHeader.substring(0, Math.min(20, authHeader.length())));
            filterChain.doFilter(request, response);
            return;
        }

        try {
            String token = authHeader.substring(7);
            log.info("🔐 Processing JWT token for {} {}", method, path);

            // 🔥 CRITICAL: Debug token for admin endpoints
            if (path.contains("/admin/")) {
                log.info("🎯 ADMIN ENDPOINT DETECTED - Debugging token...");
                jwtUtil.debugToken(token);
            }

            // 🔥 Validate JWT token
            UUID userId = jwtUtil.validateToken(token);

            if (userId == null) {
                log.error("❌ JWT validation failed - token is invalid or expired");
                filterChain.doFilter(request, response);
                return;
            }

            if (SecurityContextHolder.getContext().getAuthentication() != null) {
                log.debug("⚠️ Authentication already set, skipping");
                filterChain.doFilter(request, response);
                return;
            }

            log.info("✅ JWT valid for userId: {}", userId);

            // 🔥 Extract role from token
            String roleFromToken = jwtUtil.extractRole(token);
            log.info("🔍 Role extracted from token: '{}'", roleFromToken);

            // 🔥 Build authorities list
            List<SimpleGrantedAuthority> authorities = buildAuthorities(roleFromToken);

            log.info("🎯 Final authorities for user {}: {}", userId, authorities);

            // Create authentication object
            UsernamePasswordAuthenticationToken authentication =
                    new UsernamePasswordAuthenticationToken(
                            userId.toString(), // principal
                            null, // credentials
                            authorities
                    );

            authentication.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
            SecurityContextHolder.getContext().setAuthentication(authentication);

            log.info("✅ Authentication set for user: {} with authorities: {}", userId, authorities);

            // 🔥 Special check for admin endpoints
            if (path.contains("/admin/")) {
                boolean hasAdminRole = authorities.stream()
                        .anyMatch(auth -> auth.getAuthority().equals("ROLE_ADMIN"));

                if (hasAdminRole) {
                    log.info("🎯 ✅ ADMIN ACCESS GRANTED for user {} to path: {}", userId, path);
                } else {
                    log.error("🚫 ❌ ADMIN ACCESS DENIED for user {} to path: {} - Missing ROLE_ADMIN", userId, path);
                    log.error("🚫 User authorities: {}", authorities);
                }
            }

        } catch (Exception e) {
            log.error("💥 JWT processing exception: {}", e.getMessage(), e);
        }

        filterChain.doFilter(request, response);
    }

    /**
     * 🔥 Build authorities from role string
     */
    private List<SimpleGrantedAuthority> buildAuthorities(String role) {
        List<SimpleGrantedAuthority> authorities = new ArrayList<>();

        if (role == null || role.trim().isEmpty()) {
            log.warn("⚠️ Role is null or empty, defaulting to ROLE_USER");
            authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
            return authorities;
        }

        // Normalize role (remove whitespace, convert to uppercase)
        String normalizedRole = role.trim().toUpperCase();
        log.debug("🔄 Normalized role: '{}' -> '{}'", role, normalizedRole);

        // Add role with ROLE_ prefix if not present
        String roleAuthority;
        if (normalizedRole.startsWith("ROLE_")) {
            roleAuthority = normalizedRole;
        } else {
            roleAuthority = "ROLE_" + normalizedRole;
        }

        authorities.add(new SimpleGrantedAuthority(roleAuthority));
        log.debug("✅ Added authority: {}", roleAuthority);

        // 🔥 If role is ADMIN, also add ROLE_USER for backward compatibility
        if (roleAuthority.equals("ROLE_ADMIN")) {
            authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
            log.debug("✅ Added ROLE_USER for admin (backward compatibility)");
        }

        return authorities;
    }

    /**
     * 🔥 Check if endpoint is public
     */
    private boolean isPublicEndpoint(String path, String method) {
        // Public API endpoints
        if (path.startsWith("/api/auth/") ||
                path.startsWith("/api/users/register") ||
                path.startsWith("/api/trees/") ||
                path.startsWith("/v3/api-docs") ||
                path.startsWith("/swagger-ui") ||
                path.startsWith("/actuator") ||
                path.startsWith("/api/debug")) {
            return true;
        }

        // Public web endpoints
        if (path.equals("/") ||
                path.equals("/index.html") ||
                path.startsWith("/oauth2/") ||
                path.startsWith("/login/") ||
                path.startsWith("/assets/") ||
                path.equals("/favicon.ico") ||
                path.equals("/error")) {
            return true;
        }

        // OPTIONS requests for CORS
        if ("OPTIONS".equalsIgnoreCase(method)) {
            return true;
        }

        return false;
    }
}