package com.legacymap.backend.config;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.springframework.lang.NonNull;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.service.JwtUtil;
import com.legacymap.backend.service.UserSessionService;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
public class JwtAuthenticationFilter extends OncePerRequestFilter {

    private final JwtUtil jwtUtil;
    private final UserRepository userRepository;
    private final UserSessionService userSessionService; // ✅ THÊM dòng này

    public JwtAuthenticationFilter(JwtUtil jwtUtil, UserRepository userRepository,
                                   UserSessionService userSessionService) { // ✅ THÊM param
        this.jwtUtil = jwtUtil;
        this.userRepository = userRepository;
        this.userSessionService = userSessionService; // ✅ THÊM dòng này
    }

    @Override
    protected void doFilterInternal(
            @NonNull HttpServletRequest request,
            @NonNull HttpServletResponse response,
            @NonNull FilterChain filterChain
    ) throws ServletException, IOException {

        String path = request.getServletPath();
        String method = request.getMethod();
        log.debug("JwtFilter: {} {}", method, path);

        if (isPublicEndpoint(path, method)) {
            filterChain.doFilter(request, response);
            return;
        }

        String token = null;

        // === THÊM ĐOẠN NÀY – ĐỌC TOKEN TỪ QUERY PARAM (CHO CHAT WIDGET) ===
        String authTokenParam = request.getParameter("authToken");
        if (authTokenParam != null && !authTokenParam.isBlank()) {
            token = authTokenParam;
            log.debug("JWT token found from query param authToken");
        }
        // ===========================================================

        // Lấy token từ cookie trước...
        if (token == null && request.getCookies() != null) {
            for (Cookie cookie : request.getCookies()) {
                if ("JWT_TOKEN".equals(cookie.getName())) {
                    token = cookie.getValue();
                    break;
                }
            }
        }

        // ...hoặc từ header
        if (token == null) {
            String authHeader = request.getHeader("Authorization");
            if (authHeader != null && authHeader.startsWith("Bearer ")) {
                token = authHeader.substring(7);
            }
        }
        if (token == null) {
            log.warn("No JWT token found for protected endpoint: {} {}", method, path);
            filterChain.doFilter(request, response);
            return;
        }

        try {
            UUID userId = jwtUtil.validateToken(token);
            if (userId == null) {
                log.warn("Invalid JWT token for endpoint: {} {}", method, path);
                filterChain.doFilter(request, response);
                return;
            }

            Integer tokenPwdv = jwtUtil.extractPasswordVersion(token);
            Integer currentPwdv = userRepository.findById(userId)
                    .map(u -> u.getPasswordVersion() == null ? 0 : u.getPasswordVersion())
                    .orElse(0);

            if (tokenPwdv == null || !tokenPwdv.equals(currentPwdv)) {
                log.warn("Password version mismatch for user: {}", userId);
                filterChain.doFilter(request, response);
                return;
            }

            if (SecurityContextHolder.getContext().getAuthentication() == null) {
                String roleFromToken = jwtUtil.extractRole(token);
                List<SimpleGrantedAuthority> authorities = buildAuthorities(roleFromToken);

                UsernamePasswordAuthenticationToken authentication =
                        new UsernamePasswordAuthenticationToken(userId.toString(), null, authorities);
                authentication.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
                SecurityContextHolder.getContext().setAuthentication(authentication);

                log.debug("Authenticated user: {} with roles: {}", userId, authorities);

                // ✅ THÊM: Update session activity
                try {
                    userSessionService.updateActivity(token);
                } catch (Exception e) {
                    log.warn("Failed to update session activity: {}", e.getMessage());
                }
            }

        } catch (Exception e) {
            log.error("JWT processing exception: {}", e.getMessage(), e);
        }

        filterChain.doFilter(request, response);
    }

    private List<SimpleGrantedAuthority> buildAuthorities(String role) {
        List<SimpleGrantedAuthority> authorities = new ArrayList<>();
        if (role == null || role.trim().isEmpty()) {
            authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
            return authorities;
        }
        String normalizedRole = role.trim().toUpperCase();
        String roleAuthority = normalizedRole.startsWith("ROLE_") ? normalizedRole : "ROLE_" + normalizedRole;
        authorities.add(new SimpleGrantedAuthority(roleAuthority));
        if ("ROLE_ADMIN".equals(roleAuthority)) {
            authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
        }
        return authorities;
    }

    private boolean isPublicEndpoint(String path, String method) {
        // POST endpoints không cần auth
        if ("POST".equalsIgnoreCase(method) && (
                path.equals("/api/auth/login") ||
                        path.equals("/legacy/api/auth/login") ||
                        path.equals("/api/users/register") ||
                        path.equals("/legacy/api/users/register") ||
                        path.equals("/api/auth/forgot-password") ||
                        path.equals("/api/auth/reset-password")
//                        ||
//                        path.startsWith("/api/support")
        )) return true;

        // GET endpoints không cần auth (including WebSocket handshake)
        if ("GET".equalsIgnoreCase(method) && (
                path.startsWith("/api/auth/verify") ||
                        path.startsWith("/api/trees/shared/") ||
                        path.equals("/api/notifications/stream") ||
                        path.startsWith("/v3/api-docs") ||
                        path.startsWith("/swagger-ui") ||
                        path.startsWith("/actuator") ||
//                        path.startsWith("/api/support") ||
                        path.startsWith("/ws/") ||
                        path.startsWith("/legacy/ws/")
        )) return true;

        // Static resources
        if (path.equals("/") ||
                path.equals("/index.html") ||
                path.startsWith("/oauth2/") ||
                path.startsWith("/login/") ||
                path.startsWith("/assets/") ||
                path.equals("/favicon.ico") ||
                path.equals("/error")) return true;

        // OPTIONS requests
        return "OPTIONS".equalsIgnoreCase(method);
    }
}