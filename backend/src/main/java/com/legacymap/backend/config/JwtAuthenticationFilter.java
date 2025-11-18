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
import org.springframework.web.filter.OncePerRequestFilter;

import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.service.JwtUtil;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class JwtAuthenticationFilter extends OncePerRequestFilter {

    private final JwtUtil jwtUtil;
    private final UserRepository userRepository;

    public JwtAuthenticationFilter(JwtUtil jwtUtil, UserRepository userRepository) {
        this.jwtUtil = jwtUtil;
        this.userRepository = userRepository;
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

        // Lấy token từ cookie trước, fallback header nếu muốn
        String token = null;
        if (request.getCookies() != null) {
            for (Cookie cookie : request.getCookies()) {
                if ("JWT_TOKEN".equals(cookie.getName())) {
                    token = cookie.getValue();
                    break;
                }
            }
        }

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
                filterChain.doFilter(request, response);
                return;
            }

            Integer tokenPwdv = jwtUtil.extractPasswordVersion(token);
            Integer currentPwdv = userRepository.findById(userId)
                    .map(u -> u.getPasswordVersion() == null ? 0 : u.getPasswordVersion())
                    .orElse(0);
            if (tokenPwdv == null || !tokenPwdv.equals(currentPwdv)) {
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
        if ("POST".equalsIgnoreCase(method) && (
                path.equals("/api/auth/login") ||
                        path.equals("/api/users/register") ||
                        path.equals("/api/auth/forgot-password") ||
                        path.equals("/api/auth/reset-password")
        )) return true;

        if ("GET".equalsIgnoreCase(method) && (
            path.startsWith("/api/auth/verify") ||
                path.startsWith("/api/trees") ||
                path.equals("/api/notifications/stream") ||
                path.startsWith("/v3/api-docs") ||
                path.startsWith("/swagger-ui") ||
                path.startsWith("/actuator")
        )) return true;

        if (path.equals("/") ||
                path.equals("/index.html") ||
                path.startsWith("/oauth2/") ||
                path.startsWith("/login/") ||
                path.startsWith("/assets/") ||
                path.equals("/favicon.ico") ||
                path.equals("/error")) return true;

        return "OPTIONS".equalsIgnoreCase(method);
    }
}
