package com.legacymap.backend.config;

import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.service.JwtUtil;
import com.legacymap.backend.service.UserSessionService;
import com.legacymap.backend.service.google.CustomOAuth2UserService;
import com.legacymap.backend.service.google.CustomOidcUserService;
import com.legacymap.backend.service.google.OAuth2SuccessHandler;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;

@Slf4j
@Configuration
public class SecurityConfig {

    private final JwtUtil jwtUtil;
    private final UserRepository userRepository;
    private final UserSessionService userSessionService;  // ✅ THÊM DÒNG NÀY

    @Value("${app.frontend.url:http://localhost:3000}")
    private String frontendUrl;

    public SecurityConfig(JwtUtil jwtUtil, UserRepository userRepository, UserSessionService userSessionService) {  // ✅ THÊM THAM SỐ
        this.jwtUtil = jwtUtil;
        this.userRepository = userRepository;
        this.userSessionService = userSessionService;  // ✅ THÊM DÒNG NÀY
    }

    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder(12);
    }

    @Bean
    CorsConfigurationSource corsConfigurationSource() {
        CorsConfiguration c = new CorsConfiguration();
        c.setAllowedOriginPatterns(Arrays.asList(
                "https://legacy-map-ebon.vercel.app",
                "http://localhost:3000",
                "http://localhost:5173",
                "http://localhost:5174",
                "http://localhost:5175"
        ));
        c.setAllowedMethods(Arrays.asList("GET", "POST", "PUT", "DELETE", "OPTIONS", "PATCH"));
        c.setAllowedHeaders(Arrays.asList("*"));
        c.setAllowCredentials(true);
        c.setExposedHeaders(Arrays.asList("Authorization"));

        UrlBasedCorsConfigurationSource s = new UrlBasedCorsConfigurationSource();
        s.registerCorsConfiguration("/**", c);
        return s;
    }

    @Bean
    @Order(1)
    SecurityFilterChain apiChain(HttpSecurity http) throws Exception {
        log.info("Configuring API Security Chain");

        JwtAuthenticationFilter jwtFilter = new JwtAuthenticationFilter(jwtUtil, userRepository, userSessionService);  // ✅ SỬA DÒNG NÀY

        http
                .securityMatcher("/api/**", "/legacy/api/**")
                .csrf(csrf -> csrf.disable())
                .cors(cors -> cors.configurationSource(corsConfigurationSource()))
                .sessionManagement(sm -> sm.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
                .addFilterBefore(jwtFilter, UsernamePasswordAuthenticationFilter.class)

                .authorizeHttpRequests(auth -> auth
                        .requestMatchers(HttpMethod.OPTIONS, "/**").permitAll()
                        .requestMatchers(HttpMethod.HEAD, "/**").permitAll()

                        // ==================== PUBLIC ENDPOINTS (phải đặt TRƯỚC) ====================

                        // Authentication endpoints
                        .requestMatchers(HttpMethod.POST,
                                "/api/users/register",
                                "/legacy/api/users/register",
                                "/api/auth/login",
                                "/legacy/api/auth/login",
                                "/api/auth/forgot-password",
                                "/legacy/api/auth/forgot-password",
                                "/api/auth/reset-password",
                                "/legacy/api/auth/reset-password",
                                "/api/auth/password/forgot",
                                "/legacy/api/auth/password/forgot",
                                "/api/auth/password/reset",
                                "/legacy/api/auth/password/reset"
                        ).permitAll()

                        .requestMatchers(HttpMethod.GET,
                                "/api/auth/verify/**",
                                "/legacy/api/auth/verify/**"
                        ).permitAll()

                        .requestMatchers(HttpMethod.POST,
                                "/api/trees/**",
                                "/legacy/api/trees/**"
                        ).permitAll()

                        .requestMatchers(HttpMethod.POST,
                                "/api/user/heartbeat",
                                "/legacy/api/user/heartbeat"
                        ).authenticated()

                        // PUBLIC SHARED TREE ENDPOINTS – PHẢI ĐẶT TRƯỚC CÁC RULE /api/trees/*
                        .requestMatchers("/api/trees/shared/**", "/legacy/api/trees/shared/**").permitAll()

                        // Notifications stream
                        .requestMatchers("/api/notifications/stream", "/legacy/api/notifications/stream").permitAll()

                        // ==================== PROTECTED ENDPOINTS ====================

                        // Tree CRUD (private trees) – cần auth
                        .requestMatchers(HttpMethod.GET, "/api/trees", "/legacy/api/trees").authenticated()
                        .requestMatchers(HttpMethod.POST, "/api/trees", "/legacy/api/trees").authenticated()
                        .requestMatchers(HttpMethod.PUT, "/api/trees/*", "/legacy/api/trees/*").authenticated()
                        .requestMatchers(HttpMethod.DELETE, "/api/trees/*", "/legacy/api/trees/*").authenticated()

                        // Save tree vào dashboard (cần auth)
                        .requestMatchers(HttpMethod.POST, "/api/trees/*/save", "/legacy/api/trees/*/save").authenticated()

                        // Members & Relationships (private trees)
                        .requestMatchers(HttpMethod.GET, "/api/trees/*/members", "/legacy/api/trees/*/members").authenticated()
                        .requestMatchers(HttpMethod.POST, "/api/trees/*/members", "/legacy/api/trees/*/members").authenticated()
                        .requestMatchers(HttpMethod.PUT, "/api/trees/*/members/*", "/legacy/api/trees/*/members/*").authenticated()
                        .requestMatchers(HttpMethod.DELETE, "/api/trees/*/members/**", "/legacy/api/trees/*/members/**").authenticated()

                        .requestMatchers(HttpMethod.GET, "/api/trees/*/relationships/**", "/legacy/api/trees/*/relationships/**").authenticated()
                        .requestMatchers(HttpMethod.POST, "/api/trees/*/relationships", "/legacy/api/trees/*/relationships").authenticated()
                        .requestMatchers(HttpMethod.DELETE, "/api/trees/*/relationships/*", "/legacy/api/trees/*/relationships/*").authenticated()

                        // Share management (owner only)
                        .requestMatchers(HttpMethod.POST, "/api/trees/*/share/public", "/legacy/api/trees/*/share/public").authenticated()
                        .requestMatchers(HttpMethod.DELETE, "/api/trees/*/share/public", "/legacy/api/trees/*/share/public").authenticated()
                        .requestMatchers(HttpMethod.POST, "/api/trees/*/share/user", "/legacy/api/trees/*/share/user").authenticated()
                        .requestMatchers(HttpMethod.GET, "/api/trees/*/share/users", "/legacy/api/trees/*/share/users").authenticated()
                        .requestMatchers(HttpMethod.DELETE, "/api/trees/*/share/users/*", "/legacy/api/trees/*/share/users/*").authenticated()

                        // Authenticated actions on shared trees (edit via link)
                        .requestMatchers(HttpMethod.POST, "/api/trees/shared/*/members", "/legacy/api/trees/shared/*/members").authenticated()
                        .requestMatchers(HttpMethod.PUT, "/api/trees/shared/*/members/*", "/legacy/api/trees/shared/*/members/*").authenticated()

                        // Events & Notifications
                        .requestMatchers("/api/events/**", "/legacy/api/events/**").authenticated()
                        .requestMatchers("/api/notifications/**", "/legacy/api/notifications/**").authenticated()

                        // Debug & Docs
                        .requestMatchers("/v3/api-docs/**", "/swagger-ui/**", "/actuator/**").permitAll()
                        .requestMatchers("/api/debug/**", "/legacy/api/debug/**").permitAll()

                        // Admin
                        .requestMatchers("/api/admin/**", "/legacy/api/admin/**").hasRole("ADMIN")
                        .requestMatchers("/api/support/**").permitAll()
                        .anyRequest().authenticated()
                );

        log.info("API Security Chain configured successfully – shared tree endpoints are public");
        return http.build();
    }

    @Bean
    @Order(2)
    SecurityFilterChain webChain(HttpSecurity http,
                                 CustomOAuth2UserService oAuth2UserService,
                                 CustomOidcUserService oidcUserService,
                                 OAuth2SuccessHandler successHandler) throws Exception {
        log.info("Configuring Web Security Chain");

        http
                .securityMatcher("/**")
                .csrf(csrf -> csrf.disable())
                .cors(cors -> cors.configurationSource(corsConfigurationSource()))
                .sessionManagement(sm -> sm.sessionCreationPolicy(SessionCreationPolicy.IF_REQUIRED))
                .authorizeHttpRequests(auth -> auth
                        .requestMatchers(
                                "/", "/index.html",
                                "/oauth2/**", "/login/**", "/login/oauth2/**",
                                "/error", "/default-ui.css", "/assets/**", "/favicon.ico",
                                "/ws/**"
                        ).permitAll()
                        .anyRequest().authenticated()
                )
                .oauth2Login(oauth -> oauth
                        .loginPage("/")
                        .userInfoEndpoint(u -> u
                                .userService(oAuth2UserService)
                                .oidcUserService(oidcUserService)
                        )
                        .successHandler(successHandler)
                        .failureHandler((req, res, ex) -> {
                            log.error("OAuth2 login failed: {}", ex.getMessage());

                            String errorParam = "auth_failed";
                            String msg = ex.getMessage() != null ? ex.getMessage().toLowerCase() : "";
                            String causeMsg = ex.getCause() != null && ex.getCause().getMessage() != null
                                    ? ex.getCause().getMessage().toLowerCase() : "";

                            if (msg.contains("banned") || causeMsg.contains("banned")) {
                                errorParam = "banned";
                            } else if (msg.contains("disabled") || causeMsg.contains("disabled") ||
                                    msg.contains("inactive") || causeMsg.contains("inactive")) {
                                errorParam = "disabled";
                            }

                            String redirectUrl = frontendUrl + "/?error=" + URLEncoder.encode(errorParam, StandardCharsets.UTF_8);
                            log.info("OAuth2 failure → redirect to: {}", redirectUrl);
                            res.sendRedirect(redirectUrl);
                        })
                );

        log.info("Web Security Chain configured");
        return http.build();
    }
}