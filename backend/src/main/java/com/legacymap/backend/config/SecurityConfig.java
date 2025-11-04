package com.legacymap.backend.config;

import com.legacymap.backend.repository.UserRepository;
import com.legacymap.backend.service.JwtUtil;
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

@Slf4j
@Configuration
public class SecurityConfig {

    private final JwtUtil jwtUtil;
    private final UserRepository userRepository;
    @Value("${app.frontend.url}")
    private String frontendUrl;

    public SecurityConfig(JwtUtil jwtUtil, UserRepository userRepository) {
        this.jwtUtil = jwtUtil;
        this.userRepository = userRepository;

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

        JwtAuthenticationFilter jwtFilter = new JwtAuthenticationFilter(jwtUtil, userRepository);
        log.info("JwtAuthenticationFilter created");

        http
                .securityMatcher("/api/**", "/legacy/api/**")
                .csrf(csrf -> csrf.disable())
                .cors(cors -> cors.configurationSource(corsConfigurationSource()))
                .sessionManagement(sm -> sm.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
                .addFilterBefore(jwtFilter, UsernamePasswordAuthenticationFilter.class)

                .authorizeHttpRequests(auth -> auth
                        // Permit các HEAD & OPTIONS request cho tất cả endpoint
                        .requestMatchers(HttpMethod.OPTIONS, "/**").permitAll()
                        .requestMatchers(HttpMethod.HEAD, "/**").permitAll()

                        .requestMatchers(HttpMethod.POST, "/api/users/register").permitAll()
                        .requestMatchers(HttpMethod.POST, "/api/auth/login").permitAll()
                        .requestMatchers(HttpMethod.GET, "/api/auth/verify/**").permitAll()
                        .requestMatchers(HttpMethod.POST, "/api/auth/forgot-password").permitAll()
                        .requestMatchers(HttpMethod.POST, "/api/auth/reset-password").permitAll()
                        .requestMatchers("/api/trees/**").permitAll()
                        .requestMatchers("/api/events/**").authenticated()
                        .requestMatchers("/v3/api-docs/**", "/swagger-ui/**", "/actuator/**").permitAll()
                        .requestMatchers("/api/debug/**").permitAll()

                        .requestMatchers(HttpMethod.GET, "/api/admin/users").hasRole("ADMIN")
                        .requestMatchers(HttpMethod.GET, "/api/admin/users/*").hasRole("ADMIN")
                        .requestMatchers(HttpMethod.POST, "/api/admin/users/*/ban").hasRole("ADMIN")
                        .requestMatchers(HttpMethod.POST, "/api/admin/users/*/unban").hasRole("ADMIN")
                        .requestMatchers("/api/admin/**").hasRole("ADMIN")

                        .anyRequest().authenticated()
                );

        log.info("API Security Chain configured");
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
                                "/error", "/default-ui.css", "/assets/**", "/favicon.ico"
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
                        // FIXED: Handle OAuth2 authentication errors
                        .failureHandler((req, res, ex) -> {
                            log.error("OAuth2 login failed: {}", ex.getMessage());

                            String errorParam = "auth_failed";

                            // FIXED: Check cả direct exception message và cause
                            String exceptionMessage = ex.getMessage();

                            // Check direct exception message first
                            if (exceptionMessage != null && exceptionMessage.toLowerCase().contains("banned")) {
                                errorParam = "banned";
                                log.warn("Detected banned account (from direct message)");
                            } else if (exceptionMessage != null && exceptionMessage.toLowerCase().contains("disabled")) {
                                errorParam = "disabled";
                                log.warn("⚠Detected disabled account (from direct message)");
                            }
                            // Then check cause if not found
                            else if (ex.getCause() != null) {
                                String causeMsg = ex.getCause().getMessage();
                                log.error("Cause: {}", causeMsg);

                                if (causeMsg != null) {
                                    String lowerMsg = causeMsg.toLowerCase();

                                    if (lowerMsg.contains("banned")) {
                                        errorParam = "banned";
                                        log.warn("Detected banned account (from cause)");
                                    } else if (lowerMsg.contains("disabled")) {
                                        errorParam = "disabled";
                                        log.warn("Detected disabled account (from cause)");
                                    }
                                }
                            }

                            // FIXED: Redirect về homepage thay vì /signin
                            // Frontend sẽ tự hiển thị modal SignIn với error message
                            String redirectUrl =    frontendUrl + "/?error="
                                    + URLEncoder.encode(errorParam, StandardCharsets.UTF_8);
                            log.info("Redirecting to: {}", redirectUrl);

                            res.sendRedirect(redirectUrl);
                        })
                );

        log.info("Web Security Chain configured");
        return http.build();
    }
}