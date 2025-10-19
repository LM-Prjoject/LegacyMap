package com.legacymap.backend.config;

import com.legacymap.backend.service.google.CustomOAuth2UserService;
import com.legacymap.backend.service.google.CustomOidcUserService;
import com.legacymap.backend.service.google.OAuth2SuccessHandler;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

import java.util.Arrays;

    @Configuration
    public class SecurityConfig {

        @Bean
        public PasswordEncoder passwordEncoder() {
            return new BCryptPasswordEncoder(10);
        }

        @Bean
        CorsConfigurationSource corsConfigurationSource() {
            CorsConfiguration c = new CorsConfiguration();
            c.setAllowedOriginPatterns(Arrays.asList(
                    "http://localhost:3000", "http://localhost:5173", "http://localhost:5174", "http://localhost:5175"
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
            http
                    .securityMatcher("/api/**")
                    .csrf(csrf -> csrf.disable())
                    .cors(cors -> cors.configurationSource(corsConfigurationSource()))
                    .sessionManagement(sm -> sm.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
                    .authorizeHttpRequests(auth -> auth
                            .requestMatchers(HttpMethod.POST, "/api/users/register").permitAll()
                            .requestMatchers(HttpMethod.POST, "/api/auth/login").permitAll()
                            .requestMatchers(HttpMethod.GET, "/api/auth/verify/**").permitAll()
                            .requestMatchers("/api/trees/**").permitAll()
                            .requestMatchers("/api/auth/**", "/v3/api-docs/**", "/swagger-ui/**", "/actuator/**").permitAll()
                            .anyRequest().authenticated()
                    );
            return http.build();
        }

        @Bean
        @Order(2)
        SecurityFilterChain webChain(HttpSecurity http,
                                     CustomOAuth2UserService oAuth2UserService,
                                     CustomOidcUserService oidcUserService,
                                     OAuth2SuccessHandler successHandler) throws Exception {
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
                            .loginPage("/login")
                            .userInfoEndpoint(u -> u
                                    .userService(oAuth2UserService)
                                    .oidcUserService(oidcUserService)
                            )
                            .successHandler(successHandler)
                            .failureHandler((req, res, ex) -> {
                                ex.printStackTrace();
                                res.sendRedirect("/login?error");
                            })
                    );

            return http.build();
        }
    }