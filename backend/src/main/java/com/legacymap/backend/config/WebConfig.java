package com.legacymap.backend.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.web.servlet.config.annotation.AsyncSupportConfigurer;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
public class WebConfig implements WebMvcConfigurer {

    @Override
    public void addCorsMappings(CorsRegistry registry) {
        registry.addMapping("/**")
                .allowedOrigins(
                        "https://legacy-map-ebon.vercel.app",
                        "http://localhost:5175",
                        "http://localhost:5174",
                        "http://localhost:5173",
                        "http://localhost:3000"
                )
                .allowedMethods("GET", "POST", "PUT", "DELETE", "OPTIONS", "PATCH")
                .allowedHeaders("*")
                .allowCredentials(true)
                .maxAge(3600);
    }
    @Override
    public void configureAsyncSupport(AsyncSupportConfigurer configurer) {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(10);           // số thread tối thiểu
        executor.setMaxPoolSize(100);           // tối đa khi tải cao
        executor.setQueueCapacity(500);         // hàng đợi
        executor.setThreadNamePrefix("SSE-");   // dễ debug trong log
        executor.setWaitForTasksToCompleteOnShutdown(true);
        executor.setAwaitTerminationSeconds(60);
        executor.initialize();

        configurer.setTaskExecutor(executor);
        configurer.setDefaultTimeout(300_000); // 5 phút timeout cho SSE (rất quan trọng!)
    }

    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        registry.addResourceHandler("/uploads/**")
                .addResourceLocations("file:uploads/");
    }

    @Override
    public void configureMessageConverters(java.util.List<org.springframework.http.converter.HttpMessageConverter<?>> converters) {
        converters.stream()
                .filter(c -> c instanceof org.springframework.http.converter.StringHttpMessageConverter)
                .findFirst()
                .ifPresent(c -> ((org.springframework.http.converter.StringHttpMessageConverter) c)
                        .setDefaultCharset(java.nio.charset.StandardCharsets.UTF_8));
    }
}