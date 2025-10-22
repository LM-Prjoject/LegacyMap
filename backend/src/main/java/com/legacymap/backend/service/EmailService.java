package com.legacymap.backend.service;

import jakarta.mail.MessagingException;
import jakarta.mail.internet.MimeMessage;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class EmailService {

    private final JavaMailSender mailSender;

    @Value("${app.backend.url:http://localhost:8080}")
    private String backendUrl;

    public EmailService(JavaMailSender mailSender) {
        this.mailSender = mailSender;
    }

    public void sendVerificationEmail(String toEmail, String userName, String token) throws MessagingException {
        // ✅ Tự động dùng URL theo môi trường (Render hoặc Local)
        String verifyUrl = backendUrl + "/legacy/api/auth/verify?token=" + token;

        log.info("📧 Sending verification email to {} with verify URL: {}", toEmail, verifyUrl);

        String htmlContent = buildVerificationEmail(userName, verifyUrl);

        MimeMessage mimeMessage = mailSender.createMimeMessage();
        MimeMessageHelper helper = new MimeMessageHelper(mimeMessage, true, "UTF-8");

        helper.setTo(toEmail);
        helper.setSubject("Please verify your account!");
        helper.setFrom("legacymap180@gmail.com");
        helper.setText(htmlContent, true);

        mailSender.send(mimeMessage);
    }

    private String buildVerificationEmail(String userName, String verifyUrl) {
        String template = """
            <div style="font-family: Arial, sans-serif; line-height: 1.5;">
                <h2 style="color: #2e6da4;">Chào %s,</h2>
                <p>Bạn đã đăng ký tài khoản thành công tại hệ thống của chúng tôi.</p>
                <p>Vui lòng nhấn vào nút bên dưới để xác minh email và kích hoạt tài khoản:</p>
                <a href='%s'
                   style='display: inline-block; padding: 10px 20px; background-color: #28a745; color: white;
                          text-decoration: none; border-radius: 5px; margin-top: 10px;'>
                   Xác minh tài khoản
                </a>
                <p style='margin-top: 20px;'>Nếu bạn không yêu cầu điều này, hãy bỏ qua email này.</p>
                <p>Trân trọng,<br>Đội ngũ hỗ trợ LegacyMap</p>
            </div>
        """;
        return String.format(template, userName, verifyUrl);
    }
}
