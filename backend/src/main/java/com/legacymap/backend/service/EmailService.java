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

    @Value("${app.frontend.url:http://localhost:3000}")
    private String frontendUrl;

    public EmailService(JavaMailSender mailSender) {
        this.mailSender = mailSender;
    }

    public void sendVerificationEmail(String toEmail, String userName, String token) throws MessagingException {
        // Tự động dùng URL theo môi trường (Render hoặc Local)
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

    public void sendEmail(String toEmail, String subject, String message) {
        try {
            MimeMessage mimeMessage = mailSender.createMimeMessage();
            MimeMessageHelper helper = new MimeMessageHelper(mimeMessage, true, "UTF-8");

            helper.setTo(toEmail);
            helper.setSubject(subject);
            helper.setFrom("legacymap180@gmail.com");
            helper.setText(message, false);

            mailSender.send(mimeMessage);
            log.info("Sent email to {}: {}", toEmail, subject);
        } catch (MessagingException e) {
            log.error("Failed to send email to {}", toEmail, e);
            throw new RuntimeException("Failed to send email", e);
        }
    }

    public void sendPersonInviteEmail(String toEmail, String inviterName, String personName) throws MessagingException {
        String ctaUrl = frontendUrl + "/login?redirect=/me/claims";
        String subject = "Lời mời xác nhận liên kết hồ sơ";
        String html = buildPersonInviteEmail(inviterName, personName, ctaUrl);

        MimeMessage mimeMessage = mailSender.createMimeMessage();
        MimeMessageHelper helper = new MimeMessageHelper(mimeMessage, true, "UTF-8");
        helper.setTo(toEmail);
        helper.setSubject(subject);
        helper.setFrom("legacymap180@gmail.com");
        helper.setText(html, true);

        mailSender.send(mimeMessage);
        log.info("Sent person invite email to {} for person {}", toEmail, personName);
    }

    private String buildPersonInviteEmail(String inviterName, String personName, String ctaUrl) {
        String template = """
            <div style='font-family: Arial, sans-serif; line-height: 1.6; color:#222;'>
                <h2 style='color:#2e6da4; margin-bottom:8px;'>Thư mời xác nhận liên kết hồ sơ</h2>
                <p>Xin chào,</p>
                <p>Người dùng <strong>%s</strong> đã mời bạn xác nhận liên kết với hồ sơ <strong>%s</strong> trong hệ thống LegacyMap.</p>
                <p>Nếu đây là bạn, vui lòng đăng nhập (hoặc đăng ký nếu chưa có tài khoản) và truy cập mục "Lời mời" để xác nhận.</p>
                <a href='%s' style='display:inline-block; padding:10px 16px; background:#28a745; color:#fff; text-decoration:none; border-radius:6px; margin-top:12px;'>Đăng nhập để xác nhận</a>
                <p style='margin-top:16px;'>Nếu bạn không mong đợi thư này, vui lòng bỏ qua. Chúng tôi luôn sẵn sàng hỗ trợ khi cần.</p>
                <p>Trân trọng,<br/>Đội ngũ LegacyMap</p>
            </div>
        """;
        return String.format(template, inviterName, personName, ctaUrl);
    }
}
