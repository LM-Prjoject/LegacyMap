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

        String verifyUrl = backendUrl + "/legacy/api/auth/verify?token=" + token;
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
        } catch (MessagingException e) {
            throw new RuntimeException("Failed to send email", e);
        }
    }

    public void sendTreeShareNotification(String toEmail, String recipientName, String treeName, String ownerName, String accessLevel, String shareUrl) throws MessagingException {
        String htmlContent = buildTreeShareEmail(recipientName, treeName, ownerName, accessLevel, shareUrl);

        MimeMessage mimeMessage = mailSender.createMimeMessage();
        MimeMessageHelper helper = new MimeMessageHelper(mimeMessage, true, "UTF-8");

        helper.setTo(toEmail);
        helper.setSubject("Bạn được mời xem cây gia phả: " + treeName);
        helper.setFrom("legacymap180@gmail.com");
        helper.setText(htmlContent, true);

        mailSender.send(mimeMessage);
    }

    private String buildTreeShareEmail(String recipientName, String treeName, String ownerName, String accessLevel, String shareUrl) {
        String accessText = "edit".equals(accessLevel) ? "chỉnh sửa" : "xem";

        String template = """
           <div style="font-family: Arial, sans-serif; line-height: 1.6; max-width: 600px; margin: 0 auto;">
               <h2 style="color: #ffd89b;">🌳 Thông báo chia sẻ cây gia phả</h2>
               <p>Chào <strong>%s</strong>,</p>
               <p><strong>%s</strong> đã chia sẻ cây gia phả <strong>"%s"</strong> với bạn.</p>
               <p>Quyền truy cập: <span style="color: #28a745; font-weight: bold;">%s</span></p>
               <a href='%s'
                  style='display: inline-block; padding: 12px 24px; background: linear-gradient(to right, #d4af7a, #ffd89b);
                         color: #0f1419; text-decoration: none; border-radius: 8px; margin: 20px 0; font-weight: bold;'>
                  Xem cây gia phả
               </a>
               <p style='margin-top: 20px; color: #666;'>Nếu bạn không yêu cầu điều này, hãy bỏ qua email này.</p>
               <hr style="border: none; border-top: 1px solid #ddd; margin: 20px 0;">
               <p style="color: #999; font-size: 12px;">Trân trọng,<br>Đội ngũ LegacyMap</p>
           </div>
       """;
        return String.format(template, recipientName, ownerName, treeName, accessText, shareUrl);
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