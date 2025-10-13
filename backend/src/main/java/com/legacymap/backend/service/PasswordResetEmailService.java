package com.legacymap.backend.service;

import jakarta.mail.MessagingException;
import jakarta.mail.internet.MimeMessage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;

@Service
public class PasswordResetEmailService {

    @Autowired
    private JavaMailSender mailSender;

    public void sendResetEmail(String toEmail, String userName, String resetUrl) throws MessagingException {
        String htmlContent = buildResetEmail(userName, resetUrl);

        MimeMessage mimeMessage = mailSender.createMimeMessage();
        MimeMessageHelper helper = new MimeMessageHelper(mimeMessage, true, "UTF-8");

        helper.setTo(toEmail);
        helper.setSubject("Reset your password");
        helper.setFrom("legacymap180@gmail.com");
        helper.setText(htmlContent, true);

        mailSender.send(mimeMessage);
    }

    private String buildResetEmail(String userName, String resetUrl) {
        String template = """
        <div style=\"font-family: Arial, sans-serif; line-height: 1.5;\">
            <h2 style=\"color: #2e6da4;\">Chào %s,</h2>
            <p>Bạn đã yêu cầu đặt lại mật khẩu.</p>
            <p>Vui lòng nhấn vào nút bên dưới để chuyển đến trang đổi mật khẩu:</p>
            <a href='%s'
               style='display: inline-block; padding: 10px 20px; background-color: #007bff; color: white;
                      text-decoration: none; border-radius: 5px; margin-top: 10px;'>
               Đặt lại mật khẩu
            </a>
            <p style='margin-top: 20px;'>Nếu bạn không yêu cầu điều này, hãy bỏ qua email này.</p>
            <p>Trân trọng,<br>LegacyMap</p>
        </div>
        """;
        return String.format(template, userName, resetUrl);
    }
}
