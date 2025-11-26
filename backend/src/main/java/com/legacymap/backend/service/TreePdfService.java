package com.legacymap.backend.service;

import com.legacymap.backend.dto.export.MemberExportRow;
import com.legacymap.backend.dto.export.TreeExportDTO;
import com.legacymap.backend.dto.export.TreeExportSummary;
import com.lowagie.text.*;
import com.lowagie.text.pdf.*;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

@Service
@RequiredArgsConstructor
public class TreePdfService {

    private static final float CM = 28.3465f;

    private static final float MARGIN_LEFT   = 3.5f * CM;
    private static final float MARGIN_RIGHT  = 2.0f * CM;
    private static final float MARGIN_TOP    = 2.5f * CM;
    private static final float MARGIN_BOTTOM = 2.5f * CM;

    private static final String FONT_PATH = "fonts/DejaVuSans.ttf";

    private BaseFont loadVietnameseFont() throws IOException, DocumentException {
        ClassPathResource fontRes = new ClassPathResource(FONT_PATH);
        try (InputStream is = fontRes.getInputStream()) {
            byte[] fontBytes = is.readAllBytes();
            return BaseFont.createFont(
                    "DejaVuSans.ttf",
                    BaseFont.IDENTITY_H,
                    BaseFont.EMBEDDED,
                    true,
                    fontBytes,
                    null
            );
        }
    }

    public byte[] generateDetailsPdf(TreeExportDTO dto, byte[] treeImageBytes) {
        try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {

            Document document = new Document(
                    PageSize.A4,
                    MARGIN_LEFT,
                    MARGIN_RIGHT,
                    MARGIN_TOP,
                    MARGIN_BOTTOM
            );
            PdfWriter.getInstance(document, baos);
            document.open();

            BaseFont bf = loadVietnameseFont();
            Font titleFont = new Font(bf, 18, Font.BOLD);
            Font headingFont = new Font(bf, 14, Font.BOLD);
            Font normalFont = new Font(bf, 11, Font.NORMAL);
            Font boldFont = new Font(bf, 11, Font.BOLD);

            Paragraph title = new Paragraph("BÁO CÁO CÂY GIA PHẢ", titleFont);
            title.setAlignment(Element.ALIGN_CENTER);
            document.add(title);
            document.add(new Paragraph(" "));

            Paragraph section1 = new Paragraph("I. TỔNG QUAN", headingFont);
            section1.setSpacingBefore(10);
            section1.setSpacingAfter(10);
            document.add(section1);

            addSummarySection(document, dto.summary(), boldFont, normalFont);
            document.add(new Paragraph(" "));

            Paragraph section2 = new Paragraph("II. PHẢ ĐỒ", headingFont);
            section2.setSpacingBefore(20);
            section2.setSpacingAfter(10);
            document.add(section2);

            if (treeImageBytes != null && treeImageBytes.length > 0) {
                Image treeImg = Image.getInstance(treeImageBytes);

                float maxWidth = document.getPageSize().getWidth()
                        - document.leftMargin() - document.rightMargin();
                float maxHeight = (document.getPageSize().getHeight()
                        - document.topMargin() - document.bottomMargin()) * 0.6f;

                treeImg.scaleToFit(maxWidth, maxHeight);
                treeImg.setAlignment(Image.ALIGN_CENTER);
                document.add(treeImg);
            }

            document.add(new Paragraph(" "));

            Paragraph section3 = new Paragraph("III. DANH SÁCH THÀNH VIÊN", headingFont);
            section3.setSpacingBefore(20);
            section3.setSpacingAfter(10);
            document.add(section3);

            addMembersTable(document, dto.members(), boldFont, normalFont);
            document.close();
            return baos.toByteArray();
        } catch (Exception e) {
            throw new RuntimeException("Could not generate tree PDF", e);
        }
    }

    private void addSummarySection(
            Document document,
            TreeExportSummary s,
            Font bold,
            Font normal
    ) throws DocumentException {
        PdfPTable table = new PdfPTable(2);
        table.setWidthPercentage(100);
        table.setWidths(new float[]{25, 75});

        addLabelValueRow(table, "Tên cây", s.name(), bold, normal);
        addLabelValueRow(table, "Mô tả", s.description(), bold, normal);
        addLabelValueRow(table, "Chủ sở hữu", s.ownerName(), bold, normal);
        addLabelValueRow(table, "Email", s.ownerEmail(), bold, normal);
        addLabelValueRow(
                table,
                "Ngày tạo",
                s.createdAt() != null ? s.createdAt().toString() : "",
                bold,
                normal
        );
        addLabelValueRow(
                table,
                "Số thành viên",
                String.valueOf(s.memberCount()),
                bold,
                normal
        );
        addLabelValueRow(
                table,
                "Số thế hệ",
                String.valueOf(s.generationCount()),
                bold,
                normal
        );

        String genderStats = "Nam: " + s.maleCount()
                + " | Nữ: " + s.femaleCount()
                + " | Khác: " + s.otherCount();
        addLabelValueRow(table, "Giới tính", genderStats, bold, normal);

        String lifeStats = "Còn sống: " + s.aliveCount()
                + " | Đã mất: " + s.deceasedCount();
        addLabelValueRow(table, "Tình trạng", lifeStats, bold, normal);

        document.add(table);
    }

    private void addLabelValueRow(
            PdfPTable table,
            String label,
            String value,
            Font bold,
            Font normal
    ) {
        PdfPCell c1 = new PdfPCell(new Phrase(label, bold));
        c1.setBorder(Rectangle.NO_BORDER);
        c1.setPadding(3);
        table.addCell(c1);

        PdfPCell c2 = new PdfPCell(new Phrase(value != null ? value : "", normal));
        c2.setBorder(Rectangle.NO_BORDER);
        c2.setPadding(3);
        table.addCell(c2);
    }

    private void addMembersTable(
            Document document,
            List<MemberExportRow> rows,
            Font bold,
            Font normal
    ) throws DocumentException {

        PdfPTable table = new PdfPTable(5);
        table.setWidthPercentage(100);
        table.setWidths(new float[]{7, 30, 17, 25, 15});

        addHeaderCell(table, "STT", bold);
        addHeaderCell(table, "Họ tên", bold);
        addHeaderCell(table, "Giới tính", bold);
        addHeaderCell(table, "Năm sinh – mất", bold);
        addHeaderCell(table, "Vai trò", bold);

        for (MemberExportRow r : rows) {
            addBodyCell(table, String.valueOf(r.index()), normal, Element.ALIGN_CENTER);
            addBodyCell(table, r.fullName(), normal, Element.ALIGN_LEFT);
            addBodyCell(table, r.gender(), normal, Element.ALIGN_CENTER);
            addBodyCell(table, combineYear(r.birthYear(), r.deathYear()), normal, Element.ALIGN_CENTER);
            addBodyCell(table, r.role(), normal, Element.ALIGN_CENTER);
        }

        document.add(table);
    }

    private void addHeaderCell(PdfPTable table, String text, Font font) {
        PdfPCell cell = new PdfPCell(new Phrase(text, font));
        cell.setHorizontalAlignment(Element.ALIGN_CENTER);
        cell.setPadding(4);
        table.addCell(cell);
    }

    private void addBodyCell(PdfPTable table, String text, Font font, int horizontalAlign) {
        PdfPCell cell = new PdfPCell(new Phrase(text != null ? text : "", font));
        cell.setHorizontalAlignment(horizontalAlign);
        cell.setVerticalAlignment(Element.ALIGN_MIDDLE);
        cell.setPadding(4);
        table.addCell(cell);
    }

    private String combineYear(String birthYear, String deathYear) {
        if (birthYear == null && deathYear == null) return "";
        if (birthYear != null && deathYear == null) return birthYear + " –";
        if (birthYear == null) return "– " + deathYear;
        return birthYear + " – " + deathYear;
    }
}
