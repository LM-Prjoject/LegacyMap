package com.legacymap.backend.utils;

import com.nlf.calendar.Lunar;
import com.nlf.calendar.Solar;
import java.time.*;

public class LunarSolarConverter {
    /**
     * Chuyển ngày âm lịch sang dương lịch, xử lý timezone
     */
    public static OffsetDateTime toSolar(OffsetDateTime lunarDateTime) {
        if (lunarDateTime == null) return null;

        try {
            LocalDate lunarDate = lunarDateTime.toLocalDate();
            Lunar lunar = new Lunar(lunarDate.getYear(), lunarDate.getMonthValue(), lunarDate.getDayOfMonth());
            Solar solar = lunar.getSolar();

            LocalDate solarDate = LocalDate.of(solar.getYear(), solar.getMonth(), solar.getDay());
            LocalTime time = lunarDateTime.toLocalTime();

            return OffsetDateTime.of(solarDate, time, lunarDateTime.getOffset());
        } catch (Exception e) {
            System.err.println("Lỗi chuyển âm sang dương: " + e.getMessage());
            return lunarDateTime;
        }
    }

    /**
     * Chuyển ngày dương lịch sang âm lịch
     */
    public static LocalDateTime toLunar(LocalDateTime solarDateTime) {
        try {
            LocalDate solarDate = solarDateTime.toLocalDate();
            Solar solar = new Solar(solarDate.getYear(), solarDate.getMonthValue(), solarDate.getDayOfMonth());
            Lunar lunar = solar.getLunar();
            LocalDate lunarDate = LocalDate.of(lunar.getYear(), lunar.getMonth(), lunar.getDay());

            LocalTime time = solarDateTime.toLocalTime();
            return LocalDateTime.of(lunarDate, time);

        } catch (Exception e) {
            return solarDateTime;
        }
    }

    // Giữ nguyên các method cũ cho LocalDate
    public static LocalDate toSolar(LocalDate lunarDate) {
        try {
            Lunar lunar = new Lunar(lunarDate.getYear(), lunarDate.getMonthValue(), lunarDate.getDayOfMonth());
            Solar solar = lunar.getSolar();
            return LocalDate.of(solar.getYear(), solar.getMonth(), solar.getDay());
        } catch (Exception e) {
            return lunarDate;
        }
    }

    public static LocalDate toLunar(LocalDate solarDate) {
        try {
            Solar solar = new Solar(solarDate.getYear(), solarDate.getMonthValue(), solarDate.getDayOfMonth());
            Lunar lunar = solar.getLunar();
            return LocalDate.of(lunar.getYear(), lunar.getMonth(), lunar.getDay());
        } catch (Exception e) {
            return solarDate;
        }
    }
}
