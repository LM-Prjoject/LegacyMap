package com.legacymap.backend.service;

import com.legacymap.backend.entity.LunarDate;
import com.nlf.calendar.Lunar;
import com.nlf.calendar.Solar;
import org.springframework.stereotype.Service;

import java.time.LocalDate;

@Service
public class LunarCalendarService {

    public LunarDate solarToLunar(LocalDate solarDate) {
        try {
            Solar solar = Solar.fromYmd(
                    solarDate.getYear(),
                    solarDate.getMonthValue(),
                    solarDate.getDayOfMonth()
            );

            Lunar lunar = solar.getLunar();

            return new LunarDate(
                    lunar.getDay(),
                    lunar.getMonth(),
                    lunar.getYear(),
                    Math.abs(lunar.getMonth()) == Math.abs(lunar.getMonth())
            );

        } catch (Exception e) {
            throw new RuntimeException("Error converting solar date to lunar date: " + e.getMessage(), e);
        }
    }

    public LocalDate lunarToSolar(LunarDate lunarDate) {
        try {
            Lunar lunar = Lunar.fromYmd(
                    lunarDate.getYear(),
                    lunarDate.getMonth(),
                    lunarDate.getDay()
            );

            Solar solar = lunar.getSolar();

            return LocalDate.of(solar.getYear(), solar.getMonth(), solar.getDay());

        } catch (Exception e) {
            throw new RuntimeException("Error converting lunar date to solar date: " + e.getMessage(), e);
        }
    }
}