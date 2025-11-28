package com.legacymap.backend.entity;

import lombok.*;

@Data
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class LunarDate  {
    private int day;
    private int month;
    private int year;
    private boolean leapMonth;
}
