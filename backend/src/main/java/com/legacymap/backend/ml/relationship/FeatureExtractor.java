package com.legacymap.backend.ml.relationship;

import com.legacymap.backend.entity.Person;

import java.time.LocalDate;
import java.time.Period;

class FeatureExtractor {
    static int age(Person p) {
        LocalDate b = p.getBirthDate();
        if (b == null) return -1;
        LocalDate ref = LocalDate.now();
        if (p.getDeathDate() != null && p.getDeathDate().isBefore(ref)) {
            ref = p.getDeathDate();
        }
        return Period.between(b, ref).getYears();
    }

    static int ageDelta(Person a, Person b) {
        int aa = age(a);
        int bb = age(b);
        if (aa < 0 || bb < 0) return Integer.MIN_VALUE;
        return aa - bb;
    }

    static boolean genderIs(Person p, String g) {
        if (p.getGender() == null || g == null) return false;
        return p.getGender().trim().equalsIgnoreCase(g.trim());
    }

    static String surname(String fullName) {
        if (fullName == null) return "";
        String s = fullName.trim();
        if (s.isEmpty()) return s;
        String[] parts = s.split("\\s+");
        return parts[0].toLowerCase(); // họ là từ đầu tiên
    }

    static boolean sameSurname(Person a, Person b) {
        return surname(a.getFullName()).equals(surname(b.getFullName()));
    }
}
