package com.legacymap.backend.family.dto.request;

import lombok.Data;

import java.time.LocalDate;

@Data
public class ProfileCreateRequest {

    private String username;
    private String password;
    private String fullName;
    private String email;
    private String clanName;
    private String gender;
    private String phone;
    private LocalDate dob;
    private String city;
    private String ward;
    private String houseNumber;
}
