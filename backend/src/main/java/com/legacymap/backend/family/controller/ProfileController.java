package com.legacymap.backend.family.controller;

import com.legacymap.backend.family.dto.request.ProfileCreateRequest;
import com.legacymap.backend.family.entity.Profile;
import com.legacymap.backend.family.service.ProfileService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/profiles")
public class ProfileController {
    @Autowired
    private ProfileService profileService;

    @PostMapping
    Profile createProfile(@RequestBody ProfileCreateRequest request) {
        return profileService.createAccount(request);

    }
}
