package com.legacymap.backend.family.controller;

import com.legacymap.backend.family.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/profiles")
public class UserController {
    @Autowired
    private UserService profileService;

}
