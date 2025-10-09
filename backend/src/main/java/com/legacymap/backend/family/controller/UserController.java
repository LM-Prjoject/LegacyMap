package com.legacymap.backend.family.controller;

import com.legacymap.backend.family.dto.request.UserCreateRequest;
import com.legacymap.backend.family.entity.User;
import com.legacymap.backend.family.service.UserService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/users")
public class UserController {
    @Autowired
    private UserService userService;

    @PostMapping("/register")
    User createUser(@RequestBody @Valid UserCreateRequest request) {
        return userService.createRequest(request);
        }

}
