package com.legacymap.backend.controller;

import com.legacymap.backend.dto.request.UserCreateRequest;
import com.legacymap.backend.dto.response.ApiResponse;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.entity.UserProfile;
import com.legacymap.backend.service.UserService;
import jakarta.validation.Valid;
import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/users")
public class UserController {

    @Autowired
    private UserService userService;

    @PostMapping("/register")
    public ResponseEntity<ApiResponse<User>> createUser(@RequestBody @Valid UserCreateRequest request) {
        User user = userService.createRequest(request);
        return ResponseEntity.ok(ApiResponse.success(user));
    }

    @GetMapping("/{id}")
    public ResponseEntity<ApiResponse<User>> getUser(@PathVariable UUID id) {
        User user = userService.getUserById(id);
        return ResponseEntity.ok(ApiResponse.success(user));
    }

    @PutMapping("/{id}")
    public ResponseEntity<ApiResponse<UserProfile>> updateUser(@PathVariable UUID id, @RequestBody UserProfile profile) {
        UserProfile updated = userService.updateUserProfile(id, profile);
        return ResponseEntity.ok(ApiResponse.success(updated));
    }
}
