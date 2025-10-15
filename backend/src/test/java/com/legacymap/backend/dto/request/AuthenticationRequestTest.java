package com.legacymap.backend.dto.request;

import com.legacymap.backend.dto.request.AuthenticationRequest;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

//kiểm tra builder và getter/setter của DTO hoạt động đúng.
@Tag("unit")
public class AuthenticationRequestTest {

    @Test
    void builder_setsFieldsCorrectly() {
        // Sử dụng placeholder thay cho dữ liệu nhạy cảm
        AuthenticationRequest req = AuthenticationRequest.builder()
                .identifier("<identifier>")
                .password("<password>")
                .build();

        assertEquals("<identifier>", req.getIdentifier());
        assertEquals("<password>", req.getPassword());
    }

    @Test
    void setters_updateFieldsCorrectly() {
        AuthenticationRequest req = new AuthenticationRequest();
        req.setIdentifier("<identifier>");
        req.setPassword("<password>");

        assertEquals("<identifier>", req.getIdentifier());
        assertEquals("<password>", req.getPassword());
    }
}