package com.legacymap.backend;

import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;
//Test tất cả cùng lúc
@Suite
@SelectClasses({
        com.legacymap.backend.api.auth.AuthenticationControllerTest.class,
        com.legacymap.backend.dto.request.AuthenticationRequestTest.class,
        com.legacymap.backend.api.auth.AuthVerifyControllerTest.class,
})
public class AuthTestsSuite {}
