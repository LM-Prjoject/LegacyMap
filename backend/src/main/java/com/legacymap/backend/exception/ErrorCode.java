package com.legacymap.backend.exception;

import lombok.Getter;

@Getter
public enum ErrorCode {
    USER_EXISTED("Username already exists", 1001),
    EMAIL_EXISTED("Email is already in use", 1002),
    SEND_EMAIL_FAILED("Failed to send email", 1003),
    USER_NOT_FOUND("User not found", 1004),
    INVALID_CREDENTIALS("Invalid username or password", 1005),
    ACCOUNT_NOT_VERIFIED("Account is not verified", 1006),
    ACCOUNT_DISABLED("Account has been disabled", 1007),
    VALIDATION_FAILED("Invalid data", 1008),
    INTERNAL_ERROR("Internal system error", 1009),
    INTERNAL_SERVER_ERROR("Internal server error", 5000),
    INVALID_TOKEN("Invalid verification token", 1010),
    TOKEN_EXPIRED("Verification token has expired", 1011),
    TOKEN_ALREADY_USED("Verification token has already been used", 1012),
    INVALID_PURPOSE("Invalid token purpose", 1013),
    INVALID_PASSWORD("Incorrect password", 1024),
    BAD_REQUEST("Bad request", 1025),
    NOT_FOUND("Resource not found", 1026),

    // Authentication & Authorization
    UNAUTHENTICATED("You are not logged in", 1014),
    UNAUTHORIZED("You do not have permission to access this resource", 1015),
    OAUTH_GOOGLE_ONLY("This account can only log in using Google", 1021),

    // Family Tree related
    FAMILY_TREE_NOT_FOUND("Family tree not found or you do not have access", 1100),
    PERSON_NOT_FOUND("Person not found in the family tree", 1101),
    RELATIONSHIP_INVALID_TYPE("Invalid relationship type", 1102),
    RELATIONSHIP_ALREADY_EXISTS("This relationship already exists", 1103),
    RELATIONSHIP_NOT_SAME_TREE("Both persons do not belong to the same family tree", 1104),
    RELATIONSHIP_NOT_FOUND("Relationship not found", 1105),
    RELATIONSHIP_SELF_LINK("Cannot create a relationship with yourself", 1106),
    TREE_NOT_FOUND("Family tree not found", 1107),

    // Admin related
    USER_BANNED("Your account has been banned. Please contact support", 1110),
    USER_ALREADY_BANNED("This user is already banned", 1111),
    USER_NOT_BANNED("This user is not banned", 1112),
    CANNOT_BAN_ADMIN("Cannot ban an admin account", 1113),
    ADMIN_ACTION_FORBIDDEN("Action is forbidden for admin accounts", 1114),

    // Event related
    EVENT_NOT_FOUND("Event not found", 1120),
    INVALID_INPUT_DATA("Invalid input data", 1121),

    // Notification related
    NOTIFICATION_NOT_FOUND("Notification not found", 1113),

    // Person email uniqueness within a family tree
    PERSON_EMAIL_EXISTS_IN_TREE("Email already exists in this family tree", 1140),

    // Resource related
    RESOURCE_ALREADY_EXISTS("Resource already exists", 1141),
    RESOURCE_NOT_FOUND("Resource not found", 1142),
    ACCESS_DENIED("Access denied", 1143),

    // Chat related
    MESSAGE_EDIT_FORBIDDEN("You do not have permission to edit this message", 1200),
    MESSAGE_DELETE_FORBIDDEN("You do not have permission to delete this message", 1201),

    // Sharing related
    PERMISSION_DENIED("You do not have permission to perform this action", 4030),
    USER_ALREADY_HAS_ACCESS("The user already has access to this family tree", 4031),
    CANNOT_SHARE_TO_SELF("You cannot share the family tree with yourself", 4032);

    private final int code;
    private final String message;

    ErrorCode(String message, int code) {
        this.message = message;
        this.code = code;
    }
}
