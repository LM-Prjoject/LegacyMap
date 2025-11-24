package com.legacymap.backend.entity;

import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;

@Converter(autoApply = false)
public class ChatRoomTypeConverter implements AttributeConverter<ChatRoom.ChatRoomType, String> {

    @Override
    public String convertToDatabaseColumn(ChatRoom.ChatRoomType attribute) {
        if (attribute == null) {
            return null;
        }
        return switch (attribute) {
            case family -> "family";
            case branch -> "branch";
            case private_chat -> "private";
            case group -> "group";
        };
    }

    @Override
    public ChatRoom.ChatRoomType convertToEntityAttribute(String dbData) {
        if (dbData == null) {
            return null;
        }
        return switch (dbData.toLowerCase()) {
            case "family" -> ChatRoom.ChatRoomType.family;
            case "branch" -> ChatRoom.ChatRoomType.branch;
            case "private", "private_chat" -> ChatRoom.ChatRoomType.private_chat;
            case "group" -> ChatRoom.ChatRoomType.group;
            default -> throw new IllegalArgumentException("Unknown chat_room type: " + dbData);
        };
    }
}

