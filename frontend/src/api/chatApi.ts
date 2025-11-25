import { http } from '@/api/http';
import type {
  AttachmentUploadResponse,
  BranchRoomCreatePayload,
  ChatMessagePage,
  ChatRoom,
  ChatRoomCreatePayload,
  DirectRoomCreatePayload,
  MarkMessagesReadPayload,
  UserSearchResult,
  ChatMessage,
} from '@/types/chat';

const BASE = '/chat';

export const chatApi = {
  async getMyRooms(): Promise<ChatRoom[]> {
    const { data } = await http.get<ChatRoom[]>(`${BASE}/rooms`);
    return data;
  },

  async createRoom(payload: ChatRoomCreatePayload): Promise<ChatRoom> {
    const { data } = await http.post<ChatRoom>(`${BASE}/rooms`, payload);
    return data;
  },

  async createBranchRoom(payload: BranchRoomCreatePayload): Promise<ChatRoom> {
    const { data } = await http.post<ChatRoom>(`${BASE}/rooms/branch`, payload);
    return data;
  },

  async createDirectRoom(payload: DirectRoomCreatePayload): Promise<ChatRoom> {
    const { data } = await http.post<ChatRoom>(`${BASE}/rooms/private`, payload);
    return data;
  },

  async getMessages(roomId: string, page = 0, size = 30): Promise<ChatMessagePage> {
    const { data } = await http.get<ChatMessagePage>(`${BASE}/rooms/${roomId}/messages`, {
      params: { page, size },
    });
    return data;
  },

  async markMessagesRead(roomId: string, payload: MarkMessagesReadPayload): Promise<void> {
    await http.post(`${BASE}/rooms/${roomId}/read`, payload);
  },

  async updateMessage(roomId: string, messageId: string, payload: { messageText: string }): Promise<ChatMessage> {
    const { data } = await http.patch<ChatMessage>(`${BASE}/rooms/${roomId}/messages/${messageId}`, payload);
    return data;
  },
  async deleteMessage(roomId: string, messageId: string, isAdmin = false): Promise<void> {
    await http.delete(`${BASE}/rooms/${roomId}/messages/${messageId}`, {
      params: { isAdmin },
    });
  },
  async updateRoom(roomId: string, payload: { name?: string; description?: string }): Promise<ChatRoom> {
    const { data } = await http.put<ChatRoom>(`${BASE}/rooms/${roomId}`, payload);
    return data;
  },
  async deleteRoom(roomId: string): Promise<void> {
    await http.delete(`${BASE}/rooms/${roomId}`);
  },
  async leaveRoom(roomId: string): Promise<void> {
    await http.delete(`${BASE}/rooms/${roomId}/members/me`);
  },
  async updateMyMembership(roomId: string, payload: { nickname?: string; muted?: boolean }): Promise<ChatRoom> {
    const { data } = await http.put<ChatRoom>(`${BASE}/rooms/${roomId}/members/me`, payload);
    return data;
  },

  async uploadAttachment(roomId: string, file: File, caption?: string,): Promise<AttachmentUploadResponse> {
    const formData = new FormData();
    formData.append('file', file);
    if (caption?.trim()) {
      formData.append('caption', caption.trim());
    }
    const { data } = await http.post<AttachmentUploadResponse>(
        `${BASE}/rooms/${roomId}/attachments`,
        formData,
        {
          headers: {
          },
          timeout: 60_000,
        },
    );
    return data;
  },
};

export const userLookupApi = {
  async searchUsers(keyword: string, limit = 10): Promise<UserSearchResult[]> {
    if (!keyword.trim()) {
      return [];
    }
    const { data } = await http.get<{ success: boolean; result: UserSearchResult[] }>('/users/search', {
      params: { q: keyword.trim(), limit },
    });
    return data.result ?? [];
  },
};

