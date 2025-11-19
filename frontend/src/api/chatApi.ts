import { http } from '@/api/http';
import type {
  AttachmentUploadResponse,
  BranchRoomCreatePayload,
  ChatMessagePage,
  ChatRoom,
  ChatRoomCreatePayload,
  DirectRoomCreatePayload,
  JoinRoomPayload,
  MarkMessagesReadPayload,
  UserSearchResult,
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

  async joinRoom(roomId: string, payload: JoinRoomPayload): Promise<ChatRoom> {
    const { data } = await http.post<ChatRoom>(`${BASE}/rooms/${roomId}/join`, payload);
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

  async uploadAttachment(
    roomId: string,
    file: File,
    caption?: string,
  ): Promise<AttachmentUploadResponse> {
    const formData = new FormData();
    formData.append('file', file);
    if (caption) {
      formData.append('caption', caption);
    }
    const { data } = await http.post<AttachmentUploadResponse>(
      `${BASE}/rooms/${roomId}/attachments`,
      formData,
      {
        headers: {
          'Content-Type': 'multipart/form-data',
        },
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

