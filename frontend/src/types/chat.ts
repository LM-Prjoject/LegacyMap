export type ChatRoomType = 'family' | 'branch' | 'private_chat' | 'group';

export type ChatMemberRole = 'member' | 'admin' | 'moderator';

export interface ChatRoomMember {
  userId: string;
  username: string;
  personId: string | null;
  role: ChatMemberRole;
  muted: boolean;
  joinedAt: string;
  lastReadAt: string | null;
  nickname?: string | null;
}

export interface ChatRoom {
  id: string;
  name: string;
  description?: string | null;
  roomType: ChatRoomType;
  active: boolean;
  familyTreeId?: string | null;
  createdAt: string;
  updatedAt: string;
  members: ChatRoomMember[];
  lastMessageAt: string;
  lastMessageId?: string | null;
}

export type ChatMessageType = 'text' | 'image' | 'file' | 'system';

export interface ChatMessageRecipientStatus {
  userId: string;
  read: boolean;
  readAt: string | null;
}

export interface ChatMessage {
  id: string;
  roomId: string;
  senderId: string;
  senderName: string;
  messageText?: string | null;
  messageType: ChatMessageType;
  fileUrl?: string | null;
  fileName?: string | null;
  fileSize?: number | null;
  fileType?: string | null;
  replyToId?: string | null;
  replyToText?: string | null;
  replyToSenderName?: string | null;
  edited: boolean;
  deleted: boolean;
  createdAt: string;
  updatedAt: string;
  recipients: ChatMessageRecipientStatus[];
  pending?: boolean;
}

export interface ChatMessagePage {
  messages: ChatMessage[];
  hasMore: boolean;
}

export interface ChatMessageSendPayload {
  roomId: string;
  messageText?: string;
  messageType?: ChatMessageType;
  replyToId?: string;
  fileUrl?: string;
  fileName?: string;
  fileSize?: number;
  fileType?: string;
}

export interface MarkMessagesReadPayload {
  lastMessageId: string;
}

export interface ChatRoomCreatePayload {
  roomType: ChatRoomType;
  name: string;
  description?: string;
  familyTreeId?: string | null;
  memberUserIds?: string[];
}

export interface BranchRoomCreatePayload {
  branchPersonId: string;
  name: string;
  description?: string;
}

export interface DirectRoomCreatePayload {
  targetUserId: string;
  name?: string;
}

export interface AttachmentUploadResponse {
  fileUrl: string;
  originalName: string;
  size: number;
  contentType: string;
  message: ChatMessage;
}

export interface UserSearchResult {
  id: string;
  email: string;
  username: string;
  fullName?: string | null;
  phone?: string | null;
  avatarUrl?: string | null;
}

