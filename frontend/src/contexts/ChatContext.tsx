import { createContext, useCallback, useContext, useEffect, useMemo, useRef, useState } from 'react';
import { Client, IMessage, StompSubscription } from '@stomp/stompjs';
import SockJS from 'sockjs-client';
import { showToast } from '@/lib/toast';
import { chatApi } from '@/api/chatApi';
import { audioNotification } from '@/utils/audio';
import type { AttachmentUploadResponse, BranchRoomCreatePayload, ChatMessage, ChatMessageSendPayload, ChatRoom, ChatRoomCreatePayload, DirectRoomCreatePayload } from '@/types/chat';

interface ChatContextValue {
  rooms: ChatRoom[];
  isLoadingRooms: boolean;
  refreshRooms: () => Promise<void>;
  selectedRoomId: string | null;
  selectRoom: (roomId: string) => void;
  currentRoom: ChatRoom | null;
  messagesByRoom: Record<string, ChatMessage[]>;
  roomMessageState: Record<string, { isLoading: boolean; hasMore: boolean; page: number; initialized: boolean }>;
  loadOlderMessages: (roomId: string) => Promise<void>;
  sendMessage: (payload: ChatMessageSendPayload) => void;
  sendAttachment: (roomId: string, file: File, caption?: string) => Promise<AttachmentUploadResponse | null>;
  updateMessage: (roomId: string, messageId: string, payload: { messageText: string }) => Promise<ChatMessage | null>;
  deleteMessage: (roomId: string, messageId: string, isAdmin?: boolean) => Promise<void>;
  updateRoom: (roomId: string, payload: { name?: string; description?: string }) => Promise<ChatRoom | null>;
  deleteRoom: (roomId: string) => Promise<void>;
  leaveRoom: (roomId: string) => Promise<void>;
  createRoom: (payload: ChatRoomCreatePayload) => Promise<ChatRoom | null>;
  createBranchRoom: (payload: BranchRoomCreatePayload) => Promise<ChatRoom | null>;
  createDirectRoom: (payload: DirectRoomCreatePayload) => Promise<ChatRoom | null>;
  isWidgetOpen: boolean;
  openWidget: () => void;
  closeWidget: () => void;
  toggleWidget: () => void;
  widgetSignal: number;
  totalUnread: number;
  unreadByRoom: Record<string, number>;
  isConnected: boolean;
  isConnecting: boolean;
  canEditRoom: (room: ChatRoom) => boolean;
  canLeaveRoom: (room: ChatRoom) => boolean;
  canDeleteRoom: (room: ChatRoom) => boolean;
  canEditMessage: (message: ChatMessage) => boolean;
  canDeleteMessage: (message: ChatMessage, room: ChatRoom) => boolean;
  deleteConversationLocally: (roomId: string) => void;
  setRooms: React.Dispatch<React.SetStateAction<ChatRoom[]>>;
}

const ChatContext = createContext<ChatContextValue | undefined>(undefined);

interface ChatProviderProps {
  children: React.ReactNode;
}

const PAGE_SIZE = 30;

const resolveWsUrl = () => {
  const directWsUrl = import.meta.env.VITE_WS_BASE_URL as string | undefined;

  if (directWsUrl) {
    console.log('[chat-ws] Using direct WebSocket URL from VITE_WS_BASE_URL:', directWsUrl);
    return directWsUrl;
  }

  const apiBase = import.meta.env.VITE_API_BASE_URL as string | undefined;
  if (apiBase) {
    try {
      const parsedApi = new URL(apiBase);
      const protocol = parsedApi.protocol === 'https:' ? 'wss:' : 'ws:';
      const port = parsedApi.port ? `:${parsedApi.port}` : '';
      const wsUrl = `${protocol}//${parsedApi.hostname}${port}/legacy/ws/chat`;
      return wsUrl;
    } catch (error) {
      console.error('[chat-ws] Error constructing WebSocket URL:', error);
    }
  }

  const defaultUrl = 'ws://localhost:8080/legacy/ws/chat';
  console.warn(`[chat-ws] Using default WebSocket URL: ${defaultUrl}`);
  return defaultUrl;
};

export const ChatProvider = ({ children }: ChatProviderProps) => {
  const [rooms, setRooms] = useState<ChatRoom[]>([]);
  const [isLoadingRooms, setIsLoadingRooms] = useState(false);
  const [selectedRoomId, setSelectedRoomId] = useState<string | null>(null);
  const [messagesByRoom, setMessagesByRoom] = useState<Record<string, ChatMessage[]>>({});
  const [roomMessageState, setRoomMessageState] = useState<Record<string, { isLoading: boolean; hasMore: boolean; page: number; initialized: boolean }>>({});
  const [unreadByRoom, setUnreadByRoom] = useState<Record<string, number>>({});
  const [isWidgetOpen, setIsWidgetOpen] = useState(false);
  const [widgetSignal, setWidgetSignal] = useState(0);
  const [isConnected, setIsConnected] = useState(false);
  const [authToken, setAuthToken] = useState<string | null>(() => localStorage.getItem('authToken'));
  const [currentUserId, setCurrentUserId] = useState<string | null>(() => {
    try {
      const userStr = localStorage.getItem('user');
      return userStr ? JSON.parse(userStr)?.id : null;
    } catch {
      return null;
    }
  });
  const [isConnecting, setIsConnecting] = useState(false);

  const clientRef = useRef<Client | null>(null);
  const subscriptionsRef = useRef<Record<string, StompSubscription>>({});
  const wsUrlRef = useRef(resolveWsUrl());
  const isConnectingRef = useRef(false);
  const lastReadRef = useRef<Record<string, string>>({});
  const tokenPollRef = useRef<number | null>(null);
  const selectedRoomRef = useRef<string | null>(null);
  const roomsLoadedRef = useRef(false);
  const authTokenRef = useRef<string | null>(authToken);

  const currentRoom = useMemo(
      () => rooms.find((room) => room.id === selectedRoomId) ?? null,
      [rooms, selectedRoomId],
  );

  const totalUnread = Object.values(unreadByRoom).reduce((sum, val) => sum + (val || 0), 0);

  useEffect(() => {
    const event = new CustomEvent('chatUnreadChanged', { detail: totalUnread });
    window.dispatchEvent(event);
  }, [totalUnread]);



  const updateMessage = useCallback(async (roomId: string, messageId: string, payload: { messageText: string }) => {
    if (!roomId || !messageId) {
      console.error('Missing roomId or messageId for updateMessage');
      return null;
    }
    try {
      const updated = await chatApi.updateMessage(roomId, messageId, payload);
      if (updated) {
        setMessagesByRoom(prev => ({
          ...prev,
          [roomId]: (prev[roomId] || []).map(m => m.id === messageId ? updated : m)
        }));
      }
      return updated;
    } catch (err) {
      showToast.error('Sửa tin nhắn thất bại');
      return null;
    }
  }, []);

  const deleteMessage = useCallback(async (roomId: string, messageId: string, isAdmin = false) => {
    try {
      await chatApi.deleteMessage(roomId, messageId, isAdmin);
      setMessagesByRoom(prev => ({
        ...prev,
        [roomId]: (prev[roomId] || []).map(m => m.id === messageId ? { ...m, deleted: true, messageText: '' } : m)
      }));
    } catch (err) {
      showToast.error('Xóa tin nhắn thất bại');
    }
  }, []);

  const updateRoom = useCallback(async (roomId: string, payload: { name?: string; description?: string }) => {
    if (!roomId) {
      console.error('Missing roomId for updateRoom');
      return null;
    }
    try {
      const updatedRoom = await chatApi.updateRoom(roomId, payload);
      if (updatedRoom) {
        setRooms(prev => prev.map(r => r.id === roomId ? updatedRoom : r));
      }
      return updatedRoom;
    } catch (err) {
      showToast.error('Cập nhật phòng thất bại');
      return null;
    }
  }, []);

  const deleteRoom = useCallback(async (roomId: string) => {
    try {
      await chatApi.deleteRoom(roomId);
      setRooms(prev => prev.filter(r => r.id !== roomId));
      setMessagesByRoom(prev => { const { [roomId]: _, ...rest } = prev; return rest; });
      if (selectedRoomId === roomId) setSelectedRoomId(null);
      showToast.success('Đã xóa phòng chat');
    } catch (err) {
      showToast.error('Xóa phòng thất bại');
    }
  }, [selectedRoomId]);

  const leaveRoom = useCallback(async (roomId: string) => {
    try {
      await chatApi.leaveRoom(roomId);
      setRooms(prev => prev.filter(r => r.id !== roomId));
      setMessagesByRoom(prev => { const { [roomId]: _, ...rest } = prev; return rest; });
      if (subscriptionsRef.current[roomId]) {
        subscriptionsRef.current[roomId].unsubscribe();
        delete subscriptionsRef.current[roomId];
      }
      if (selectedRoomId === roomId) {
        setSelectedRoomId(null);
      }
      showToast.success('Đã rời phòng chat');
    } catch (err) {
      showToast.error('Rời phòng thất bại');
    }
  }, [selectedRoomId]);

  const resetState = useCallback(() => {
    setRooms([]);
    setMessagesByRoom({});
    setRoomMessageState({});
    setUnreadByRoom({});
    setSelectedRoomId(null);
    Object.values(subscriptionsRef.current).forEach((sub) => sub.unsubscribe());
    subscriptionsRef.current = {};
    if (clientRef.current) {
      clientRef.current.deactivate();
      clientRef.current = null;
    }
    setIsConnected(false);
  }, []);

  const markAsRead = useCallback(
      async (roomId: string, lastMessageId: string) => {
        if (!authToken || !lastMessageId) return;
        if (lastReadRef.current[roomId] === lastMessageId) return;

        setUnreadByRoom((prev) => {
          if (prev[roomId] === 0) return prev;
          return {
            ...prev,
            [roomId]: 0,
          };
        });

        lastReadRef.current[roomId] = lastMessageId;
        try {
          await chatApi.markMessagesRead(roomId, { lastMessageId });
        } catch (err) {
          console.error('Failed to mark messages read', err);
        }
      },
      [authToken],
  );

  const loadOlderMessages = useCallback(
      async (roomId: string, reset = false) => {
        if (!authToken) return;
        setRoomMessageState((prev) => ({
          ...prev,
          [roomId]: {
            ...(prev[roomId] ?? { page: 0, hasMore: true, initialized: false }),
            isLoading: true,
          },
        }));
        try {
          const targetPage = reset ? 0 : (roomMessageState[roomId]?.page ?? 0);
          const response = await chatApi.getMessages(roomId, targetPage, PAGE_SIZE);
          if (!response || !response.messages) {
            throw new Error('Invalid message response');
          }
          const nextMessages = [...response.messages].reverse();
          setMessagesByRoom((prev) => {
            const existing = prev[roomId] ?? [];
            const merged = reset ? nextMessages : [...nextMessages, ...existing];
            return {
              ...prev,
              [roomId]: merged.sort(
                  (a, b) => new Date(a.createdAt).getTime() - new Date(b.createdAt).getTime(),
              ),
            };
          });
          setRoomMessageState((prev) => ({
            ...prev,
            [roomId]: {
              isLoading: false,
              hasMore: response.hasMore ?? true,
              page: reset ? 1 : (prev[roomId]?.page ?? 0) + 1,
              initialized: true,
            },
          }));
          if (reset && response.messages.length) {
            const latest = response.messages[0];
            if (latest) {
              markAsRead(roomId, latest.id);
            }
          }
        } catch (err) {
          console.error('Failed to load messages', err);
          setRoomMessageState((prev) => ({
            ...prev,
            [roomId]: {
              ...(prev[roomId] ?? { page: 0, hasMore: true, initialized: false }),
              isLoading: false,
            },
          }));
          showToast.error('Không thể tải tin nhắn');
        }
      },
      [authToken, markAsRead],
  );

  const ensureMessagesLoaded = useCallback(
      async (roomId: string) => {
        const state = roomMessageState[roomId];
        if (state?.initialized) {
          return;
        }
        await loadOlderMessages(roomId, true);
      },
      [loadOlderMessages],
  );

  const bumpRoomToTop = useCallback(
      (roomId: string, lastActivity?: string) => {
        setRooms((prev) => {
          const index = prev.findIndex((room) => room.id === roomId);
          if (index === -1) {
            return prev;
          }
          const room = prev[index];
          const updatedRoom =
              lastActivity && room.updatedAt !== lastActivity ? { ...room, updatedAt: lastActivity } : room;
          const remaining = [...prev.slice(0, index), ...prev.slice(index + 1)];
          return [updatedRoom, ...remaining];
        });
      },
      [],
  );

  const subscribeRoom = useCallback(
      (roomId: string) => {
        if (!clientRef.current || !isConnected || subscriptionsRef.current[roomId]) {
          return;
        }
        const subscription = clientRef.current.subscribe(`/topic/chat/${roomId}`, (frame: IMessage) => {
          try {
            const payload = JSON.parse(frame.body) as ChatMessage;
            if (payload.messageType === 'system' && payload.messageText?.includes('deleted')) {

              setRooms(prev => prev.filter(r => r.id !== roomId));
              setMessagesByRoom(prev => {
                const { [roomId]: _, ...rest } = prev;
                return rest;
              });
              if (selectedRoomId === roomId) {
                setSelectedRoomId(null);
              }
              showToast.info('Phòng chat đã bị xóa');
              return;
            }

            bumpRoomToTop(roomId, payload.createdAt);
            setMessagesByRoom((prev) => {
              const existing = prev[roomId] ?? [];
              const alreadyExists = existing.some((msg) => msg.id === payload.id);
              if (alreadyExists) {
                return {
                  ...prev,
                  [roomId]: existing.map((msg) => (msg.id === payload.id ? payload : msg)),
                };
              }
              return {
                ...prev,
                [roomId]: [...existing, payload].sort(
                    (a, b) => new Date(a.createdAt).getTime() - new Date(b.createdAt).getTime(),
                ),
              };
            });
            if (payload.messageType !== 'system' && payload.senderId != currentUserId && (roomId !== selectedRoomRef.current || !isWidgetOpen)) {
              setUnreadByRoom((prev) => ({
                ...prev,
                [roomId]: (prev[roomId] || 0) + 1,
              }));

              if (payload.senderId !== currentUserId) {
                const room = rooms.find(r => r.id === roomId);
                const myMember = room?.members.find(m => m.userId === currentUserId);
                if (room && !myMember?.muted) {
                  audioNotification.play().catch(() => {});
                }
              }
            } else if (payload.messageType === 'system' && roomId === selectedRoomRef.current) {

            }
          } catch (err) {
            console.error('Failed to parse chat message', err);
          }
        });
        subscriptionsRef.current[roomId] = subscription;
      },
      [bumpRoomToTop, isConnected, isWidgetOpen, markAsRead],
  );

  const connectWebsocket = useCallback(() => {
    if (!authToken) {
      console.warn('[chat-ws] No auth token, skipping WebSocket initialization');
      return;
    }

    if (clientRef.current) {
      console.log('[chat-ws] Cleaning up existing WebSocket connection');
      try {
        const deactivatePromise = clientRef.current.deactivate();
        if (deactivatePromise && typeof deactivatePromise.catch === 'function') {
          deactivatePromise.catch(e => {
            console.warn('[chat-ws] Error while deactivating existing client', e);
          });
        }
      } catch (e) {
        console.warn('[chat-ws] Error in deactivate', e);
      } finally {
        clientRef.current = null;
      }
    }

    if (isConnectingRef.current) {
      console.log('[chat-ws] Connection already in progress, skipping');
      return;
    }

    isConnectingRef.current = true;
    setIsConnecting(true);

    const wsUrl = wsUrlRef.current;
    console.log('[chat-ws] Initializing WebSocket connection to:', wsUrl);

    try {
      const useSockJS = wsUrl.startsWith('http');

      let webSocketFactory;
      let brokerURL = wsUrl;

      try {
        if (useSockJS) {
          const sockJsUrl = wsUrl.startsWith('ws:')
              ? wsUrl.replace(/^ws:/, 'http:')
              : wsUrl.startsWith('wss:')
                  ? wsUrl.replace(/^wss:/, 'https:')
                  : wsUrl;

          console.log('[chat-ws] Using SockJS with URL:', sockJsUrl);
          webSocketFactory = () => new SockJS(sockJsUrl);
        } else {
          console.log('[chat-ws] Using native WebSocket with URL:', wsUrl);
          brokerURL = wsUrl;
        }
      } catch (error) {
        console.error('[chat-ws] Error initializing WebSocket factory:', error);
        throw error;
      }

      const client = new Client({
        brokerURL: useSockJS ? undefined : brokerURL,
        webSocketFactory: useSockJS ? webSocketFactory : undefined,
        connectHeaders: {
          Authorization: `Bearer ${authToken}`,
        },
        reconnectDelay: 5000,
        heartbeatIncoming: 20000,
        heartbeatOutgoing: 20000,
        debug: (msg) => {
          console.debug('[chat-ws]', msg);
        },
        onConnect: () => {
          console.log('[chat-ws] Successfully connected to WebSocket');
          isConnectingRef.current = false;
          setIsConnecting(false);
          setIsConnected(true);

          Object.keys(subscriptionsRef.current).forEach(roomId => {
            subscribeRoom(roomId);
          });
        },
        onStompError: (frame) => {
          console.error('[chat-ws] STOMP protocol error:', frame.headers?.['message'], frame.body);
          isConnectingRef.current = false;
          setIsConnecting(false);
          setIsConnected(false);

          if (isConnected) {
            showToast.error('Connection to chat server lost. Reconnecting...');
          }
        },
        onWebSocketClose: (event) => {
          console.warn(`[chat-ws] WebSocket closed (code: ${event?.code}, reason: ${event?.reason || 'No reason provided'})`);
          isConnectingRef.current = false;
          setIsConnecting(false);
          setIsConnected(false);

          clientRef.current = null;
        },
        onWebSocketError: (event) => {
          console.error('[chat-ws] WebSocket error:', event);
          isConnectingRef.current = false;
          setIsConnecting(false);
          setIsConnected(false);

          if (isConnected) {
            showToast.error('Connection error. Reconnecting...');
          }
        },
        onDisconnect: () => {
          console.log('[chat-ws] Disconnected from WebSocket');
          isConnectingRef.current = false;
          setIsConnecting(false);
          setIsConnected(false);

          clientRef.current = null;
        },
      });

      clientRef.current = client;

      console.log('[chat-ws] Activating WebSocket connection...');
      try {
        const activateResult = client.activate();

        if (activateResult != null && typeof activateResult === 'object' && 'then' in activateResult && typeof (activateResult as any).then === 'function') {
          (activateResult as Promise<void>).catch((error: Error) => {
            console.error('[chat-ws] Failed to activate WebSocket connection:', error);
            isConnectingRef.current = false;
            setIsConnecting(false);
            clientRef.current = null;

            if (!isConnected) {
              showToast.error('Không thể kết nối đến máy chủ chat. Vui lòng thử lại sau.');
            }
          });
        } else {
          console.log('[chat-ws] WebSocket activation initiated');
        }
      } catch (error) {
        console.error('[chat-ws] Error activating WebSocket connection:', error);
        isConnectingRef.current = false;
        setIsConnecting(false);
        clientRef.current = null;
      }
    } catch (error) {
      console.error('[chat-ws] Error initializing WebSocket connection:', error);
      isConnectingRef.current = false;
      setIsConnecting(false);
      clientRef.current = null;
    }
  }, [authToken]);

  const refreshRooms = useCallback(async () => {
    if (!authToken) return;
    setIsLoadingRooms(true);
    try {
      const data = await chatApi.getMyRooms();
      const sorted = [...data].sort((a, b) => {
        const timeA = new Date(a.updatedAt ?? a.createdAt ?? 0).getTime();
        const timeB = new Date(b.updatedAt ?? b.createdAt ?? 0).getTime();
        return timeB - timeA;
      });
      setRooms(sorted);

      const initialUnread: Record<string, number> = {};
      const unreadPromises = data.map(async (room) => {
        const myMember = room.members.find(m => m.userId === currentUserId);
        if (!myMember) return { roomId: room.id, count: 0 };
        const lastReadTime = myMember.lastReadAt ? new Date(myMember.lastReadAt).getTime() : 0;
        const messagePage = await chatApi.getMessages(room.id, 0, 10);

        let unreadCount = 0;
        for (const message of messagePage.messages) {
          const messageTime = new Date(message.createdAt).getTime();

          if (messageTime > lastReadTime && message.messageType !== 'system') {
            unreadCount++;
          } else {break;}
        }
        return { roomId: room.id, count: unreadCount };
      });

      const unreadResults = await Promise.all(unreadPromises);

      unreadResults.forEach(result => {
        if (result.count > 0) {
          initialUnread[result.roomId] = result.count;
        }
      });

      setUnreadByRoom(initialUnread);

      setRoomMessageState((prev) => {
        const updated = { ...prev };
        for (const room of sorted) {
          updated[room.id] = updated[room.id] ?? { isLoading: false, hasMore: true, page: 0, initialized: false };
        }
        return updated;
      });
      setSelectedRoomId((prev) => prev ?? (sorted.length ? sorted[0].id : null));
      roomsLoadedRef.current = true;
    } catch (err) {
      console.error('Failed to load chat rooms', err);
      showToast.error('Không thể tải phòng chat');
    } finally {
      setIsLoadingRooms(false);
    }
  }, [authToken, currentUserId]);

  const selectRoom = useCallback(
      async (roomId: string) => {
        setSelectedRoomId(roomId);
        await ensureMessagesLoaded(roomId);

        subscribeRoom(roomId);

        const roomMessages = messagesByRoom[roomId];
        const latest =
            roomMessages && roomMessages.length > 0
                ? roomMessages[roomMessages.length - 1]
                : null;

        if (latest) {
          markAsRead(roomId, latest.id);
        }
      },
      [ensureMessagesLoaded, markAsRead, messagesByRoom, subscribeRoom],
  );

  const sendMessage = useCallback(
      (payload: ChatMessageSendPayload) => {
        if (!clientRef.current || !isConnected) {
          showToast.error('Chat chưa sẵn sàng, vui lòng thử lại');
          return;
        }
        try {
          clientRef.current.publish({
            destination: '/app/chat/send',
            body: JSON.stringify(payload),
            headers: {
              Authorization: `Bearer ${authToken}`,
            },
          });
          bumpRoomToTop(payload.roomId);
        } catch (err) {
          console.error('Failed to send chat message', err);
          showToast.error('Gửi tin nhắn thất bại');
        }
      },
      [authToken, bumpRoomToTop, isConnected],
  );

  const sendAttachment = useCallback(
      async (roomId: string, file: File, caption?: string) => {
        try {
          const response = await chatApi.uploadAttachment(roomId, file, caption);
          console.log('[sendAttachment] Response:', response);
          if (response?.message) {
            console.log('[sendAttachment] Adding message to state:', response.message);
            setMessagesByRoom((prev) => {
              const roomMessages = prev[roomId] ?? [];
              const alreadyExists = roomMessages.some((msg) => msg.id === response.message.id);
              if (alreadyExists) {
                return prev;
              }
              return {
                ...prev,
                [roomId]: [...roomMessages, response.message].sort(
                    (a, b) => new Date(a.createdAt).getTime() - new Date(b.createdAt).getTime(),
                ),
              };
            });
            bumpRoomToTop(roomId, response.message.createdAt);
          } else {
            console.warn('[sendAttachment] No message in response:', response);
          }
          return response;
        } catch (err) {
          console.error('Failed to upload attachment', err);
          showToast.error('Tải tệp thất bại');
          return null;
        }
      },
      [bumpRoomToTop],
  );

  const createRoom = useCallback(
      async (payload: ChatRoomCreatePayload) => {
        try {
          const room = await chatApi.createRoom(payload);
          setRooms((prev) => [room, ...prev.filter((existing) => existing.id !== room.id)]);
          subscribeRoom(room.id);
          showToast.success('Đã tạo phòng chat');
          return room;
        } catch (err) {
          console.error('Create room failed', err);
          showToast.error('Không tạo được phòng chat');
          return null;
        }
      },
      [subscribeRoom],
  );

  const createBranchRoom = useCallback(
      async (payload: BranchRoomCreatePayload) => {
        try {
          const room = await chatApi.createBranchRoom(payload);
          setRooms((prev) => {
            const filtered = prev.filter((r) => r.id !== room.id);
            return [room, ...filtered];
          });
          subscribeRoom(room.id);
          showToast.success('Đã tạo phòng nhánh');
          return room;
        } catch (err) {
          console.error('Create branch room failed', err);
          showToast.error('Không tạo được phòng nhánh');
          return null;
        }
      },
      [subscribeRoom],
  );

  const createDirectRoom = useCallback(
      async (payload: DirectRoomCreatePayload) => {
        try {
          const room = await chatApi.createDirectRoom(payload);
          setRooms((prev) => {
            const filtered = prev.filter((r) => r.id !== room.id);
            return [room, ...filtered];
          });
          subscribeRoom(room.id);
          showToast.success('Đã mở chat riêng');
          return room;
        } catch (err) {
          console.error('Create direct room failed', err);
          showToast.error('Không tạo được chat riêng');
          return null;
        }
      },
      [subscribeRoom],
  );

  const openWidget = useCallback(() => {
    if (!authToken) {
      showToast.warning('Vui lòng đăng nhập để sử dụng chat');
      return;
    }
    setIsWidgetOpen(true);
    setWidgetSignal(Date.now());
  }, [authToken]);

  const closeWidget = useCallback(() => setIsWidgetOpen(false), []);
  const toggleWidget = useCallback(() => setIsWidgetOpen((prev) => !prev), []);

  useEffect(() => {
    selectedRoomRef.current = selectedRoomId;
  }, [selectedRoomId]);

  useEffect(() => {
    const syncToken = () => {
      const t = localStorage.getItem('authToken');
      if (t !== authTokenRef.current) {
        authTokenRef.current = t;
        setAuthToken(t);
      }
      try {
        const userStr = localStorage.getItem('user');
        const uid = userStr ? JSON.parse(userStr)?.id : null;
        if (uid !== currentUserId) {
          setCurrentUserId(uid);
        }
      } catch (e) {
        // ignore
      }
    };
    const focusListener = () => syncToken();
    window.addEventListener('storage', syncToken);
    window.addEventListener('focus', focusListener);
    tokenPollRef.current = window.setInterval(syncToken, 5000);
    return () => {
      window.removeEventListener('storage', syncToken);
      window.removeEventListener('focus', focusListener);
      if (tokenPollRef.current) {
        clearInterval(tokenPollRef.current);
      }
    };
  }, []);

  useEffect(() => {
    if (!authToken) {
      resetState();
    }
  }, [authToken, resetState]);

  useEffect(() => {
    authTokenRef.current = authToken;
  }, [authToken]);

  useEffect(() => {
    if (!authToken) {
      console.log('[chat-ws] No auth token, skipping WebSocket connection');
      return;
    }

    console.log('[chat-ws] Auth token available, initializing WebSocket connection');

    const timer = setTimeout(() => {
      try {
        connectWebsocket();
      } catch (error) {
        console.error('[chat-ws] Error in connectWebsocket:', error);
        isConnectingRef.current = false;
        setIsConnecting(false);
      }
    }, 100);

    return () => {
      clearTimeout(timer);
      console.log('[chat-ws] Cleaning up WebSocket connection');

      const cleanupClient = () => {
        if (clientRef.current) {
          try {
            console.log('[chat-ws] Deactivating WebSocket client');
            const deactivatePromise = clientRef.current.deactivate();
            if (deactivatePromise && typeof deactivatePromise.catch === 'function') {
              deactivatePromise.catch(e => {
                console.warn('[chat-ws] Error while deactivating WebSocket client', e);
              });
            }
          } catch (e) {
            console.warn('[chat-ws] Error in deactivate', e);
          } finally {
            clientRef.current = null;
          }
        }
      };

      cleanupClient();
      isConnectingRef.current = false;
      setIsConnecting(false);
      setIsConnected(false);
    };
  }, [authToken]);

  useEffect(() => {
    if (!authToken) {
      return;
    }
    if (!roomsLoadedRef.current) {
      refreshRooms();
    }
  }, [authToken]);

  useEffect(() => {
    if (!isConnected || !rooms.length) return;
    rooms.forEach((room) => subscribeRoom(room.id));
  }, [authToken, isConnected, subscribeRoom]);

  useEffect(() => {
    if (!currentRoom) return;
    ensureMessagesLoaded(currentRoom.id);
  }, [currentRoom?.id]);

  useEffect(() => {
    return () => {
      resetState();
    };
  }, []);

  // Permission helpers
  const canEditRoom = useCallback((room: ChatRoom) => {
    if (!currentUserId) return false;
    if (room.roomType === 'family' || room.roomType === 'private_chat') return false;
    const member = room.members.find(m => m.userId === currentUserId);
    return member?.role === 'admin' || member?.role === 'moderator';
  }, [currentUserId]);

  const canLeaveRoom = useCallback((room: ChatRoom) => {
    if (!currentUserId) return false;
    if (room.roomType === 'family' || room.roomType === 'private_chat') return false;
    return true;
  }, [currentUserId]);

  const canDeleteRoom = useCallback((room: ChatRoom) => {
    if (!currentUserId) return false;
    if (room.roomType !== 'branch' && room.roomType !== 'group') return false;
    const member = room.members.find(m => m.userId === currentUserId);
    return member?.role === 'admin';
  }, [currentUserId]);

  const canEditMessage = useCallback((message: ChatMessage) => {
    if (!currentUserId) return false;
    return message.senderId === currentUserId;
  }, [currentUserId]);

  const canDeleteMessage = useCallback((message: ChatMessage, room: ChatRoom) => {
    if (!currentUserId) return false;
    if (message.senderId === currentUserId) return true;
    if (room.roomType === 'private_chat') return false;
    const member = room.members.find(m => m.userId === currentUserId);
    return member?.role === 'admin' || member?.role === 'moderator';
  }, [currentUserId]);

  const deleteConversationLocally = useCallback((roomId: string) => {
    setRooms(prev => prev.filter(r => r.id !== roomId));
    setMessagesByRoom(prev => { const { [roomId]: _, ...rest } = prev; return rest; });
    setRoomMessageState(prev => { const { [roomId]: _, ...rest } = prev; return rest; });
    setUnreadByRoom(prev => { const { [roomId]: _, ...rest } = prev; return rest; });
    if (selectedRoomId === roomId) setSelectedRoomId(null);
    if (subscriptionsRef.current[roomId]) {
      subscriptionsRef.current[roomId].unsubscribe();
      delete subscriptionsRef.current[roomId];
    }
    showToast.success('Đã xóa hội thoại khỏi thiết bị này');
  }, [selectedRoomId]);

  const contextValue = useMemo(
      () => ({
        rooms,
        isLoadingRooms,
        refreshRooms,
        selectedRoomId,
        selectRoom,
        currentRoom,
        messagesByRoom,
        roomMessageState,
        loadOlderMessages,
        sendMessage,
        sendAttachment,
        updateMessage,
        deleteMessage,
        updateRoom,
        deleteRoom,
        leaveRoom,
        createRoom,
        createBranchRoom,
        createDirectRoom,
        isWidgetOpen,
        openWidget,
        closeWidget,
        toggleWidget,
        widgetSignal,
        totalUnread,
        unreadByRoom,
        isConnected,
        isConnecting,
        canEditRoom,
        canLeaveRoom,
        canDeleteRoom,
        canEditMessage,
        canDeleteMessage,
        deleteConversationLocally,
        setRooms,
      }),
      [
        rooms,
        isLoadingRooms,
        refreshRooms,
        selectedRoomId,
        selectRoom,
        currentRoom,
        messagesByRoom,
        roomMessageState,
        loadOlderMessages,
        sendMessage,
        sendAttachment,
        updateMessage,
        deleteMessage,
        updateRoom,
        deleteRoom,
        leaveRoom,
        createRoom,
        createBranchRoom,
        createDirectRoom,
        isWidgetOpen,
        openWidget,
        closeWidget,
        toggleWidget,
        widgetSignal,
        totalUnread,
        unreadByRoom,
        isConnected,
        isConnecting,
        canEditRoom,
        canLeaveRoom,
        canDeleteRoom,
        canEditMessage,
        canDeleteMessage,
        deleteConversationLocally,
        setRooms,
      ],
  );

  return <ChatContext.Provider value={contextValue}>{children}</ChatContext.Provider>;
};

export const useChat = () => {
  const context = useContext(ChatContext);
  if (!context) {
    throw new Error('useChat must be used within ChatProvider');
  }
  return context;
};
