import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import { Paperclip, Plus, Send, X, ChevronDown, ChevronRight, Loader2, Users, Inbox, Edit, Trash, Reply, Settings, Bell, BellOff } from 'lucide-react';
import { useChat } from '@/contexts/ChatContext';
import type { ChatRoomType, UserSearchResult, ChatMessage, ChatRoom, ChatRoomCreatePayload } from '@/types/chat';
import treeApi, { FamilyTree, Person } from '@/api/trees';
import { showToast } from '@/lib/toast';
import { userLookupApi, chatApi } from '@/api/chatApi';

type RoomFilter = 'all' | ChatRoomType | 'private';
type ViewMode = 'list' | 'chat';

const ROOM_TYPE_LABEL_MAP: Record<ChatRoomType, string> = {
  family: 'Gia phả',
  branch: 'Nhánh',
  private_chat: 'Riêng tư',
  group: 'Riêng tư',
};

interface CreateRoomFormState {
  roomType: ChatRoomType;
  name: string;
  description: string;
  familyTreeId: string;
  branchPersonId: string;
}

const defaultCreateState: CreateRoomFormState = {
  roomType: 'branch',
  name: '',
  description: '',
  familyTreeId: '',
  branchPersonId: '',
};

export const ChatWidget = () => {
  const {
    isWidgetOpen,
    closeWidget,
    rooms,
    selectRoom,
    currentRoom,
    messagesByRoom,
    roomMessageState,
    loadOlderMessages,
    sendMessage,
    sendAttachment,
    unreadByRoom,
    createRoom,
    createBranchRoom,
    createDirectRoom,
    updateMessage,
    deleteMessage,
    updateRoom,
    deleteRoom,
    leaveRoom,
    widgetSignal,
    canEditRoom,
    canLeaveRoom,
    canDeleteRoom,
    canEditMessage,
    canDeleteMessage,
    deleteConversationLocally,
    setRooms,
  } = useChat();
  const [isCollapsed, setIsCollapsed] = useState(false);
  const [viewMode, setViewMode] = useState<ViewMode>('list');
  const [filter, setFilter] = useState<RoomFilter>('all');
  const [search, setSearch] = useState('');
  const [messageInput, setMessageInput] = useState('');
  const [showCreate, setShowCreate] = useState(false);
  const [showDirect, setShowDirect] = useState(false);
  const [createState, setCreateState] = useState<CreateRoomFormState>(defaultCreateState);
  const [createMemberSearch, setCreateMemberSearch] = useState('');
  const [createMemberResults, setCreateMemberResults] = useState<UserSearchResult[]>([]);
  const [createMemberSelections, setCreateMemberSelections] = useState<UserSearchResult[]>([]);
  const [createMemberLoading, setCreateMemberLoading] = useState(false);
  const [directRoomName, setDirectRoomName] = useState('');
  const [directSearchTerm, setDirectSearchTerm] = useState('');
  const [directSearchResults, setDirectSearchResults] = useState<UserSearchResult[]>([]);
  const [directSelection, setDirectSelection] = useState<UserSearchResult | null>(null);
  const [directSearchLoading, setDirectSearchLoading] = useState(false);
  const [availableTrees, setAvailableTrees] = useState<FamilyTree[]>([]);
  const [treeMembers, setTreeMembers] = useState<Record<string, Person[]>>({});
  const [loadingTrees, setLoadingTrees] = useState(false);
  const [sending, setSending] = useState(false);
  const [showQuickActions, setShowQuickActions] = useState(false);
  const [showAllFilters, setShowAllFilters] = useState(false);
  const [showMembersList, setShowMembersList] = useState(false);
  const [editingMessageId, setEditingMessageId] = useState<string | null>(null);
  const [editingText, setEditingText] = useState('');
  const [replyingTo, setReplyingTo] = useState<ChatMessage | null>(null);
  const [showRoomMenu, setShowRoomMenu] = useState(false);
  const [showMessageMenu, setShowMessageMenu] = useState<string | null>(null);
  const [showNicknameModal, setShowNicknameModal] = useState(false);
  const [tempNickname, setTempNickname] = useState('');
  const [showEditBranchNameModal, setShowEditBranchNameModal] = useState(false);
  const [tempBranchName, setTempBranchName] = useState('');
  const [uploadingFiles, setUploadingFiles] = useState<Set<string>>(new Set());
  const messageEndRef = useRef<HTMLDivElement>(null);
  const fileInputRef = useRef<HTMLInputElement>(null);
  const [currentUserId, setCurrentUserId] = useState<string | null>(null);
  const createMemberSearchTimer = useRef<NodeJS.Timeout | null>(null);
  const directSearchTimer = useRef<NodeJS.Timeout | null>(null);
  const membersListRef = useRef<HTMLDivElement>(null);

  const currentMessages = useMemo(
    () => (currentRoom ? messagesByRoom[currentRoom.id] ?? [] : []),
    [currentRoom, messagesByRoom],
  );

  const currentRoomState = currentRoom ? roomMessageState[currentRoom.id] : undefined;

  const getDisplayName = useCallback((room: ChatRoom | null): string => {
    if (!room) return 'Phòng chat';
    if (room.roomType !== 'private_chat') return room.name;

    const otherMember = room.members.find(m => m.userId !== currentUserId);
    if (!otherMember) return 'Chat riêng tư';

    return otherMember.nickname?.trim() || otherMember.username || 'Người dùng';
  }, [currentUserId]);

  const isRoomMuted = useCallback((roomId: string) => {
    if (!currentRoom || currentRoom.id !== roomId) return false;
    const myMember = currentRoom.members.find(m => m.userId === currentUserId);
    return myMember?.muted ?? false;
  }, [currentRoom, currentUserId]);

  const toggleMuteRoom = useCallback(async (roomId: string) => {
    if (!roomId || !currentRoom) return;

    const currentMuted = isRoomMuted(roomId);
    const newMuted = !currentMuted;

    try {
      await chatApi.updateMyMembership(roomId, { muted: newMuted });

      setRooms(prevRooms =>
        prevRooms.map(room =>
          room.id === roomId
            ? {
              ...room,
              members: room.members.map(m =>
                m.userId === currentUserId ? { ...m, muted: newMuted } : m
              )
            }
            : room
        )
      );

      showToast.success(newMuted ? 'Đã tắt thông báo' : 'Đã bật thông báo');
    } catch (err) {
      showToast.error('Cập nhật thất bại');
    }
  }, [currentRoom, currentUserId, isRoomMuted]);

  const setNicknameForPrivateRoom = useCallback(() => {
    if (!currentRoom || currentRoom.roomType !== 'private_chat') return;

    const otherMember = currentRoom.members.find(m => m.userId !== currentUserId);
    if (!otherMember) return;

    const currentNickname = otherMember.nickname || otherMember.username;
    setTempNickname(currentNickname);
    setShowNicknameModal(true);
  }, [currentRoom, currentUserId]);

  const getUserLabel = useCallback(
    (user: UserSearchResult) => user.fullName?.trim() || user.username || user.email,
    [],
  );

  const getUserSecondaryLabel = useCallback(
    (user: UserSearchResult) => user.email || user.phone || user.username,
    [],
  );

  const renderMessageContent = useCallback((message: ChatMessage) => {
    if (message.deleted) {
      return <p className="italic text-white/50 m-0">Tin nhắn đã bị xóa</p>;
    }

    if (message.messageType === 'image' && message.fileUrl) {
      return (
        <img
          src={message.fileUrl}
          alt={message.fileName || 'Ảnh'}
          loading="lazy"
          className="rounded-lg w-full max-w-xs h-auto object-cover cursor-pointer hover:opacity-90 transition"
          onClick={() => window.open(message.fileUrl!, '_blank')}
        />
      );
    }

    if (message.messageType === 'file' && message.fileUrl) {
      const fileExt = message.fileName?.split('.').pop()?.toLowerCase() || '';
      const isPdf = fileExt === 'pdf';
      const isDoc = ['doc', 'docx', 'docm'].includes(fileExt);
      const isExcel = ['xls', 'xlsx', 'xlsm'].includes(fileExt);
      const isPowerPoint = ['ppt', 'pptx'].includes(fileExt);

      const iconBg = isPdf
        ? 'bg-red-600'
        : isDoc
          ? 'bg-blue-600'
          : isExcel
            ? 'bg-green-600'
            : isPowerPoint
              ? 'bg-orange-600'
              : 'bg-gray-600';

      const iconText = isPdf
        ? 'PDF'
        : isDoc
          ? 'DOC'
          : isExcel
            ? 'XLS'
            : isPowerPoint
              ? 'PPT'
              : fileExt.toUpperCase().slice(0, 3);

      return (
        <a
          href={message.fileUrl}
          target="_blank"
          rel="noopener noreferrer"
          className="inline-flex items-center gap-2.5 px-3 py-2.5 bg-white/8 hover:bg-white/12 rounded-xl transition-all shadow-sm max-w-[180px]"          >

          {/* Icon file */}
          <div className={`w-8 h-9 ${iconBg} rounded flex flex-col items-center justify-center text-white text-[10px] font-bold leading-tight flex-shrink-0`}>
            <span>{iconText}</span>
          </div>

          {/* Thông tin file */}
          <div className="flex-1 min-w-0">
            <p className="text-xs font-medium text-white truncate leading-tight">
              {message.fileName || 'Tệp đính kèm'}
            </p>
            <p className="text-[10px] text-white/50 mt-0.5">
              {message.fileSize
                ? `${(message.fileSize / 1024 / 1024).toFixed(1).replace('.0', '')} MB`
                : 'Đang tải...'}
            </p>
          </div>

          {/* Mũi tên tải xuống nhỏ */}
          <div className="flex-shrink-0">
            <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" className="text-white/60">
              <path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4" />
              <polyline points="7 10 12 15 17 10" />
              <line x1="12" y1="15" x2="12" y2="3" />
            </svg>
          </div>
        </a>
      );
    }

    return <p className="whitespace-pre-line m-0 break-words">{message.messageText}</p>;
  }, []);

  const filterTabs: { key: RoomFilter; label: string }[] = [
    { key: 'all', label: 'Tất cả' },
    { key: 'family', label: 'Gia phả' },
    { key: 'branch', label: 'Nhánh' },
    { key: 'private', label: 'Riêng tư' },
  ];

  const runMemberSearch = useCallback(
    async (term: string) => {
      try {
        const results = await userLookupApi.searchUsers(term);
        setCreateMemberResults(results.filter((user) => !createMemberSelections.some((sel) => sel.id === user.id)));
      } catch (err) {
        console.error('Failed to search users', err);
      } finally {
        setCreateMemberLoading(false);
      }
    },
    [createMemberSelections],
  );

  const runDirectSearch = useCallback(async (term: string) => {
    try {
      const results = await userLookupApi.searchUsers(term);
      setDirectSearchResults(results);
    } catch (err) {
      console.error('Failed to search users', err);
    } finally {
      setDirectSearchLoading(false);
    }
  }, []);

  const filteredRooms = useMemo(() => {
    const keyword = search.toLowerCase().trim();
    return rooms
      .filter((room) => {
        if (filter === 'private') {
          return room.roomType === 'private_chat' || room.roomType === 'group';
        }
        return filter === 'all' || room.roomType === filter;
      })
      .filter((room) => (keyword ? room.name.toLowerCase().includes(keyword) : true))
      .sort((a, b) => {
        const unreadA = unreadByRoom[a.id] || 0;
        const unreadB = unreadByRoom[b.id] || 0;
        if (unreadA > 0 && unreadB === 0) return -1;
        if (unreadB > 0 && unreadA === 0) return 1;
        const timeA = new Date(a.updatedAt ?? a.createdAt ?? 0).getTime();
        const timeB = new Date(b.updatedAt ?? b.createdAt ?? 0).getTime();
        if (timeA !== timeB) {
          return timeB - timeA;
        }
        return a.name.localeCompare(b.name);
      });
  }, [filter, rooms, search, unreadByRoom]);

  useEffect(() => {
    if (!isWidgetOpen) return;
    try {
      const userStr = localStorage.getItem('user');
      if (userStr) {
        const parsed = JSON.parse(userStr);
        setCurrentUserId(parsed?.id ?? null);
      } else {
        setCurrentUserId(null);
      }
    } catch (err) {
      console.error('Failed to parse user', err);
      setCurrentUserId(null);
    }
  }, [isWidgetOpen]);

  useEffect(() => {
    if (!isWidgetOpen) {
      return;
    }
    setViewMode('list');
    setIsCollapsed(false);
  }, [isWidgetOpen, widgetSignal]);

  useEffect(() => {
    setShowMembersList(false);
  }, [currentRoom?.id, isWidgetOpen]);

  useEffect(() => {
    if (!showMembersList) return;
    const handleClickOutside = (event: MouseEvent) => {
      if (membersListRef.current && !membersListRef.current.contains(event.target as Node)) {
        setShowMembersList(false);
      }
    };
    document.addEventListener('mousedown', handleClickOutside);
    return () => {
      document.removeEventListener('mousedown', handleClickOutside);
    };
  }, [showMembersList]);

  useEffect(() => {
    if (!isWidgetOpen || !currentRoom) return;
    messageEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  }, [currentRoom, currentMessages.length, isWidgetOpen]);

  useEffect(() => {
    setShowMessageMenu(null);
  }, [currentRoom?.id, viewMode, isWidgetOpen]);

  useEffect(() => {
    if (!showCreate) {
      if (createMemberSearch !== '') {
        setCreateMemberSearch('');
      }
      if (createMemberResults.length) {
        setCreateMemberResults([]);
      }
      if (createMemberLoading) {
        setCreateMemberLoading(false);
      }
      if (createMemberSelections.length) {
        setCreateMemberSelections([]);
      }
      if (createMemberSearchTimer.current) {
        clearTimeout(createMemberSearchTimer.current);
      }
      return;
    }
    if (!createMemberSearch.trim() || createMemberSearch.trim().length < 2) {
      setCreateMemberResults([]);
      setCreateMemberLoading(false);
      if (createMemberSearchTimer.current) {
        clearTimeout(createMemberSearchTimer.current);
      }
      return;
    }
    setCreateMemberLoading(true);
    if (createMemberSearchTimer.current) {
      clearTimeout(createMemberSearchTimer.current);
    }
    createMemberSearchTimer.current = setTimeout(() => {
      runMemberSearch(createMemberSearch.trim());
    }, 350);

    return () => {
      if (createMemberSearchTimer.current) {
        clearTimeout(createMemberSearchTimer.current);
      }
    };
  }, [createMemberSearch, showCreate, runMemberSearch, createMemberResults.length, createMemberLoading, createMemberSelections.length]);

  useEffect(() => {
    if (!showDirect) {
      if (directSearchTerm !== '') {
        setDirectSearchTerm('');
      }
      if (directSelection) {
        setDirectSelection(null);
      }
      if (directSearchResults.length) {
        setDirectSearchResults([]);
      }
      if (directSearchLoading) {
        setDirectSearchLoading(false);
      }
      if (directSearchTimer.current) {
        clearTimeout(directSearchTimer.current);
      }
      return;
    }
    if (directSelection) {
      setDirectSearchResults([]);
      setDirectSearchLoading(false);
      if (directSearchTimer.current) {
        clearTimeout(directSearchTimer.current);
      }
      return;
    }
    if (!directSearchTerm.trim() || directSearchTerm.trim().length < 2) {
      setDirectSearchResults([]);
      setDirectSearchLoading(false);
      if (directSearchTimer.current) {
        clearTimeout(directSearchTimer.current);
      }
      return;
    }
    setDirectSearchLoading(true);
    if (directSearchTimer.current) {
      clearTimeout(directSearchTimer.current);
    }
    directSearchTimer.current = setTimeout(() => {
      runDirectSearch(directSearchTerm.trim());
    }, 350);

    return () => {
      if (directSearchTimer.current) {
        clearTimeout(directSearchTimer.current);
      }
    };
  }, [directSearchTerm, showDirect, directSelection, runDirectSearch, directSearchResults.length, directSearchLoading]);

  const ensureTreesLoaded = useCallback(async () => {
    if (loadingTrees || availableTrees.length) return;
    try {
      const userStr = localStorage.getItem('user');
      if (!userStr) return;
      const parsed = JSON.parse(userStr);
      if (!parsed?.id) return;
      setLoadingTrees(true);
      const list = await treeApi.listTrees(parsed.id);
      setAvailableTrees(list);
    } catch (err) {
      console.error('Failed to load trees', err);
      showToast.error('Không thể tải danh sách cây gia phả');
    } finally {
      setLoadingTrees(false);
    }
  }, [availableTrees.length, loadingTrees]);

  const loadTreeMembers = useCallback(
    async (treeId: string) => {
      if (!currentUserId || !treeId || treeMembers[treeId]) return;
      try {
        const members = await treeApi.listMembers(currentUserId, treeId);
        setTreeMembers((prev) => ({
          ...prev,
          [treeId]: members,
        }));
      } catch (err) {
        console.error('Failed to load tree members', err);
      }
    },
    [currentUserId, treeMembers],
  );

  useEffect(() => {
    if (showCreate && !availableTrees.length && !loadingTrees) {
      ensureTreesLoaded();
    }
  }, [showCreate, availableTrees.length, loadingTrees, ensureTreesLoaded]);

  const handleSendText = useCallback(() => {
    if (!currentRoom || !messageInput.trim()) return;
    setSending(true);
    sendMessage({
      roomId: currentRoom.id,
      messageText: messageInput.trim(),
      messageType: 'text',
      replyToId: replyingTo?.id,
    });
    setMessageInput('');
    setReplyingTo(null);
    setTimeout(() => setSending(false), 150);
  }, [currentRoom, messageInput, sendMessage, replyingTo]);

  const handleUploadAttachment = async (event: React.ChangeEvent<HTMLInputElement>) => {
    if (!currentRoom) return;
    const file = event.target.files?.[0];
    if (!file) return;

    const fileId = `${Date.now()}-${file.name}`;
    setUploadingFiles(prev => new Set(prev).add(fileId));

    try {
      await sendAttachment(currentRoom.id, file);
      showToast.success('Đã gửi tệp!');
    } catch (err) {
      showToast.error('Gửi tệp thất bại');
    } finally {
      setUploadingFiles(prev => {
        const next = new Set(prev);
        next.delete(fileId);
        return next;
      });
      event.target.value = '';
    }
  };

  const handleEditMessage = useCallback(
    async (messageId: string) => {
      if (!editingText.trim() || !currentRoom) return;
      const updated = await updateMessage(currentRoom.id, messageId, { messageText: editingText.trim() });
      if (updated) {
        setEditingMessageId(null);
        setEditingText('');
      }
    },
    [editingText, currentRoom, updateMessage],
  );

  const handleDeleteMessage = useCallback(
    async (messageId: string, isAdmin = false) => {
      if (!currentRoom) return;
      await deleteMessage(currentRoom.id, messageId, isAdmin);
    },
    [currentRoom, deleteMessage],
  );

  const handleReplyMessage = useCallback((message: ChatMessage) => {
    setReplyingTo(message);
    setMessageInput(`@${message.senderName} `);
  }, []);

  const addMemberSelection = useCallback(
    (user: UserSearchResult) => {
      if (createMemberSelections.some((item) => item.id === user.id)) {
        return;
      }
      setCreateMemberSelections((prev) => [...prev, user]);
      setCreateMemberSearch('');
      setCreateMemberResults([]);
    },
    [createMemberSelections],
  );

  const removeMemberSelection = useCallback((userId: string) => {
    setCreateMemberSelections((prev) => prev.filter((user) => user.id !== userId));
  }, []);

  const handleSelectDirectUser = useCallback(
    (user: UserSearchResult) => {
      setDirectSelection(user);
      setDirectSearchTerm(getUserLabel(user));
      setDirectSearchResults([]);
    },
    [getUserLabel],
  );

  const handleCreateRoom = async () => {
    const trimmedName = createState.name.trim();
    const trimmedDescription = createState.description.trim();

    if (!trimmedName) {
      showToast.warning('Vui lòng nhập tên phòng');
      return;
    }

    try {
      let room: ChatRoom | null = null;

      if (createState.roomType === 'branch') {
        if (!createState.familyTreeId || !createState.branchPersonId) {
          showToast.warning('Vui lòng chọn đầy đủ thông tin nhánh');
          return;
        }

        room = await createBranchRoom({
          branchPersonId: createState.branchPersonId,
          name: trimmedName,
          description: trimmedDescription || undefined,
        });
      }
      else {
        const memberIds = createMemberSelections.map(m => m.id);

        const payload: ChatRoomCreatePayload = {
          roomType: createState.roomType,
          name: trimmedName,
          description: trimmedDescription || undefined,
          familyTreeId: undefined,
          memberUserIds: memberIds.length > 0 ? memberIds : undefined,
        };

        room = await createRoom(payload);
      }

      if (room) {
        selectRoom(room.id);
        setShowCreate(false);
        setCreateState(defaultCreateState);
        setCreateMemberSelections([]);
        setCreateMemberSearch('');
        setCreateMemberResults([]);
        showToast.success('Đã tạo phòng chat thành công');
      }
    } catch (err) {
      showToast.error('Tạo phòng thất bại');
    }
  };

  const handleCreateDirect = async () => {
    const targetId = directSelection?.id;
    if (!targetId) {
      showToast.warning('Chọn người bạn muốn nhắn tin');
      return;
    }
    const room = await createDirectRoom({
      targetUserId: targetId,
      name: directRoomName.trim() || undefined,
    });
    if (room) {
      selectRoom(room.id);
      setShowDirect(false);
      setDirectSelection(null);
      setDirectSearchTerm('');
      setDirectRoomName('');
      setDirectSearchResults([]);
    }
  };

  const handleRoomClick = (roomId: string) => {
    selectRoom(roomId);
    setViewMode('chat');
    setIsCollapsed(false);
  };

  if (!isWidgetOpen) {
    return null;
  }

  const renderRoomMenu = () => {
    if (!currentRoom || !showRoomMenu) return null;

    const isFamily = currentRoom.roomType === 'family';
    const isBranch = currentRoom.roomType === 'branch';
    const isPrivate = currentRoom.roomType === 'private_chat';
    const isGroup = currentRoom.roomType === 'group';

    return (
        <>
          <div
              className="fixed inset-0 z-40"
              onClick={() => setShowRoomMenu(false)}
          />

          <div
              className="absolute right-4 top-16 w-64 bg-slate-950/95 border border-white/10 rounded-2xl shadow-2xl z-50 py-3">
            {/* Private chat: Đặt biệt danh */}
            {isPrivate && (
                <button
                    type="button"
                    onClick={(e) => {
                      e.preventDefault();
                      e.stopPropagation();
                      setNicknameForPrivateRoom();
                      setShowRoomMenu(false);
                    }}
                    className="w-full text-left px-4 py-2.5 text-sm hover:bg-white/10 transition flex items-center gap-3"
                >
                  <Edit size={16} /> Đặt biệt danh
                </button>
            )}

            {/* Branch: Đổi tên */}
            {(isBranch || isGroup) && canEditRoom(currentRoom) && (
                <button
                    type="button"
                    onClick={(e) => {
                      e.preventDefault();
                      e.stopPropagation();
                      setTempBranchName(currentRoom.name);
                      setShowEditBranchNameModal(true);
                      setShowRoomMenu(false);
                    }}
                    className="w-full text-left px-4 py-2.5 text-sm hover:bg-white/10 transition flex items-center gap-3"
                >
                  <Edit size={16} /> Đổi tên phòng
                </button>
            )}

            {/* Tất cả phòng: Mute/Unmute */}
            <button
                onClick={() => {
                  toggleMuteRoom(currentRoom.id);
                  setShowRoomMenu(false);
                }}
                className="w-full text-left px-4 py-2.5 text-sm hover:bg-white/10 transition flex items-center gap-3"
            >
              {isRoomMuted(currentRoom.id) ? (
                  <>
                    <Bell size={16} /> Bật thông báo
                  </>
              ) : (
                  <>
                    <BellOff size={16} /> Tắt thông báo
                  </>
              )}
            </button>

            <div className="border-t border-white/10 my-1" />

            {/* Branch: Rời phòng */}
            {(isBranch || isGroup) && canLeaveRoom(currentRoom) && (
                <button
                    onClick={() => {
                      if (confirm('Rời khỏi phòng chat này?')) {
                        leaveRoom(currentRoom.id);
                      }
                      setShowRoomMenu(false);
                    }}
                    className="w-full text-left px-4 py-2.5 text-sm hover:bg-white/10 transition flex items-center gap-3"
                >
                  <X size={16} /> Rời phòng
                </button>
            )}

            {/* Private: Xóa hội thoại */}
            {isPrivate && (
                <button
                    onClick={() => {
                      if (confirm('Xóa hội thoại này khỏi thiết bị của bạn?')) {
                        deleteConversationLocally(currentRoom.id);
                      }
                      setShowRoomMenu(false);
                    }}
                    className="w-full text-left px-4 py-2.5 text-sm hover:bg-white/10 transition flex items-center gap-3 text-orange-400"
                >
                  <Trash size={16} /> Xóa hội thoại (chỉ bạn)
                </button>
            )}

            {/* Branch: Xóa phòng */}
            {(isBranch || isGroup) && canDeleteRoom(currentRoom) && (
                <>
                  <div className="border-t border-white/10 my-1" />
                  <button
                      onClick={() => {
                        if (confirm('XÓA HOÀN TOÀN phòng này? Tất cả tin nhắn sẽ mất!')) {
                          deleteRoom(currentRoom.id);
                        }
                        setShowRoomMenu(false);
                      }}
                      className="w-full text-left px-4 py-2.5 text-sm hover:bg-red-500/20 transition flex items-center gap-3 text-red-400"
                  >
                    <Trash size={16} /> Xóa phòng (toàn bộ)
                  </button>
                </>
            )}

            {/* Family: chỉ mute */}
            {isFamily && (
                <div className="px-4 py-2.5 text-sm text-white/40">
                  Phòng chung gia phả – chỉ có thể tắt thông báo
                </div>
            )}
          </div>
        </>
    );
  };

  const renderMessageMenu = (message: ChatMessage, isMine: boolean) => {
    const isPinned = showMessageMenu === message.id;

    return (
        <div
            className={`absolute top-1/2 -translate-y-1/2 p-2 flex gap-1 z-20 transition-all duration-200 
        ${isMine ? "right-full" : "left-full"}
        ${isPinned
                ? "opacity-100 pointer-events-auto" 
                : "opacity-0 group-hover:opacity-100 pointer-events-none group-hover:pointer-events-auto" 
            }
      `}
            onClick={(e) => e.stopPropagation()}
        >
          <button
              onClick={(e) => {
                e.stopPropagation();
                handleReplyMessage(message);
              }}
              title="Trả lời"
              className="p-1 transition-all"
          >
            <Reply size={14} className="text-white" />
          </button>

          {canEditMessage(message) && (
              <button
                  onClick={(e) => {
                    e.stopPropagation();
                    setEditingMessageId(message.id)
                    setEditingText(message.messageText || "")
                    setShowMessageMenu(null)
                  }}
                  title="Sửa"
                  className="p-1 transition-all"
              >
                <Edit size={14} className="text-white" />
              </button>
          )}

          {currentRoom && canDeleteMessage(message, currentRoom) && (
              <button
                  onClick={(e) => {
                    e.stopPropagation();
                    handleDeleteMessage(message.id, true)
                    setShowMessageMenu(null)
                  }}
                  title="Xóa"
                  className="p-1 transition-all"
              >
                <Trash size={14} className="text-red-400 hover:text-red-300" />
              </button>
          )}
        </div>
    )
  };

  const renderCollapsedDock = () => (
    <div className="fixed bottom-4 right-4 z-[1200] flex flex-col items-center gap-2">
      {currentRoom && (
        <button
          onClick={() => {
            setIsCollapsed(false);
            setViewMode('chat');
          }}
          className="w-12 h-12 rounded-full bg-gradient-to-b from-[#ffd89b] to-[#f9a826] shadow-lg flex items-center justify-center text-[#1e2a3a] font-semibold hover:scale-105 transition"
          title={currentRoom.name}
        >
          {currentRoom.name.slice(0, 1).toUpperCase()}
        </button>
      )}
      <button
        onClick={() => {
          setIsCollapsed(false);
          setViewMode('list');
        }}
        className="w-12 h-12 rounded-full bg-[#1e2a3a] shadow-xl flex items-center justify-center text-white text-lg hover:bg-[#182230] transition"
        title="Danh sách chat"
      >
        <Plus size={20} />
      </button>
      <button
        onClick={closeWidget}
        className="w-12 h-12 rounded-full bg-gray-900/80 border border-white/20 text-white flex items-center justify-center text-sm hover:bg-gray-800 transition"
        title="Đóng chat"
      >
        <X size={18} />
      </button>
    </div>
  );

  const renderListView = () => (
    <div className="fixed top-20 right-6 z-[1200] w-full max-w-sm">
      <div className="bg-[#1e2a3a] text-white rounded-2xl shadow-2xl border border-white/10 overflow-hidden">
        <header className="px-4 py-2.5 border-b border-white/10 flex items-center justify-between relative">
          <p className="text-lg font-semibold">Đoạn chat</p>
          <div className="flex items-center gap-2">
            <div className="relative">
              <button
                onClick={() => setShowQuickActions((prev) => !prev)}
                className="p-2 rounded-full hover:bg-white/10 transition"
                title="Tạo nhanh"
              >
                <Plus size={18} />
              </button>
              {showQuickActions && (
                <>
                  <div
                    className="fixed inset-0 z-10"
                    onClick={() => setShowQuickActions(false)}
                    onKeyDown={(e) => {
                      if (e.key === 'Escape') setShowQuickActions(false);
                    }}
                    role="button"
                    tabIndex={0}
                  />

                  <div
                    className="absolute top-full right-0 mt-2 w-56 bg-[#0f172a] border border-white/10 rounded-2xl shadow-2xl py-3 z-50 overflow-hidden">
                    <button
                      onClick={() => {
                        setShowQuickActions(false);
                        setShowCreate(true);
                        ensureTreesLoaded();
                      }}
                      className="w-full text-left px-4 py-2.5 text-sm hover:bg-white/10 transition flex items-center gap-3"
                    >
                      <Users size={16} />
                      Tạo phòng mới
                    </button>
                    <div className="border-t border-white/10 my-1" />
                    <button
                      onClick={() => {
                        setShowQuickActions(false);
                        setShowDirect(true);
                      }}
                      className="w-full text-left px-4 py-2.5 text-sm hover:bg-white/10 transition flex items-center gap-3"
                    >
                      <Send size={16} />
                      Nhắn riêng
                    </button>
                  </div>
                </>
              )}
            </div>
            <button
              onClick={() => setIsCollapsed(true)}
              className="p-2 rounded-full hover:bg-white/10 transition"
              title="Thu nhỏ"
            >
              <ChevronRight size={18} />
            </button>
            <button
              onClick={closeWidget}
              className="p-2 rounded-full hover:bg-white/10 transition"
              title="Đóng"
            >
              <X size={18} />
            </button>
          </div>
        </header>
        <div className="p-4 border-b border-white/10 space-y-3">
          <div className="flex items-center gap-2">
            <div className="flex-1">
              <input
                value={search}
                onChange={(e) => setSearch(e.target.value)}
                placeholder="Tìm kiếm trên Messenger"
                className="w-full bg-white/5 border border-white/10 rounded-xl px-4 py-2 text-sm focus:outline-none"
              />
            </div>
          </div>
          <div className="flex items-center gap-2 flex-wrap">
            {(showAllFilters ? filterTabs : filterTabs.slice(0, 4)).map((tab) => (
              <button
                key={tab.key}
                onClick={() => setFilter(tab.key)}
                className={`px-3 py-1.5 rounded-lg text-sm transition ${filter === tab.key ? 'bg-[#ffd89b] text-[#1e2a3a] font-semibold' : 'bg-white/5 hover:bg-white/10'
                  }`}
              >
                {tab.label}
              </button>
            ))}
            {filterTabs.length > 4 && (
              <button
                onClick={() => setShowAllFilters((prev) => !prev)}
                className="px-3 py-1.5 rounded-lg text-sm bg-white/5 hover:bg-white/10"
              >
                {showAllFilters ? 'Thu gọn' : '...'}
              </button>
            )}
          </div>
        </div>
        <div className="max-h-96 overflow-y-auto divide-y divide-white/5">
          {filteredRooms.length === 0 ? (
            <div className="py-16 text-center space-y-2 text-white/70">
              <Inbox className="mx-auto text-white/40" size={36} />
              <p className="text-sm">Chưa có cuộc trò chuyện nào</p>
            </div>
          ) : (
            filteredRooms.map((room) => (
              <button
                key={room.id}
                onClick={() => handleRoomClick(room.id)}
                className={`w-full text-left px-4 py-3 transition ${room.id === currentRoom?.id
                  ? 'bg-white/10'
                  : unreadByRoom[room.id]
                    ? 'bg-white/5 border-l-2 border-amber-300'
                    : 'hover:bg-white/5'
                  }`}
              >
                <div className="flex items-center justify-between">
                  <p className="font-semibold truncate">
                    {room.roomType === 'private_chat'
                      ? getDisplayName(room)
                      : room.name
                    }
                  </p>
                  {unreadByRoom[room.id] ? (
                    <span className="text-xs bg-red-500/80 rounded-full px-2 py-0.5">
                      {unreadByRoom[room.id]}
                    </span>
                  ) : (
                    <span className="text-[11px] text-white/50">
                      {ROOM_TYPE_LABEL_MAP[room.roomType]}
                    </span>
                  )}
                </div>
                <p className="text-xs text-white/60 line-clamp-1">
                  {room.description || (room.roomType === 'private_chat' ? 'Phòng chat riêng tư 1-1' : 'Không có mô tả')}
                </p>
              </button>
            ))
          )}
        </div>
      </div>
    </div>
  );

  const renderChatView = () => {
    const memberCount = currentRoom?.members?.length ?? 0;

    return (
      <div className="fixed bottom-4 right-4 z-[1300] w-full max-w-sm">
        <div className="bg-gradient-to-br from-[#1e2a3a] via-[#253446] to-[#1a2633] text-white rounded-3xl shadow-2xl border-2 border-[#ffd89b]/30 overflow-hidden relative backdrop-blur-sm">
          {/* Header */}
          <header className="flex items-center justify-between px-3 py-2.5 border-b border-white/10">
            <div className="flex items-center gap-2">
              <button
                onClick={() => setViewMode('list')}
                className="p-2 rounded-xl hover:bg-[#ffd89b]/10 transition-all duration-200 hover:scale-105"
                title="Danh sách"
              >
                <ChevronDown size={18} className="transform rotate-90" />
              </button>
              <div>
                <p className="font-bold text-base text-[#ffd89b]">{getDisplayName(currentRoom)}</p>
                {currentRoom && (
                  <button
                    onClick={() => setShowMembersList((prev) => !prev)}
                    className="text-xs text-white hover:text-[#ffd89b] transition underline-offset-2 hover:underline flex items-center gap-1"
                  >
                    {memberCount} thành viên
                  </button>
                )}
              </div>
            </div>
            <div className="flex items-center gap-1">
              <button
                onClick={() => setShowRoomMenu((prev) => !prev)}
                className="p-2 rounded-xl hover:bg-[#ffd89b]/10 transition-all duration-200 hover:scale-105"
                title="Cài đặt phòng"
              >
                <Settings size={18} />
              </button>
              <button
                onClick={() => setIsCollapsed(true)}
                className="p-2 rounded-xl hover:bg-[#ffd89b]/10 transition-all duration-200 hover:scale-105"
                title="Thu nhỏ"
              >
                <ChevronRight size={18} />
              </button>
              <button
                onClick={closeWidget}
                className="p-2 rounded-xl hover:bg-[#ffd89b]/10 transition-all duration-200 hover:scale-105"
                title="Đóng"
              >
                <X size={18} />
              </button>
            </div>
          </header>

          {/* Room Menu */}
          {renderRoomMenu()}

          {/* Members List */}
          {showMembersList && currentRoom && (
            <div
              ref={membersListRef}
              className="absolute right-4 top-16 w-72 bg-slate-950/95 border border-white/10 rounded-2xl shadow-2xl z-50"
            >
              <div className="px-4 py-3 border-b border-white/10">
                <p className="text-sm font-semibold">Thành viên ({memberCount})</p>
              </div>
              <div className="max-h-64 overflow-y-auto divide-y divide-white/5">
                {currentRoom.members.map((member) => (
                  <div key={member.userId} className="px-4 py-2 text-sm flex items-center justify-between">
                    <div>
                      <p className="font-medium">{member.username}</p>
                    </div>
                    <span
                      className={`text-[10px] uppercase tracking-wider px-2 py-0.5 rounded ${member.role === 'admin'
                        ? 'bg-[#ffd89b]/20 text-[#ffd89b]'
                        : member.role === 'moderator'
                          ? 'bg-cyan-500/20 text-cyan-300'
                          : 'bg-white/10 text-white/70'
                        }`}
                    >
                      {member.role === 'admin' ? 'Admin' : member.role === 'moderator' ? 'Điều phối' : 'Thành viên'}
                    </span>
                  </div>
                ))}
              </div>
            </div>
          )}

          {/* Chat Area */}
          {currentRoom ? (
            <>
              {/* Messages */}
              <div className="h-[350px] overflow-y-auto overflow-x-visible px-2.5 py-2 space-y-1.5 bg-gradient-to-b from-[#1e2a3a]/30 to-transparent"
                   onClick={() => setShowMessageMenu(null)}
              >
                {/* Load more */}
                {currentRoomState?.hasMore && (
                  <button
                    onClick={() => loadOlderMessages(currentRoom.id)}
                    className="text-xs text-[#ffd89b]/80 mx-auto block hover:text-[#ffd89b] transition"
                    disabled={currentRoomState?.isLoading}
                  >
                    {currentRoomState?.isLoading ? 'Đang tải...' : 'Xem tin nhắn cũ hơn'}
                  </button>
                )}

                {/* Messages List */}
                {currentMessages.map((message) => {
                  const isMine = message.senderId === currentUserId;
                  const isSystem = message.messageType === 'system';

                  if (isSystem) {
                    return (
                      <div key={message.id} className="text-center py-2">
                        <p className="text-xs text-white/60 italic">{message.messageText}</p>
                      </div>
                    );
                  }

                  return (
                    <div key={message.id} className={`flex ${isMine ? 'justify-end' : 'justify-start'} group`}>
                      <div className="relative max-w-[70%] overflow-visible cursor-pointer"
                           onClick={(e) => {
                             e.stopPropagation();
                             setShowMessageMenu(showMessageMenu === message.id ? null : message.id);
                           }}
                      >
                        <div
                          className={`rounded-2xl px-3 py-0.5 text-sm shadow-lg transition-all duration-200 hover:shadow-xl inline-block ${isMine
                            ? 'bg-gradient-to-br from-[#ffd89b] via-[#ffcd7a] to-[#ffc165] text-[#1e2a3a] border-2 border-[#ffd89b]/40'
                            : 'bg-gradient-to-br from-[#2a3a4a] to-[#1e2a3a] text-white border-2 border-white/10 backdrop-blur-sm'
                            }`}
                        >
                          {/* Reply Preview */}
                          {message.replyToId && (
                            <div className={`text-xs opacity-80 border-l-2 pl-2 mb-1 ${isMine ? 'border-[#1e2a3a]/40' : 'border-[#ffd89b]/40'}`}>
                              <span className={isMine ? 'text-[#1e2a3a]/70' : 'text-[#ffd89b]/70'}>
                                Trả lời @{message.replyToSenderName}:
                              </span>{' '}
                              {message.replyToText?.slice(0, 50)}...
                            </div>
                          )}

                          {/* Edit Mode */}
                          {editingMessageId === message.id ? (
                            <input
                              value={editingText}
                              onChange={(e) => setEditingText(e.target.value)}
                              onKeyDown={(e) => {
                                if (e.key === 'Enter' && !e.shiftKey) {
                                  e.preventDefault();
                                  handleEditMessage(message.id);
                                }
                                if (e.key === 'Escape') {
                                  setEditingMessageId(null);
                                  setEditingText('');
                                }
                              }}
                              className={`w-full bg-transparent outline-none ${isMine ? 'text-[#1e2a3a] placeholder-[#1e2a3a]/50' : 'text-white placeholder-white/50'}`}
                              autoFocus
                            />
                          ) : (
                            <>
                              <p className={`text-[11px] m-0 font-semibold ${isMine ? 'text-[#1e2a3a]/80' : 'text-[#ffd89b]/90'}`}>
                                {isMine ? 'Bạn' : message.senderName}
                              </p>
                              {renderMessageContent(message)}
                              {message.edited && (
                                <span className="text-[10px] opacity-60 ml-1 italic">(đã chỉnh sửa)</span>
                              )}
                            </>
                          )}

                          {/* Timestamp */}
                          <p className={`text-[10px] m-0 ${isMine ? 'text-[#1e2a3a]/60' : 'text-white/60'} whitespace-nowrap`}>
                            {new Date(message.createdAt).toLocaleString('vi-VN', {
                              hour: '2-digit',
                              minute: '2-digit',
                              day: 'numeric',
                              month: 'long',
                              year: 'numeric'
                            }).replace(',', '')}
                          </p>
                        </div>
                        {renderMessageMenu(message, isMine)}
                      </div>
                    </div>
                  );
                })}
                <div ref={messageEndRef} />
              </div>

              {/* Reply Preview */}
              {replyingTo && (
                <div className="mx-5 mb-3 p-3 bg-[#ffd89b]/10 rounded-xl flex items-center gap-3 border-l-4 border-[#ffd89b]">
                  <Reply size={16} className="text-[#ffd89b]" />
                  <div className="flex-1 text-sm">
                    <p className="text-[#ffd89b]/90 font-medium">Trả lời @{replyingTo.senderName}</p>
                    <p className="text-white/90 truncate text-xs">
                      {replyingTo.deleted ? 'Tin nhắn đã bị xóa' : replyingTo.messageText}
                    </p>
                  </div>
                  <button onClick={() => setReplyingTo(null)} className="text-white/60 hover:text-[#ffd89b] transition">
                    <X size={16} />
                  </button>
                </div>
              )}

              {/* Input Area */}
              <div className="px-4 py-3 border-t-2 border-[#ffd89b]/20 bg-gradient-to-r from-[#1e2a3a] to-[#2a3a4a]">
                <div className="flex items-end gap-2">
                  <button
                    onClick={() => fileInputRef.current?.click()}
                    disabled={uploadingFiles.size > 0}
                    className="relative p-2.5 rounded-xl bg-[#ffd89b]/10 hover:bg-[#ffd89b]/20 disabled:opacity-50 disabled:cursor-not-allowed transition-all duration-200 group"
                    title={uploadingFiles.size > 0 ? "Đang tải lên..." : "Gửi ảnh, file, video..."}
                  >
                    {uploadingFiles.size > 0 ? (
                      <Loader2 className="animate-spin text-[#ffd89b]" size={20} />
                    ) : (
                      <Paperclip size={20} className="text-white/80 group-hover:text-[#ffd89b] transition" />
                    )}
                  </button>

                  <textarea
                    value={messageInput}
                    onChange={(e) => setMessageInput(e.target.value)}
                    onKeyDown={(e) => {
                      if (e.key === 'Enter' && !e.shiftKey) {
                        e.preventDefault();
                        handleSendText();
                      }
                    }}
                    rows={Math.min(messageInput.split('\n').length || 1, 5)}
                    className="flex-1 bg-white/5 border-2 border-[#ffd89b]/20 rounded-xl px-3 py-2 text-sm resize-none focus:outline-none focus:border-[#ffd89b]/50 placeholder-white/40"
                    placeholder="Nhập tin nhắn..."
                  />

                  <button
                    onClick={handleSendText}
                    disabled={!messageInput.trim() || sending}
                    className="p-2.5 rounded-xl bg-gradient-to-r from-[#ffd89b] to-[#ffcd7a] text-[#1e2a3a] disabled:opacity-40 hover:scale-110 transition-all shadow-lg hover:shadow-[#ffd89b]/50 font-semibold"
                    title="Gửi"
                  >
                    <Send size={18} />
                  </button>
                </div>

                <input
                  ref={fileInputRef}
                  type="file"
                  className="hidden"
                  onChange={handleUploadAttachment}
                  multiple
                />
              </div>
            </>
          ) : (
            <div className="h-[420px] flex items-center justify-center text-center px-6">
              <div>
                <Users size={36} className="mx-auto text-[#ffd89b]/60 mb-3" />
                <p className="font-semibold text-[#ffd89b]">Chọn một phòng chat</p>
                <p className="text-sm text-white/60 mt-1">
                  Hãy quay lại danh sách để bắt đầu trò chuyện.
                </p>
              </div>
            </div>
          )}
        </div>
      </div>
    );
  };

  const renderActivePanel = () => {
    if (isCollapsed) return renderCollapsedDock();
    if (viewMode === 'list') return renderListView();
    return renderChatView();
  };

  return (
    <>
      {renderActivePanel()}
      {showCreate && (
        <div className="fixed inset-0 z-[1300] bg-black/50 flex items-center justify-center px-4">
          <div className="bg-[#1e2a3a] text-white rounded-2xl w-full max-w-lg shadow-2xl border border-[#ffd89b]/20 p-6 space-y-4">
            <div className="flex items-center justify-between">
              <p className="font-semibold text-[#ffd89b]">Tạo phòng chat</p>
              <button onClick={() => setShowCreate(false)} className="text-[#ffd89b]/70 hover:text-[#ffd89b]">
                <X size={20} />
              </button>
            </div>
            <div className="space-y-3">
              <label className="text-sm space-y-1 block">
                <span className="text-[#ffd89b]">Loại phòng</span>
                <select
                  value={createState.roomType}
                  onChange={async (e) => {
                    const nextType = e.target.value as ChatRoomType;
                    setCreateState((prev) => ({
                      ...prev,
                      roomType: nextType,
                      familyTreeId: nextType === 'branch' ? prev.familyTreeId : '',
                      branchPersonId: nextType === 'branch' ? prev.branchPersonId : '',
                    }));
                    if (nextType === 'branch') {
                      await ensureTreesLoaded();
                    }
                  }}
                  className="w-full bg-[#1e2a3a]/80 border border-[#ffd89b]/20 rounded-xl px-3 py-2 text-sm text-white hover:border-[#ffd89b]/40 transition"
                >
                  <option value="branch">Theo nhánh (cùng cây gia phả)</option>
                  <option value="group">Nhóm chat tự do</option>
                </select>
              </label>
              <label className="text-sm space-y-1 block">
                <span className="text-[#ffd89b]">Tên phòng</span>
                <input
                  value={createState.name}
                  onChange={(e) => setCreateState((prev) => ({ ...prev, name: e.target.value }))}
                  className="w-full bg-[#1e2a3a]/80 border border-[#ffd89b]/20 rounded-xl px-3 py-2 text-sm text-white placeholder-white/40 hover:border-[#ffd89b]/40 focus:border-[#ffd89b]/60 transition"
                />
              </label>
              <label className="text-sm space-y-1 block">
                <span className="text-[#ffd89b]">Mô tả</span>
                <textarea
                  value={createState.description}
                  onChange={(e) => setCreateState((prev) => ({ ...prev, description: e.target.value }))}
                  className="w-full bg-[#1e2a3a]/80 border border-[#ffd89b]/20 rounded-xl px-3 py-2 text-sm text-white placeholder-white/40 hover:border-[#ffd89b]/40 focus:border-[#ffd89b]/60 transition"
                  rows={2}
                />
              </label>
              {createState.roomType !== 'group' && (
                <label className="text-sm space-y-1 block">
                  <span className="text-[#ffd89b]">Chọn cây gia phả</span>
                  <select
                    value={createState.familyTreeId}
                    onChange={async (e) => {
                      const treeId = e.target.value;
                      setCreateState((prev) => ({
                        ...prev,
                        familyTreeId: treeId,
                        branchPersonId: treeId ? '' : prev.branchPersonId,
                      }));
                      await loadTreeMembers(treeId);
                    }}
                    className="w-full bg-[#1e2a3a]/80 border border-[#ffd89b]/20 rounded-xl px-3 py-2 text-sm text-white hover:border-[#ffd89b]/40 transition"
                  >
                    <option value="">-- Chọn --</option>
                    {availableTrees.map((tree) => (
                      <option key={tree.id} value={tree.id}>
                        {tree.name}
                      </option>
                    ))}
                  </select>
                </label>
              )}
              {createState.roomType === 'branch' && createState.familyTreeId && (
                <label className="text-sm space-y-1 block">
                  <span className="text-[#ffd89b]">Chọn thành viên đại diện cho nhánh</span>
                  <select
                    value={createState.branchPersonId}
                    onChange={(e) =>
                      setCreateState((prev) => ({
                        ...prev,
                        branchPersonId: e.target.value,
                      }))
                    }
                    className="w-full bg-[#1e2a3a]/80 border border-[#ffd89b]/20 rounded-xl px-3 py-2 text-sm text-white hover:border-[#ffd89b]/40 transition"
                  >
                    <option value="">-- Chọn --</option>
                    {(treeMembers[createState.familyTreeId] ?? []).map((person) => (
                      <option key={person.id} value={person.id}>
                        {person.fullName}
                      </option>
                    ))}
                  </select>
                  <p className="text-xs text-[#ffd89b]/70">
                    Thành viên sẽ được tự động thêm theo nhánh này (bao gồm hậu duệ và vợ/chồng đã liên kết tài
                    khoản).
                  </p>
                </label>
              )}
              {createState.roomType !== 'branch' && (
                <div className="text-sm space-y-2">
                  <div className="space-y-1">
                    <span className="block text-[#ffd89b]">
                      {createState.roomType === 'group'
                        ? 'Thêm thành viên (tìm bằng email / số điện thoại / tên)'
                        : 'Tìm người nhận'}
                    </span>
                    <input
                      value={createMemberSearch}
                      onChange={(e) => setCreateMemberSearch(e.target.value)}
                      placeholder="Nhập ít nhất 2 ký tự để tìm..."
                      className="w-full bg-[#1e2a3a]/80 border border-[#ffd89b]/20 rounded-xl px-3 py-2 text-sm text-white placeholder-white/40 hover:border-[#ffd89b]/40 transition"
                    />
                  </div>
                  {createMemberLoading && <p className="text-xs text-white/60">Đang tìm kiếm...</p>}
                  {createMemberResults.length > 0 && (
                    <div className="max-h-40 overflow-y-auto border border-[#ffd89b]/20 rounded-xl divide-y divide-[#ffd89b]/10">
                      {createMemberResults.map((user) => (
                        <button
                          key={user.id}
                          type="button"
                          onClick={() => addMemberSelection(user)}
                          className="w-full text-left px-3 py-2 hover:bg-[#ffd89b]/10 transition"
                        >
                          <p className="font-medium">{getUserLabel(user)}</p>
                          <p className="text-xs text-white/70">{getUserSecondaryLabel(user)}</p>
                        </button>
                      ))}
                    </div>
                  )}
                  {createMemberSelections.length > 0 && (
                    <div className="flex flex-wrap gap-2">
                      {createMemberSelections.map((user) => (
                        <span
                          key={user.id}
                          className="inline-flex items-center gap-1 px-3 py-1 rounded-full bg-[#ffd89b]/20 text-xs text-[#ffd89b]"
                        >
                          {getUserLabel(user)}
                          <button
                            type="button"
                            onClick={() => removeMemberSelection(user.id)}
                            className="text-[#ffd89b]/60 hover:text-[#ffd89b]"
                          >
                            ×
                          </button>
                        </span>
                      ))}
                    </div>
                  )}
                </div>
              )}
            </div>
            <div className="flex justify-end gap-2 pt-2">
              <button onClick={() => setShowCreate(false)} className="px-4 py-2 rounded-xl bg-[#ffd89b]/10 hover:bg-[#ffd89b]/20 text-white/80 text-sm transition">
                Huỷ
              </button>
              <button
                onClick={handleCreateRoom}
                className="px-4 py-2 rounded-xl bg-[#ffd89b] hover:bg-[#ffd89b]/90 text-[#1e2a3a] text-sm font-semibold transition shadow-lg"
              >
                Tạo phòng
              </button>
            </div>
          </div>
        </div>
      )}

      {showDirect && (
        <div className="fixed inset-0 z-[1300] bg-black/50 flex items-center justify-center px-4">
          <div className="bg-[#1e2a3a] text-white rounded-2xl w-full max-w-md border border-[#ffd89b]/20 p-6 space-y-3 shadow-2xl">
            <div className="flex items-center justify-between">
              <p className="font-semibold text-[#ffd89b]">Chat riêng tư</p>
              <button onClick={() => setShowDirect(false)} className="text-[#ffd89b]/70 hover:text-[#ffd89b]">
                <X size={20} />
              </button>
            </div>
            <div className="text-sm space-y-2">
              <div className="space-y-1">
                <span className="block text-[#ffd89b]">Tìm người nhận (email / số điện thoại / tên)</span>
                <input
                  value={directSearchTerm}
                  onChange={(e) => {
                    setDirectSearchTerm(e.target.value);
                    if (directSelection) {
                      setDirectSelection(null);
                    }
                  }}
                  placeholder="Nhập ít nhất 2 ký tự để tìm..."
                  className="w-full bg-[#1e2a3a]/80 border border-[#ffd89b]/20 rounded-xl px-3 py-2 text-sm text-white placeholder-white/40 hover:border-[#ffd89b]/40 transition"
                />
              </div>
              {directSearchLoading && <p className="text-xs text-white/60">Đang tìm kiếm...</p>}
              {directSearchResults.length > 0 && !directSelection && (
                <div className="max-h-40 overflow-y-auto border border-[#ffd89b]/20 rounded-xl divide-y divide-[#ffd89b]/10">
                  {directSearchResults.map((user) => (
                    <button
                      key={user.id}
                      type="button"
                      onClick={() => handleSelectDirectUser(user)}
                      className="w-full text-left px-3 py-2 hover:bg-[#ffd89b]/10 transition"
                    >
                      <p className="font-medium">{getUserLabel(user)}</p>
                      <p className="text-xs text-white/70">{getUserSecondaryLabel(user)}</p>
                    </button>
                  ))}
                </div>
              )}
              {directSelection && (
                <div className="flex items-center justify-between bg-[#ffd89b]/10 border border-[#ffd89b]/20 rounded-xl px-3 py-2">
                  <div>
                    <p className="text-sm font-medium">{getUserLabel(directSelection)}</p>
                    <p className="text-xs text-white/70">{getUserSecondaryLabel(directSelection)}</p>
                  </div>
                  <button
                    type="button"
                    onClick={() => {
                      setDirectSelection(null);
                      setDirectSearchTerm('');
                    }}
                    className="text-xs text-[#ffd89b] hover:text-[#ffd89b]/80 transition"
                  >
                    Chọn lại
                  </button>
                </div>
              )}
            </div>
            <label className="text-sm space-y-1 block">
              <span className="text-[#ffd89b]">Tên phòng (tuỳ chọn)</span>
              <input
                value={directRoomName}
                onChange={(e) => setDirectRoomName(e.target.value)}
                className="w-full bg-[#1e2a3a]/80 border border-[#ffd89b]/20 rounded-xl px-3 py-2 text-sm text-white placeholder-white/40 hover:border-[#ffd89b]/40 transition"
              />
            </label>
            <div className="flex justify-end gap-2 pt-2">
              <button onClick={() => setShowDirect(false)} className="px-4 py-2 rounded-xl bg-[#ffd89b]/10 hover:bg-[#ffd89b]/20 text-white/80 text-sm transition">
                Huỷ
              </button>
              <button
                onClick={handleCreateDirect}
                className="px-4 py-2 rounded-xl bg-[#ffd89b] hover:bg-[#ffd89b]/90 text-[#1e2a3a] text-sm font-semibold transition shadow-lg"
              >
                Bắt đầu
              </button>
            </div>
          </div>
        </div>
      )}

      {showNicknameModal && currentRoom && currentRoom.roomType === 'private_chat' && (
        <div className="fixed inset-0 z-[1300] bg-black/50 flex items-center justify-center px-4">
          <div className="bg-[#1e2a3a] rounded-2xl border border-[#ffd89b]/20 p-6 w-full max-w-sm shadow-2xl">
            <div className="flex items-center justify-between mb-4">
              <h3 className="text-lg font-semibold text-[#ffd89b]">Đặt biệt danh</h3>
              <button onClick={() => setShowNicknameModal(false)} className="text-[#ffd89b]/70 hover:text-[#ffd89b]">
                <X size={20} />
              </button>
            </div>

            <div className="space-y-4">
              <div>
                <label className="text-sm text-white/70 block mb-2">
                  Biệt danh cho <span className="text-[#ffd89b] font-medium">{getDisplayName(currentRoom)}</span>
                </label>
                <input
                  type="text"
                  value={tempNickname}
                  onChange={(e) => setTempNickname(e.target.value)}
                  onKeyDown={(e) => e.key === 'Enter' && e.currentTarget.closest('button') === null && document.querySelector('button')?.click?.()}
                  placeholder="Nhập biệt danh..."
                  className="w-full bg-[#1e2a3a]/80 border border-[#ffd89b]/20 rounded-xl px-4 py-3 text-sm text-white placeholder-white/40 focus:outline-none focus:border-[#ffd89b]/60 hover:border-[#ffd89b]/40 transition"
                  autoFocus
                />
              </div>

              <div className="flex justify-end gap-3 pt-2">
                <button
                  onClick={() => {
                    setShowNicknameModal(false);
                    setTempNickname('');
                  }}
                  className="px-5 py-2.5 rounded-xl bg-[#ffd89b]/10 hover:bg-[#ffd89b]/20 transition text-sm text-white/80"
                >
                  Hủy
                </button>
                <button
                  onClick={async () => {
                    if (!tempNickname.trim()) {
                      showToast.warning('Vui lòng nhập biệt danh');
                      return;
                    }

                    try {
                      await chatApi.updateMyMembership(currentRoom.id, {
                        nickname: tempNickname.trim()
                      });

                      setRooms(prevRooms =>
                        prevRooms.map(room =>
                          room.id === currentRoom.id
                            ? {
                              ...room,
                              members: room.members.map(m =>
                                m.userId !== currentUserId
                                  ? { ...m, nickname: tempNickname.trim() }
                                  : m
                              )
                            }
                            : room
                        )
                      );
                      showToast.success(`Đã đổi biệt danh thành: ${tempNickname.trim()}`);
                      setShowNicknameModal(false);
                      setTempNickname('');
                    } catch (err) {
                      showToast.error('Đổi biệt danh thất bại');
                    }
                  }}
                  className="px-5 py-2.5 rounded-xl bg-[#ffd89b] hover:bg-[#ffd89b]/90 text-[#1e2a3a] font-medium text-sm transition shadow-lg"
                >
                  Lưu
                </button>
              </div>
            </div>
          </div>
        </div>
      )}

      {showEditBranchNameModal && currentRoom && (
        <div className="fixed inset-0 z-[1300] bg-black/50 flex items-center justify-center px-4">
          <div className="bg-[#1e2a3a] rounded-2xl border border-[#ffd89b]/20 p-6 w-full max-w-sm shadow-2xl">
            <div className="flex items-center justify-between mb-4">
              <h3 className="text-lg font-semibold text-[#ffd89b]">
                {currentRoom.roomType === 'group' ? 'Đổi tên nhóm' : 'Đổi tên phòng nhánh'}
              </h3>
              <button
                onClick={() => {
                  setShowEditBranchNameModal(false);
                  setTempBranchName('');
                }}
                className="text-[#ffd89b]/70 hover:text-[#ffd89b] transition"
              >
                <X size={20} />
              </button>
            </div>

            <div className="space-y-4">
              <div>
                <label className="text-sm text-white/70 block mb-2">
                  Tên mới cho phòng <span className="text-[#ffd89b] font-medium">"{currentRoom.name}"</span>
                </label>
                <input
                  type="text"
                  value={tempBranchName}
                  onChange={(e) => setTempBranchName(e.target.value)}
                  onKeyDown={(e) => {
                    if (e.key === 'Enter') {
                      e.preventDefault();
                      document.getElementById('save-branch-name-btn')?.click();
                    }
                  }}
                  placeholder="Nhập tên phòng mới..."
                  className="w-full bg-[#1e2a3a]/80 border border-[#ffd89b]/20 rounded-xl px-4 py-3 text-sm text-white placeholder-white/40 focus:outline-none focus:border-[#ffd89b]/60 hover:border-[#ffd89b]/40 transition"
                  autoFocus
                />
              </div>

              <div className="flex justify-end gap-3 pt-2">
                <button
                  onClick={() => {
                    setShowEditBranchNameModal(false);
                    setTempBranchName('');
                  }}
                  className="px-5 py-2.5 rounded-xl bg-[#ffd89b]/10 hover:bg-[#ffd89b]/20 transition text-sm text-white/80"
                >
                  Hủy
                </button>
                <button
                  id="save-branch-name-btn"
                  onClick={async () => {
                    const newName = tempBranchName.trim();
                    if (!newName) {
                      showToast.warning('Vui lòng nhập tên phòng');
                      return;
                    }
                    if (newName === currentRoom.name) {
                      setShowEditBranchNameModal(false);
                      return;
                    }

                    try {
                      await updateRoom(currentRoom.id, { name: newName });
                      showToast.success('Đã đổi tên phòng thành công');
                      setShowEditBranchNameModal(false);
                      setTempBranchName('');
                    } catch (err) {
                      showToast.error('Đổi tên thất bại');
                    }
                  }}
                  className="px-5 py-2.5 rounded-xl bg-[#ffd89b] hover:bg-[#ffd89b]/90 text-[#1e2a3a] font-medium text-sm transition shadow-lg"
                >
                  Lưu
                </button>
              </div>
            </div>
          </div>
        </div>
      )}
    </>
  );
};

export default ChatWidget;