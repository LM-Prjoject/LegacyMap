import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import { Paperclip, Plus, Send, X, ChevronDown, ChevronRight, Loader2, Users, Inbox } from 'lucide-react';
import { useChat } from '@/contexts/ChatContext';
import type { ChatRoomType, UserSearchResult } from '@/types/chat';
import treeApi, { FamilyTree, Person } from '@/api/trees';
import { showToast } from '@/lib/toast';
import { userLookupApi } from '@/api/chatApi';

type RoomFilter = 'all' | ChatRoomType;
type ViewMode = 'list' | 'chat';

const ROOM_TYPE_LABEL_MAP: Record<ChatRoomType, string> = {
  family: 'Gia phả',
  branch: 'Nhánh',
  private_chat: 'Riêng tư',
};

interface CreateRoomFormState {
  roomType: ChatRoomType;
  name: string;
  description: string;
  familyTreeId: string;
  branchPersonId: string;
}

const defaultCreateState: CreateRoomFormState = {
  roomType: 'family',
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
    joinRoom,
    createDirectRoom,
    widgetSignal,
  } = useChat();
  const [isCollapsed, setIsCollapsed] = useState(false);
  const [viewMode, setViewMode] = useState<ViewMode>('list');
  const [filter, setFilter] = useState<RoomFilter>('all');
  const [search, setSearch] = useState('');
  const [messageInput, setMessageInput] = useState('');
  const [isUploading, setIsUploading] = useState(false);
  const [showCreate, setShowCreate] = useState(false);
  const [showJoin, setShowJoin] = useState(false);
  const [showDirect, setShowDirect] = useState(false);
  const [createState, setCreateState] = useState<CreateRoomFormState>(defaultCreateState);
  const [createMemberSearch, setCreateMemberSearch] = useState('');
  const [createMemberResults, setCreateMemberResults] = useState<UserSearchResult[]>([]);
  const [createMemberSelections, setCreateMemberSelections] = useState<UserSearchResult[]>([]);
  const [createMemberLoading, setCreateMemberLoading] = useState(false);
  const [joinRoomId, setJoinRoomId] = useState('');
  const [joinPersonId, setJoinPersonId] = useState('');
  const [joinRole, setJoinRole] = useState('member');
  const [joinTreeId, setJoinTreeId] = useState('');
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

  const getUserLabel = useCallback(
    (user: UserSearchResult) => user.fullName?.trim() || user.username || user.email,
    [],
  );

  const getUserSecondaryLabel = useCallback(
    (user: UserSearchResult) => user.email || user.phone || user.username,
    [],
  );

  const filterTabs: { key: RoomFilter; label: string }[] = [
    { key: 'all', label: 'Tất cả' },
    { key: 'family', label: 'Gia phả' },
    { key: 'branch', label: 'Nhánh' },
    { key: 'private_chat', label: 'Riêng tư' },
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
      .filter((room) => filter === 'all' || room.roomType === filter)
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
    if ((showCreate || showJoin) && !availableTrees.length && !loadingTrees) {
      ensureTreesLoaded();
    }
  }, [showCreate, showJoin, availableTrees.length, loadingTrees, ensureTreesLoaded]);

  useEffect(() => {
    if (joinTreeId) {
      loadTreeMembers(joinTreeId);
    } else {
      setJoinPersonId('');
    }
  }, [joinTreeId, loadTreeMembers]);

  const handleSendText = useCallback(() => {
    if (!currentRoom || !messageInput.trim()) return;
    setSending(true);
    sendMessage({
      roomId: currentRoom.id,
      messageText: messageInput.trim(),
      messageType: 'text',
    });
    setMessageInput('');
    setTimeout(() => setSending(false), 150);
  }, [currentRoom, messageInput, sendMessage]);

  const handleUploadAttachment = async (event: React.ChangeEvent<HTMLInputElement>) => {
    if (!currentRoom) return;
    const file = event.target.files?.[0];
    if (!file) return;
    setIsUploading(true);
    try {
      await sendAttachment(currentRoom.id, file);
    } finally {
      setIsUploading(false);
      event.target.value = '';
    }
  };

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
    if (createState.roomType === 'branch') {
      if (!createState.familyTreeId) {
        showToast.warning('Chọn cây gia phả cho phòng nhánh');
        return;
      }
      if (!createState.branchPersonId) {
        showToast.warning('Chọn thành viên đại diện cho nhánh');
        return;
      }
      const room = await createBranchRoom({
        branchPersonId: createState.branchPersonId,
        name: trimmedName,
        description: trimmedDescription || undefined,
      });
      if (room) {
        selectRoom(room.id);
        setShowCreate(false);
        setCreateState(defaultCreateState);
        setCreateMemberSelections([]);
        setCreateMemberSearch('');
        setCreateMemberResults([]);
      }
      return;
    }
    if (createState.roomType !== 'private_chat' && !createState.familyTreeId) {
      showToast.warning('Chọn cây gia phả cho phòng này');
      return;
    }
    const memberIds = createMemberSelections.map((member) => member.id);
    const payload = {
      roomType: createState.roomType,
      name: trimmedName,
      description: trimmedDescription || undefined,
      familyTreeId: createState.roomType === 'private_chat' ? undefined : createState.familyTreeId || undefined,
      memberUserIds: memberIds.length ? memberIds : undefined,
    };
    const room = await createRoom(payload);
    if (room) {
      selectRoom(room.id);
      setShowCreate(false);
      setCreateState(defaultCreateState);
      setCreateMemberSelections([]);
      setCreateMemberSearch('');
      setCreateMemberResults([]);
    }
  };

  const handleJoinRoom = async () => {
    const roomId = joinRoomId.trim();
    if (!roomId) {
      showToast.warning('Chọn hoặc nhập phòng cần tham gia');
      return;
    }
    const room = await joinRoom(roomId, {
      personId: joinPersonId || undefined,
      role: joinRole as any,
    });
    if (room) {
      selectRoom(room.id);
      setShowJoin(false);
      setJoinRoomId('');
      setJoinPersonId('');
      setJoinRole('member');
      setJoinTreeId('');
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
        <header className="px-4 py-3 border-b border-white/10 flex items-center justify-between relative">
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
                    />

                    <div className="absolute top-full right-0 mt-2 w-56 bg-[#0f172a] border border-white/10 rounded-2xl shadow-2xl py-3 z-50 overflow-hidden">
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
            <button
              onClick={() => setShowJoin(true)}
              className="text-sm text-amber-200 hover:text-white transition whitespace-nowrap"
            >
              + Tham gia
            </button>
          </div>
          <div className="flex items-center gap-2 flex-wrap">
            {(showAllFilters ? filterTabs : filterTabs.slice(0, 4)).map((tab) => (
              <button
                key={tab.key}
                onClick={() => setFilter(tab.key)}
                className={`px-3 py-1.5 rounded-lg text-sm transition ${
                  filter === tab.key ? 'bg-[#ffd89b] text-[#1e2a3a] font-semibold' : 'bg-white/5 hover:bg-white/10'
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
                className={`w-full text-left px-4 py-3 transition ${
                  room.id === currentRoom?.id
                    ? 'bg-white/10'
                    : unreadByRoom[room.id]
                        ? 'bg-white/5 border-l-2 border-amber-300'
                        : 'hover:bg-white/5'
                }`}
              >
                <div className="flex items-center justify-between">
                  <p className="font-semibold truncate">{room.name}</p>
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
      <div className="bg-slate-900/95 text-white rounded-2xl shadow-2xl border border-white/10 overflow-hidden relative">
        <header className="flex items-center justify-between px-4 py-3 border-b border-white/10">
          <div className="flex items-center gap-2">
            <button
              onClick={() => setViewMode('list')}
              className="p-1.5 rounded-full hover:bg-white/10 transition"
              title="Danh sách"
            >
              <ChevronDown size={18} className="transform rotate-90" />
            </button>
            <div>
              <p className="font-semibold text-sm">{currentRoom?.name || 'Tin nhắn'}</p>
              {currentRoom && (
                <button
                  onClick={() => setShowMembersList((prev) => !prev)}
                  className="text-xs text-emerald-200 hover:text-white transition underline-offset-2 hover:underline flex items-center gap-1"
                >
                  {memberCount} thành viên
                </button>
              )}
            </div>
          </div>
          <div className="flex items-center gap-1">
            <button
              onClick={() => setIsCollapsed(true)}
              className="p-1.5 rounded-full hover:bg-white/10 transition"
              title="Thu nhỏ"
            >
              <ChevronRight size={18} />
            </button>
            <button
              onClick={closeWidget}
              className="p-1.5 rounded-full hover:bg-white/10 transition"
              title="Đóng"
            >
              <X size={18} />
            </button>
          </div>
        </header>
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
                    {member.personId && (
                      <p className="text-[11px] text-white/60">Đã liên kết với một thành viên trong cây</p>
                    )}
                  </div>
                  <span
                    className={`text-[10px] uppercase tracking-wide px-2 py-0.5 rounded ${
                      member.role === 'admin'
                        ? 'bg-amber-500/20 text-amber-200'
                        : member.role === 'moderator'
                            ? 'bg-sky-500/20 text-sky-200'
                            : 'bg-white/10 text-white/70'
                    }`}
                  >
                    {member.role === 'admin'
                      ? 'Admin'
                      : member.role === 'moderator'
                          ? 'Điều phối'
                          : 'Thành viên'}
                  </span>
                </div>
              ))}
            </div>
          </div>
        )}
        {currentRoom ? (
          <>
            <div className="h-[360px] overflow-y-auto px-4 py-3 space-y-3">
              {currentRoomState?.hasMore && (
                <button
                  onClick={() => loadOlderMessages(currentRoom.id)}
                  className="text-xs text-white/60 mx-auto block hover:text-white transition"
                  disabled={currentRoomState?.isLoading}
                >
                  {currentRoomState?.isLoading ? 'Đang tải...' : 'Xem tin nhắn cũ hơn'}
                </button>
              )}
              {currentMessages.map((message) => {
                const isMine = message.senderId === currentUserId;
                return (
                  <div key={message.id} className={`flex ${isMine ? 'justify-end' : 'justify-start'}`}>
                    <div
                      className={`max-w-[80%] rounded-2xl px-3 py-2 text-sm shadow ${
                        isMine
                          ? 'bg-gradient-to-r from-amber-500 to-rose-500 text-slate-900'
                          : 'bg-white/10 text-white'
                      }`}
                    >
                      <p className="text-[11px] mb-1 opacity-80">{isMine ? 'Bạn' : message.senderName}</p>
                      {message.messageType === 'text' && (
                        <p className="whitespace-pre-line">{message.messageText}</p>
                      )}
                      {message.messageType === 'image' && message.fileUrl && (
                        <img
                          src={message.fileUrl}
                          alt={message.fileName || 'Ảnh đính kèm'}
                          className="rounded-lg max-h-48 object-cover"
                        />
                      )}
                      {message.messageType === 'file' && message.fileUrl && (
                        <a
                          href={message.fileUrl}
                          target="_blank"
                          rel="noopener noreferrer"
                          className="text-xs underline flex items-center gap-1"
                        >
                          <Paperclip size={12} />
                          {message.fileName || 'Tải tệp'}
                        </a>
                      )}
                      <p className="text-[10px] mt-1 opacity-70">
                        {new Date(message.createdAt).toLocaleString()}
                      </p>
                    </div>
                  </div>
                );
              })}
              <div ref={messageEndRef} />
            </div>
            <div className="px-4 py-3 border-t border-white/10 space-y-2">
              <div className="flex items-end gap-2">
                <button
                  onClick={() => fileInputRef.current?.click()}
                  className="p-2 rounded-xl bg-white/5 hover:bg-white/10 transition"
                  disabled={isUploading}
                >
                  {isUploading ? <Loader2 className="animate-spin" size={16} /> : <Paperclip size={16} />}
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
                  rows={2}
                  className="flex-1 bg-white/5 border border-white/10 rounded-xl px-3 py-2 text-sm focus:outline-none"
                  placeholder="Nhập tin nhắn..."
                />
                <button
                  onClick={handleSendText}
                  disabled={!messageInput.trim() || sending}
                  className="p-3 rounded-2xl bg-gradient-to-r from-emerald-400 to-cyan-400 text-slate-900 disabled:opacity-50"
                >
                  <Send size={16} />
                </button>
              </div>
              <input ref={fileInputRef} type="file" className="hidden" onChange={handleUploadAttachment} />
            </div>
          </>
        ) : (
          <div className="h-[420px] flex items-center justify-center text-center px-6">
            <div>
              <Users size={36} className="mx-auto text-white/40 mb-3" />
              <p className="font-semibold">Chọn một phòng chat</p>
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
          <div className="bg-slate-900 text-white rounded-2xl w-full max-w-lg shadow-2xl border border-white/10 p-6 space-y-4">
            <div className="flex items-center justify-between">
              <p className="font-semibold">Tạo phòng chat</p>
              <button onClick={() => setShowCreate(false)}>
                <X size={20} />
              </button>
            </div>
            <div className="space-y-3">
              <label className="text-sm space-y-1 block">
                <span>Loại phòng</span>
                <select
                  value={createState.roomType}
                  onChange={async (e) => {
                    const nextType = e.target.value as ChatRoomType;
                    setCreateState((prev) => ({
                      ...prev,
                      roomType: nextType,
                      branchPersonId: nextType === 'branch' ? prev.branchPersonId : '',
                      familyTreeId: nextType === 'private_chat' ? '' : prev.familyTreeId,
                    }));
                    if (nextType !== 'private_chat') {
                      await ensureTreesLoaded();
                    }
                  }}
                  className="w-full bg-white/5 border border-white/10 rounded-xl px-3 py-2 text-sm"
                >
                  <option value="family">Theo cây gia phả</option>
                  <option value="branch">Theo nhánh</option>
                  <option value="private_chat">Riêng tư / nhóm nhỏ</option>
                </select>
              </label>
              <label className="text-sm space-y-1 block">
                <span>Tên phòng</span>
                <input
                  value={createState.name}
                  onChange={(e) => setCreateState((prev) => ({ ...prev, name: e.target.value }))}
                  className="w-full bg-white/5 border border-white/10 rounded-xl px-3 py-2 text-sm"
                />
              </label>
              <label className="text-sm space-y-1 block">
                <span>Mô tả</span>
                <textarea
                  value={createState.description}
                  onChange={(e) => setCreateState((prev) => ({ ...prev, description: e.target.value }))}
                  className="w-full bg-white/5 border border-white/10 rounded-xl px-3 py-2 text-sm"
                  rows={2}
                />
              </label>
              {createState.roomType !== 'private_chat' && (
                <label className="text-sm space-y-1 block">
                  <span>Chọn cây gia phả</span>
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
                    className="w-full bg-white/5 border border-white/10 rounded-xl px-3 py-2 text-sm"
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
                  <span>Chọn thành viên đại diện cho nhánh</span>
                  <select
                    value={createState.branchPersonId}
                    onChange={(e) =>
                      setCreateState((prev) => ({
                        ...prev,
                        branchPersonId: e.target.value,
                      }))
                    }
                    className="w-full bg-white/5 border border-white/10 rounded-xl px-3 py-2 text-sm"
                  >
                    <option value="">-- Chọn --</option>
                    {(treeMembers[createState.familyTreeId] ?? []).map((person) => (
                      <option key={person.id} value={person.id}>
                        {person.fullName}
                      </option>
                    ))}
                  </select>
                  <p className="text-xs text-emerald-200/80">
                    Thành viên sẽ được tự động thêm theo nhánh này (bao gồm hậu duệ và vợ/chồng đã liên kết tài khoản).
                  </p>
                </label>
              )}
              {createState.roomType !== 'branch' && (
                <div className="text-sm space-y-2">
                <div className="space-y-1">
                  <span className="block">Thêm thành viên (tìm bằng email / số điện thoại / tên)</span>
                  <input
                    value={createMemberSearch}
                    onChange={(e) => setCreateMemberSearch(e.target.value)}
                    placeholder="Nhập ít nhất 2 ký tự để tìm..."
                    className="w-full bg-white/5 border border-white/10 rounded-xl px-3 py-2 text-sm"
                  />
                </div>
                {createMemberLoading && <p className="text-xs text-white/60">Đang tìm kiếm...</p>}
                {createMemberResults.length > 0 && (
                  <div className="max-h-40 overflow-y-auto border border-white/10 rounded-xl divide-y divide-white/5">
                    {createMemberResults.map((user) => (
                      <button
                        key={user.id}
                        type="button"
                        onClick={() => addMemberSelection(user)}
                        className="w-full text-left px-3 py-2 hover:bg-white/10 transition"
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
                        className="inline-flex items-center gap-1 px-3 py-1 rounded-full bg-white/10 text-xs"
                      >
                        {getUserLabel(user)}
                        <button
                          type="button"
                          onClick={() => removeMemberSelection(user.id)}
                          className="text-white/60 hover:text-white"
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
            <div className="flex justify-end gap-2">
              <button onClick={() => setShowCreate(false)} className="px-4 py-2 rounded-xl bg-white/10 text-sm">
                Huỷ
              </button>
              <button
                onClick={handleCreateRoom}
                className="px-4 py-2 rounded-xl bg-gradient-to-r from-amber-500 to-rose-500 text-sm font-semibold text-slate-900"
              >
                Tạo phòng
              </button>
            </div>
          </div>
        </div>
      )}

      {showJoin && (
        <div className="fixed inset-0 z-[1300] bg-black/50 flex items-center justify-center px-4">
          <div className="bg-slate-900 text-white rounded-2xl w-full max-w-md border border-white/10 p-6 space-y-3">
            <div className="flex items-center justify-between">
              <p className="font-semibold">Tham gia phòng</p>
              <button onClick={() => setShowJoin(false)}>
                <X size={20} />
              </button>
            </div>
            <label className="text-sm space-y-1 block">
              <span>Chọn phòng của bạn</span>
              <select
                value={rooms.some((room) => room.id === joinRoomId) ? joinRoomId : ''}
                onChange={(e) => setJoinRoomId(e.target.value)}
                className="w-full bg-white/5 border border-white/10 rounded-xl px-3 py-2 text-sm"
              >
                <option value="">-- Chọn phòng đã có --</option>
                {rooms.map((room) => (
                  <option key={room.id} value={room.id}>
                    {room.name}
                  </option>
                ))}
              </select>
            </label>
            <label className="text-sm space-y-1 block">
              <span>Hoặc nhập Room ID (nếu bạn được cung cấp trực tiếp)</span>
              <input
                value={joinRoomId}
                onChange={(e) => setJoinRoomId(e.target.value)}
                placeholder="Dán Room ID tại đây"
                className="w-full bg-white/5 border border-white/10 rounded-xl px-3 py-2 text-sm"
              />
            </label>
            <label className="text-sm space-y-1 block">
              <span>Liên kết với cây gia phả (tuỳ chọn)</span>
              <select
                value={joinTreeId}
                onChange={(e) => setJoinTreeId(e.target.value)}
                className="w-full bg-white/5 border border-white/10 rounded-xl px-3 py-2 text-sm"
              >
                <option value="">-- Không liên kết --</option>
                {availableTrees.map((tree) => (
                  <option key={tree.id} value={tree.id}>
                    {tree.name}
                  </option>
                ))}
              </select>
            </label>
            {joinTreeId && (
              <label className="text-sm space-y-1 block">
                <span>Chọn thành viên đại diện (tuỳ chọn)</span>
                <select
                  value={joinPersonId}
                  onChange={(e) => setJoinPersonId(e.target.value)}
                  className="w-full bg-white/5 border border-white/10 rounded-xl px-3 py-2 text-sm"
                >
                  <option value="">-- Không chọn --</option>
                  {(treeMembers[joinTreeId] ?? []).map((person) => (
                    <option key={person.id} value={person.id}>
                      {person.fullName}
                    </option>
                  ))}
                </select>
              </label>
            )}
            <label className="text-sm space-y-1 block">
              <span>Vai trò</span>
              <select
                value={joinRole}
                onChange={(e) => setJoinRole(e.target.value)}
                className="w-full bg-white/5 border border-white/10 rounded-xl px-3 py-2 text-sm"
              >
                <option value="member">Thành viên</option>
                <option value="moderator">Điều hành</option>
                <option value="admin">Quản trị</option>
              </select>
            </label>
            <div className="flex justify-end gap-2 pt-2">
              <button onClick={() => setShowJoin(false)} className="px-4 py-2 rounded-xl bg-white/10 text-sm">
                Huỷ
              </button>
              <button
                onClick={handleJoinRoom}
                className="px-4 py-2 rounded-xl bg-gradient-to-r from-emerald-400 to-cyan-400 text-sm font-semibold text-slate-900"
              >
                Tham gia
              </button>
            </div>
          </div>
        </div>
      )}

      {showDirect && (
        <div className="fixed inset-0 z-[1300] bg-black/50 flex items-center justify-center px-4">
          <div className="bg-slate-900 text-white rounded-2xl w-full max-w-md border border-white/10 p-6 space-y-3">
            <div className="flex items-center justify-between">
              <p className="font-semibold">Chat riêng tư</p>
              <button onClick={() => setShowDirect(false)}>
                <X size={20} />
              </button>
            </div>
            <div className="text-sm space-y-2">
              <div className="space-y-1">
                <span className="block">Tìm người nhận (email / số điện thoại / tên)</span>
                <input
                  value={directSearchTerm}
                  onChange={(e) => {
                    setDirectSearchTerm(e.target.value);
                    if (directSelection) {
                      setDirectSelection(null);
                    }
                  }}
                  placeholder="Nhập ít nhất 2 ký tự để tìm..."
                  className="w-full bg-white/5 border border-white/10 rounded-xl px-3 py-2 text-sm"
                />
              </div>
              {directSearchLoading && <p className="text-xs text-white/60">Đang tìm kiếm...</p>}
              {directSearchResults.length > 0 && !directSelection && (
                <div className="max-h-40 overflow-y-auto border border-white/10 rounded-xl divide-y divide-white/5">
                  {directSearchResults.map((user) => (
                    <button
                      key={user.id}
                      type="button"
                      onClick={() => handleSelectDirectUser(user)}
                      className="w-full text-left px-3 py-2 hover:bg-white/10 transition"
                    >
                      <p className="font-medium">{getUserLabel(user)}</p>
                      <p className="text-xs text-white/70">{getUserSecondaryLabel(user)}</p>
                    </button>
                  ))}
                </div>
              )}
              {directSelection && (
                <div className="flex items-center justify-between bg-white/5 border border-white/10 rounded-xl px-3 py-2">
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
                    className="text-xs text-amber-200 hover:text-white"
                  >
                    Chọn lại
                  </button>
                </div>
              )}
            </div>
            <label className="text-sm space-y-1 block">
              <span>Tên phòng (tuỳ chọn)</span>
              <input
                value={directRoomName}
                onChange={(e) => setDirectRoomName(e.target.value)}
                className="w-full bg-white/5 border border-white/10 rounded-xl px-3 py-2 text-sm"
              />
            </label>
            <div className="flex justify-end gap-2 pt-2">
              <button onClick={() => setShowDirect(false)} className="px-4 py-2 rounded-xl bg-white/10 text-sm">
                Huỷ
              </button>
              <button
                onClick={handleCreateDirect}
                className="px-4 py-2 rounded-xl bg-gradient-to-r from-purple-500 to-indigo-500 text-sm font-semibold"
              >
                Bắt đầu
              </button>
            </div>
          </div>
        </div>
      )}
    </>
  );
};

export default ChatWidget;

