import React, { useState, useRef, useEffect, useCallback } from 'react';
import { Bot, X, Send, Mic, Square } from 'lucide-react';
import ReactMarkdown from 'react-markdown';

const API_BASE = (import.meta.env.VITE_API_BASE_URL || 'http://localhost:8080/legacy')
  .replace(/\/+$/, '');

const cn = (...inputs: (string | undefined | null | false)[]) =>
  inputs.filter(Boolean).join(' ');

const getAuthToken = (): string | null => {
  try {
    const token = localStorage.getItem('authToken');
    if (token) return token;
    const cookieToken = document.cookie
      .split('; ')
      .find(row => row.startsWith('JWT_TOKEN='))
      ?.split('=')[1];
    return cookieToken || null;
  } catch {
    return null;
  }
};

const CHAT_WIDTH = 384;
const CHAT_HEIGHT = 520;
const BUTTON_SIZE = 56;
const EDGE_PADDING = 16;

const SUGGESTIONS = [
  'LegacyMap là gì và dùng để làm gì?',
  'Làm sao để tạo cây gia phả mới?',
  'Cách mời người thân tham gia cây gia phả?',
  'Tôi bị lỗi đăng nhập, phải làm sao?',
  'LegacyMap có miễn phí không?'
];

export default function LegacyChatWidget() {
  const [isOpen, setIsOpen] = useState(false);
  const [input, setInput] = useState('');
  const [messages, setMessages] = useState<
    Array<{ text: string; sender: 'user' | 'bot' }>
  >([
    {
      text:
        'Chào bạn! Mình là trợ lý hỗ trợ của LegacyMap.vn đây ạ! Mình có thể giúp gì cho bạn hôm nay nào?',
      sender: 'bot'
    }
  ]);

  const [isTyping, setIsTyping] = useState(false);

  // DRAG STATE
  const [position, setPosition] = useState({ x: 0, y: 0 });
  const positionRef = useRef(position);
  useEffect(() => {
    positionRef.current = position;
  }, [position]);

  const dragRef = useRef({
    isDragging: false,
    offsetX: 0,
    offsetY: 0,
    hasMoved: false,
  });

  useEffect(() => {
    const offsetTop = 72;
    const x = window.innerWidth - BUTTON_SIZE - EDGE_PADDING;
    const y = offsetTop;

    setPosition({
      x: Math.max(EDGE_PADDING, x),
      y: Math.max(EDGE_PADDING, y)
    });
  }, []);

  const handleDragStart = (e: React.MouseEvent<HTMLButtonElement>) => {
    dragRef.current.isDragging = true;
    dragRef.current.offsetX = e.clientX - positionRef.current.x;
    dragRef.current.offsetY = e.clientY - positionRef.current.y;
    dragRef.current.hasMoved = false;
  };

  const handleDragMove = useCallback(
    (e: MouseEvent) => {
      if (!dragRef.current.isDragging) return;

      const x = e.clientX - dragRef.current.offsetX;
      const y = e.clientY - dragRef.current.offsetY;
      dragRef.current.hasMoved = true;

      const currentWidth = isOpen ? CHAT_WIDTH : BUTTON_SIZE;
      const currentHeight = isOpen ? CHAT_HEIGHT : BUTTON_SIZE;

      const maxX = window.innerWidth - currentWidth - EDGE_PADDING;
      const maxY = window.innerHeight - currentHeight - EDGE_PADDING;

      setPosition({
        x: Math.min(Math.max(EDGE_PADDING, x), Math.max(EDGE_PADDING, maxX)),
        y: Math.min(Math.max(EDGE_PADDING, y), Math.max(EDGE_PADDING, maxY))
      });
    },
    [isOpen]
  );

  const handleDragEnd = useCallback(() => {
    dragRef.current.isDragging = false;
  }, []);

  useEffect(() => {
    window.addEventListener('mousemove', handleDragMove);
    window.addEventListener('mouseup', handleDragEnd);
    return () => {
      window.removeEventListener('mousemove', handleDragMove);
      window.removeEventListener('mouseup', handleDragEnd);
    };
  }, [handleDragMove, handleDragEnd]);

  const toggleOpen = () => {
    setIsOpen(prev => {
      const next = !prev;
      if (next) {
        setPosition(prevPos => {
          const maxX = window.innerWidth - CHAT_WIDTH - EDGE_PADDING;
          const maxY = window.innerHeight - CHAT_HEIGHT - EDGE_PADDING;
          return {
            x: Math.min(
              Math.max(EDGE_PADDING, prevPos.x),
              Math.max(EDGE_PADDING, maxX)
            ),
            y: Math.min(
              Math.max(EDGE_PADDING, prevPos.y),
              Math.max(EDGE_PADDING, maxY)
            )
          };
        });
      }
      return next;
    });
  };

  const handleButtonClick = () => {
    if (dragRef.current.hasMoved) {
      dragRef.current.hasMoved = false;
      return;
    }
    toggleOpen();
  };

  const eventSourceRef = useRef<EventSource | null>(null);
  const messagesEndRef = useRef<HTMLDivElement>(null);
  const sessionId = useRef('web_' + Math.random().toString(36).substr(2, 9)).current;

  const scrollToBottom = () => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  };

  useEffect(() => {
    scrollToBottom();
  }, [messages, isTyping]);

  // ================= VOICE =================
  const [isRecording, setIsRecording] = useState(false);
  const mediaRecorderRef = useRef<MediaRecorder | null>(null);
  const audioChunksRef = useRef<Blob[]>([]);

  const uploadAudioToBackend = async (audioBlob: Blob) => {
    try {
      const formData = new FormData();
      formData.append('audio', audioBlob, 'voice.webm');

      const token = getAuthToken();

      const res = await fetch(
        `${API_BASE}/voice/chat${sessionId ? `?sessionId=${sessionId}` : ''}`,
        {
          method: 'POST',
          headers: {
            ...(token ? { Authorization: `Bearer ${token}` } : {}),
          },
          body: formData,
        }
      );

      if (!res.ok) throw new Error();

      const data = await res.json();

      setMessages(prev => [...prev, { text: data.userText, sender: 'user' }]);
      setMessages(prev => [...prev, { text: data.botText, sender: 'bot' }]);

      if (data.audio && data.audio.startsWith('data:audio')) {
        new Audio(data.audio).play();
      }

    } catch {
      setMessages(prev => [
        ...prev,
        { text: 'Em đang hơi mệt xíu, anh/chị nói lại giúp em nha', sender: 'bot' }
      ]);
    }
  };

  const startRecording = async () => {
    try {
      const stream = await navigator.mediaDevices.getUserMedia({ audio: true });
      const mediaRecorder = new MediaRecorder(stream);

      mediaRecorderRef.current = mediaRecorder;
      audioChunksRef.current = [];
      setIsRecording(true);

      mediaRecorder.ondataavailable = (event) => {
        if (event.data.size > 0) {
          audioChunksRef.current.push(event.data);
        }
      };

      mediaRecorder.onstop = () => {
        setIsRecording(false);
        const audioBlob = new Blob(audioChunksRef.current, { type: 'audio/webm' });
        uploadAudioToBackend(audioBlob);
        stream.getTracks().forEach((t) => t.stop());
      };

      mediaRecorder.start();
    } catch {
      setMessages(prev => [
        ...prev,
        {
          text: 'Em không mở được micro rồi ạ. Anh/chị kiểm tra lại quyền truy cập giúp em nhé 🙏',
          sender: 'bot'
        },
      ]);
    }
  };

  const stopRecording = () => {
    if (mediaRecorderRef.current && mediaRecorderRef.current.state === 'recording') {
      mediaRecorderRef.current.stop();
    }
  };

  const toggleRecording = () => {
    if (isRecording) stopRecording();
    else startRecording();
  };

  // ================= TEXT CHAT (SSE) =================
  const sendToBot = (userMessage: string) => {
    const trimmed = userMessage.trim();
    if (!trimmed || isTyping) return;

    if (eventSourceRef.current) eventSourceRef.current.close();

    setMessages(prev => [...prev, { text: trimmed, sender: 'user' }]);
    setInput('');
    setIsTyping(true);

    const token = getAuthToken();
    const textChatUrl = `${API_BASE}/support/chat`;

    const params = new URLSearchParams({
      sessionId,
      message: trimmed,
      authToken: token || ''
    });

    const es = new EventSource(`${textChatUrl}?${params}`);
    eventSourceRef.current = es;

    let botResponse = '';

    es.onmessage = event => {
      if (event.data === '[DONE]') {
        es.close();
        setIsTyping(false);
        return;
      }

      botResponse += event.data;

      setMessages(prev => {
        const newMessages = [...prev];
        if (
          newMessages.length > 0 &&
          newMessages[newMessages.length - 1].sender === 'bot'
        ) {
          newMessages[newMessages.length - 1].text = botResponse;
        } else {
          newMessages.push({ text: botResponse, sender: 'bot' });
        }
        return newMessages;
      });
    };

    es.onerror = () => {
      es.close();
      setIsTyping(false);
      setMessages(prev => [
        ...prev,
        { text: 'Oops! Mình đang hơi chậm, bạn thử lại sau vài giây nhé!', sender: 'bot' }
      ]);
    };
  };

  const sendMessage = (e: React.FormEvent) => {
    e.preventDefault();
    if (!input.trim() || isTyping) return;
    sendToBot(input);
  };

  const handleSuggestionClick = (suggestion: string) => {
    if (isTyping) return;
    sendToBot(suggestion);
  };

  // ================= RETURN UI =================
  return (
    <div
      className="fixed z-50 flex flex-col items-end gap-3"
      style={{ top: position.y, left: position.x }}
    >
      {/* CHAT WINDOW */}
      <div
        className={cn(
          'flex flex-col bg-[#2C3E50] rounded-2xl shadow-2xl transition-all duration-300 overflow-hidden',
          isOpen ? 'w-96 h-[520px] opacity-100' : 'w-0 h-0 opacity-0 pointer-events-none'
        )}
      >
        {/* HEADER */}
        <div className="flex items-center justify-between p-3 border-b border-[#C9A961]/20 bg-[#1E293B]/50">
          <div className="flex items-center gap-3">
            <div className="w-10 h-10 rounded-full bg-[#C9A961] flex items-center justify-center shadow-md">
              <Bot className="w-6 h-6 text-[#2C3E50]" />
            </div>
            <div>
              <h3 className="font-bold text-white text-base">Legacy Map AI</h3>
              <div className="flex items-center gap-1.5">
                <span className="w-2 h-2 rounded-full bg-green-500 animate-pulse" />
                <p className="text-xs text-[#C9A961]">Trợ lý hỗ trợ 24/7</p>
              </div>
            </div>
          </div>
          <button
            type="button"
            onClick={() => setIsOpen(false)}
            className="text-[#C9A961] hover:text-white hover:bg-white/10 p-1 rounded-lg transition-colors"
          >
            <X className="w-5 h-5" />
          </button>
        </div>

        {/* MESSAGES */}
        <div className="flex-1 overflow-y-auto p-4 space-y-4 bg-[#2C3E50]">
          {messages.map((msg, i) => (
            <div
              key={i}
              className={cn(
                'flex',
                msg.sender === 'user' ? 'justify-end' : 'justify-start'
              )}
            >
              <div
                className={cn(
                  'max-w-[85%] rounded-2xl px-4 py-2.5 text-sm whitespace-pre-wrap shadow-sm',
                  msg.sender === 'user'
                    ? 'bg-[#C9A961] text-[#2C3E50] font-medium rounded-tr-sm'
                    : 'bg-[#1E293B] text-gray-100 border border-[#C9A961]/20 rounded-tl-sm'
                )}
              >
                {msg.sender === 'bot' ? (
                  <ReactMarkdown
                    components={{
                      a: ({ ...props }) => (
                        <a
                          {...props}
                          target={
                            props.href && props.href.startsWith('/')
                              ? '_self'
                              : '_blank'
                          }
                          rel="noopener noreferrer"
                          className="underline text-[#C9A961] hover:opacity-80"
                        />
                      )
                    }}
                  >
                    {msg.text}
                  </ReactMarkdown>
                ) : (
                  msg.text
                )}
              </div>
            </div>
          ))}

          {isTyping && messages[messages.length - 1]?.sender === 'user' && (
            <div className="flex justify-start">
              <div className="bg-[#1E293B] border border-[#C9A961]/20 rounded-2xl rounded-tl-sm px-4 py-3">
                <div className="flex gap-1.5">
                  <span className="w-2 h-2 bg-[#C9A961] rounded-full animate-bounce" />
                  <span
                    className="w-2 h-2 bg-[#C9A961] rounded-full animate-bounce"
                    style={{ animationDelay: '150ms' }}
                  />
                  <span
                    className="w-2 h-2 bg-[#C9A961] rounded-full animate-bounce"
                    style={{ animationDelay: '300ms' }}
                  />
                </div>
              </div>
            </div>
          )}

          <div ref={messagesEndRef} />
        </div>

        {/* GỢI Ý */}
        {!messages.some(m => m.sender === 'user') && (
          <div className="px-4 pt-2 pb-1 bg-[#1E293B]/40 border-t border-[#C9A961]/10">
            <p className="text-[11px] uppercase tracking-wide text-[#C9A961]/70 mb-1">
              Gợi ý câu hỏi
            </p>
            <div className="flex flex-wrap gap-2">
              {SUGGESTIONS.map(s => (
                <button
                  key={s}
                  type="button"
                  onClick={() => handleSuggestionClick(s)}
                  disabled={isTyping}
                  className="text-[11px] px-3 py-1 rounded-full border border-[#C9A961]/40 text-[#C9A961] hover:bg-[#C9A961]/10 disabled:opacity-40 transition-colors"
                >
                  {s}
                </button>
              ))}
            </div>
          </div>
        )}

        {/* INPUT */}
        <form
          onSubmit={sendMessage}
          className="p-4 border-t border-[#C9A961]/20 bg-[#1E293B]/30"
        >
          <div className="flex gap-2 items-center">
            <button
              type="button"
              onClick={toggleRecording}
              disabled={isTyping}
              className={cn(
                'rounded-xl p-3 border transition-all flex items-center justify-center',
                isRecording
                  ? 'bg-red-600 border-red-400 text-white animate-pulse'
                  : 'bg-[#1E293B] border-[#C9A961]/40 text-[#C9A961] hover:bg-[#C9A961]/10'
              )}
            >
              {isRecording ? <Square className="w-4 h-4" /> : <Mic className="w-4 h-4" />}
            </button>

            <input
              type="text"
              value={input}
              onChange={e => setInput(e.target.value)}
              placeholder={isRecording ? 'Đang ghi âm...' : 'Nhập tin nhắn...'}
              disabled={isTyping}
              className="flex-1 bg-[#1E293B] text-white placeholder:text-[#C9A961]/50 rounded-xl px-4 py-3 text-sm border border-[#C9A961]/20 focus:outline-none focus:border-[#C9A961] focus:ring-1 focus:ring-[#C9A961] disabled:opacity-50"
            />
            <button
              type="submit"
              disabled={!input.trim() || isTyping}
              className="bg-[#C9A961] text-[#2C3E50] rounded-xl p-3 hover:bg-[#D4AF37] disabled:opacity-50 transition-colors shadow-md"
            >
              <Send className="w-5 h-5" />
            </button>
          </div>
        </form>
      </div>

      {/* TOGGLE BUTTON */}
      <button
        type="button"
        onMouseDown={handleDragStart}
        onClick={handleButtonClick}
        className="group w-14 h-14 rounded-full bg-[#C9A961] text-[#2C3E50] shadow-lg hover:bg-[#D4AF37] hover:scale-105 active:scale-95 transition-all flex items-center justify-center cursor-move"
      >
        {isOpen ? (
          <X className="w-7 h-7 group-hover:rotate-90 transition-transform" />
        ) : (
          <Bot className="w-8 h-8 group-hover:-rotate-12 transition-transform" />
        )}
      </button>
    </div>
  );
}
