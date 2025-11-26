import React, { useState, useRef, useEffect } from 'react';
import { Bot, X, Send } from 'lucide-react';
import ReactMarkdown from 'react-markdown';

// Hàm cn đơn giản (không cần @/lib/utils nữa)
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

// Một vài gợi ý câu hỏi cho người dùng
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

  const eventSourceRef = useRef<EventSource | null>(null);
  const messagesEndRef = useRef<HTMLDivElement>(null);
  const sessionId = useRef(
    'web_' + Math.random().toString(36).substr(2, 9)
  ).current;

  const scrollToBottom = () => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  };

  useEffect(() => {
    scrollToBottom();
  }, [messages, isTyping]);

  // Hàm core để gửi tin nhắn lên server (dùng chung cho input & gợi ý)
  const sendToBot = (userMessage: string) => {
    const trimmed = userMessage.trim();
    if (!trimmed || isTyping) return;

    // Đóng event cũ nếu có
    if (eventSourceRef.current) {
      eventSourceRef.current.close();
    }

    setMessages(prev => [...prev, { text: trimmed, sender: 'user' }]);
    setInput('');
    setIsTyping(true);

    const token = getAuthToken();
    const baseUrl = `${import.meta.env.VITE_API_BASE_URL}/support/chat`;

    const params = new URLSearchParams({
      sessionId,
      message: trimmed,
      authToken: token || '' // DÒNG NÀY LÀ CHÌA KHÓA VÀNG!
    });

    const es = new EventSource(`${baseUrl}?${params}`);
    eventSourceRef.current = es;

    let botResponse = '';

    es.onmessage = event => {
      if (event.data === '[DONE]') {
        es.close();
        setIsTyping(false);
        return;
      }

      botResponse += event.data;

      // Chỉ update tin nhắn bot cuối cùng
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
        {
          text: 'Oops! Mình đang hơi chậm, bạn thử lại sau vài giây nhé!',
          sender: 'bot'
        }
      ]);
    };
  };

  const sendMessage = (e: React.FormEvent) => {
    e.preventDefault();
    if (!input.trim() || isTyping) return;
    sendToBot(input);
  };

  const handleSuggestionClick = (suggestion: string) => {
    if (isTyping) return; // đang trả lời thì ignore click
    sendToBot(suggestion);
  };

  return (
 <div className="fixed top-20 right-6 z-50 flex flex-col items-end gap-3">

      {/* Chat Window */}
      <div
        className={cn(
          'flex flex-col bg-[#2C3E50] rounded-2xl shadow-2xl transition-all duration-300 overflow-hidden',
          isOpen ? 'w-96 h-[520px] opacity-100' : 'w-0 h-0 opacity-0 pointer-events-none'
        )}
      >
        {/* Header */}
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

        {/* Messages */}
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
                  <>
                    <ReactMarkdown
                      components={{
                        a: ({
                          node,
                          ...props
                        }: React.ComponentProps<'a'> & { node?: any }) => (
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
                    {i === messages.length - 1 && isTyping && (
                      <span className="inline-block w-2 h-5 ml-1 bg-[#C9A961] animate-pulse align-middle" />
                    )}
                  </>
                ) : (
                  <>
                    {msg.text}
                    {msg.sender as 'user' | 'bot' &&
                      i === messages.length - 1 &&
                      isTyping && (
                        <span className="inline-block w-2 h-5 ml-1 bg-[#C9A961] animate-pulse align-middle" />
                      )}
                  </>
                )}
              </div>
            </div>
          ))}

          {isTyping && messages[messages.length - 1]?.sender === 'user' && (
            <div className="flex justify-start">
              <div className="bg-[#1E293B] border border-[#C9A961]/20 rounded-2xl rounded-tl-sm px-4 py-3">
                <div className="flex gap-1.5">
                  <span
                    className="w-2 h-2 bg-[#C9A961] rounded-full animate-bounce"
                    style={{ animationDelay: '0ms' }}
                  />
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

        {/* Gợi ý câu hỏi – chỉ hiện khi chưa có tin nhắn của user */}
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
                  className="text-[11px] px-3 py-1 rounded-full border border-[#C9A961]/40 text-[#C9A961] hover:bg-[#C9A961]/10 disabled:opacity-40 disabled:hover:bg-transparent transition-colors"
                >
                  {s}
                </button>
              ))}
            </div>
          </div>
        )}

        {/* Input */}
        <form
          onSubmit={sendMessage}
          className="p-4 border-t border-[#C9A961]/20 bg-[#1E293B]/30"
        >
          <div className="flex gap-2">
            <input
              type="text"
              value={input}
              onChange={e => setInput(e.target.value)}
              placeholder="Nhập tin nhắn..."
              disabled={isTyping}
              className="flex-1 bg-[#1E293B] text-white placeholder:text-[#C9A961]/50 rounded-xl px-4 py-3 text-sm border border-[#C9A961]/20 focus:outline-none focus:border-[#C9A961] focus:ring-1 focus:ring-[#C9A961] disabled:opacity-50 transition-all"
            />
            <button
              type="submit"
              disabled={!input.trim() || isTyping}
              className="bg-[#C9A961] text-[#2C3E50] rounded-xl p-3 hover:bg-[#D4AF37] disabled:opacity-50 disabled:hover:bg-[#C9A961] transition-colors shadow-md"
            >
              <Send className="w-5 h-5" />
            </button>
          </div>
        </form>
      </div>

      {/* Toggle Button (luôn hiển thị) */}
      <button
        type="button"
        onClick={() => setIsOpen(prev => !prev)}
        className="group w-14 h-14 rounded-full bg-[#C9A961] text-[#2C3E50] shadow-lg shadow-[#C9A961]/20 hover:bg-[#D4AF37] hover:scale-105 active:scale-95 transition-all duration-300 flex items-center justify-center"
      >
        {isOpen ? (
          <X className="w-7 h-7 transition-transform duration-300 group-hover:rotate-90" />
        ) : (
          <Bot className="w-8 h-8 transition-transform duration-300 group-hover:-rotate-12" />
        )}
      </button>
    </div>
  );
}
