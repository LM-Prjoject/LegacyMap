// src/pages/chatbot/LegacyChatWidget.tsx
import React, { useState, useRef, useEffect } from 'react';
import { MessageCircle, X, Send } from 'lucide-react';

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

export default function LegacyChatWidget() {
  const [isOpen, setIsOpen] = useState(false);
  const [input, setInput] = useState('');
  const [messages, setMessages] = useState<Array<{ text: string; sender: 'user' | 'bot' }>>([
    { text: "Chào bạn! Mình là trợ lý hỗ trợ của LegacyMap.vn đây ạ! Mình có thể giúp gì cho bạn hôm nay nào?", sender: "bot" }
  ]);
  const [isTyping, setIsTyping] = useState(false);

  const eventSourceRef = useRef<EventSource | null>(null);
  const messagesEndRef = useRef<HTMLDivElement>(null);
  const sessionId = useRef("web_" + Math.random().toString(36).substr(2, 9)).current;

  const scrollToBottom = () => {
    messagesEndRef.current?.scrollIntoView({ behavior: "smooth" });
  };

  useEffect(() => {
    scrollToBottom();
  }, [messages, isTyping]);

  const sendMessage = (e: React.FormEvent) => {
    e.preventDefault();
    if (!input.trim() || isTyping) return;

    const userMessage = input.trim();
    setMessages(prev => [...prev, { text: userMessage, sender: "user" }]);
    setInput('');
    setIsTyping(true);

    if (eventSourceRef.current) {
      eventSourceRef.current.close();
    }

    const token = getAuthToken();

    // CHỈ CẦN DÒNG NÀY LÀ ĐÚNG 100% VỚI DỰ ÁN VITE CỦA BẠN
    const baseUrl = `${import.meta.env.VITE_API_BASE_URL}/support/chat`;

    const params = new URLSearchParams({
      sessionId,
      message: userMessage,
      ...(token && { authToken: token })
    });

    const es = new EventSource(`${baseUrl}?${params}`);
    eventSourceRef.current = es;

    let botResponse = '';

    es.onmessage = (event) => {
      if (event.data === "[DONE]") {
        es.close();
        setIsTyping(false);
        return;
      }

      botResponse += event.data;

      // Chỉ UPDATE tin nhắn bot cuối cùng, không tạo mới → hết lỗi duplicate
      setMessages(prev => {
        const newMessages = [...prev];
        if (newMessages.length > 0 && newMessages[newMessages.length - 1].sender === "bot") {
          newMessages[newMessages.length - 1].text = botResponse;
        } else {
          newMessages.push({ text: botResponse, sender: "bot" });
        }
        return newMessages;
      });
    };

    es.onerror = () => {
      es.close();
      setIsTyping(false);
      setMessages(prev => [...prev, {
        text: "Oops! Mình đang hơi chậm, bạn thử lại sau vài giây nhé!",
        sender: "bot"
      }]);
    };
  };

  return (
  <div className="fixed bottom-6 right-6 z-50 flex flex-col items-end gap-3">
    {/* Chat Window */}
    <div
      className={cn(
        "flex flex-col bg-[#2C3E50] rounded-2xl shadow-2xl transition-all duration-300 overflow-hidden",
        isOpen ? "w-96 h-[520px] opacity-100" : "w-0 h-0 opacity-0 pointer-events-none"
      )}
    >
      {/* Header */}
      <div className="flex items-center justify-between p-2 border-b border-[#C9A961]/20">
        <div className="flex items-center gap-3">
          <div className="w-10 h-10 rounded-full bg-[#C9A961] flex items-center justify-center">
            <MessageCircle className="w-5 h-5 text-[#2C3E50]" />
          </div>
          <div>
            <h3 className="font-semibold text-white">Legacy Map Assistant</h3>
            <p className="text-xs text-[#C9A961]">Trợ lý hỗ trợ 24/7</p>
          </div>
        </div>
        <button
          type="button"
          onClick={() => setIsOpen(false)}
          className="text-[#C9A961] hover:text-white"
        >
          <X className="w-5 h-5" />
        </button>
      </div>

      {/* Messages */}
      <div className="flex-1 overflow-y-auto p-4 space-y-4">
        {messages.map((msg, i) => (
          <div
            key={i}
            className={cn(
              "flex",
              msg.sender === "user" ? "justify-end" : "justify-start"
            )}
          >
            <div
              className={cn(
                "max-w-[85%] rounded-2xl px-4 py-2.5 text-sm whitespace-pre-wrap",
                msg.sender === "user"
                  ? "bg-[#C9A961] text-[#2C3E50]"
                  : "bg-[#1E293B] text-white border border-[#C9A961]/20"
              )}
            >
              {msg.text}
              {msg.sender === "bot" &&
                i === messages.length - 1 &&
                isTyping && (
                  <span className="inline-block w-2 h-5 ml-1 bg-[#C9A961] animate-pulse" />
                )}
            </div>
          </div>
        ))}

        {isTyping &&
          messages[messages.length - 1]?.sender === "user" && (
            <div className="flex justify-start">
              <div className="bg-[#1E293B] border border-[#C9A961]/20 rounded-2xl px-4 py-2.5">
                <div className="flex gap-1">
                  <span
                    className="w-2 h-2 bg-[#C9A961] rounded-full animate-bounce"
                    style={{ animationDelay: "0ms" }}
                  />
                  <span
                    className="w-2 h-2 bg-[#C9A961] rounded-full animate-bounce"
                    style={{ animationDelay: "150ms" }}
                  />
                  <span
                    className="w-2 h-2 bg-[#C9A961] rounded-full animate-bounce"
                    style={{ animationDelay: "300ms" }}
                  />
                </div>
              </div>
            </div>
          )}
        <div ref={messagesEndRef} />
      </div>

      {/* Input */}
      <form onSubmit={sendMessage} className="p-4 border-t border-[#C9A961]/20">
        <div className="flex gap-2">
          <input
            type="text"
            value={input}
            onChange={(e) => setInput(e.target.value)}
            placeholder="Nhập tin nhắn..."
            disabled={isTyping}
            className="flex-1 bg-[#1E293B] text-white placeholder:text-[#C9A961]/50 rounded-xl px-4 py-2.5 text-sm border border-[#C9A961]/20 focus:outline-none focus:border-[#C9A961] disabled:opacity-50"
          />
          <button
            type="submit"
            disabled={!input.trim() || isTyping}
            className="bg-[#C9A961] text-[#2C3E50] rounded-xl p-3 hover:bg-[#D4AF37] disabled:opacity-50"
          >
            <Send className="w-5 h-5" />
          </button>
        </div>
      </form>
    </div>

    {/* Toggle Button (luôn hiển thị) */}
    <button
      type="button"
      onClick={() => setIsOpen((prev) => !prev)}
      className="w-14 h-14 rounded-full bg-[#C9A961] text-[#2C3E50] shadow-lg hover:bg-[#D4AF37] transition-all flex items-center justify-center"
    >
      {isOpen ? (
        <X className="w-7 h-7" />
      ) : (
        <MessageCircle className="w-7 h-7" />
      )}
    </button>
  </div>
);

}