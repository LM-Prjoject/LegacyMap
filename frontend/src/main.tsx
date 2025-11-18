import React from 'react'
import ReactDOM from 'react-dom/client'
import { RouterProvider } from 'react-router-dom'
import { router } from './routes/index.tsx'
import '../index.css'
import 'bootstrap/dist/css/bootstrap.min.css';
import { Toaster } from 'react-hot-toast'
import { EventProvider } from '@/contexts/EventContext'
import { ChatProvider } from '@/contexts/ChatContext'
import ChatWidget from '@/components/chat/ChatWidget'
import { useUserActivity } from './hooks/useUserActivity'  

function AppWithHeartbeat() {
    useUserActivity(); 
    return (
        <EventProvider>
            <ChatProvider>
                <RouterProvider router={router} />
                <ChatWidget />
                <Toaster
                    position="top-center"
                    reverseOrder={false}
                    containerStyle={{
                        zIndex: 999999,
                        top: 20,
                    }}
                    toastOptions={{
                        duration: 2500,
                        style: { borderRadius: '10px', background: '#fff', color: '#333' },
                    }}
                />
            </ChatProvider>
        </EventProvider>
    );
}

ReactDOM.createRoot(document.getElementById('root')!).render(
    <React.StrictMode>
        <AppWithHeartbeat /> 
    </React.StrictMode>,
)