import React from 'react'
import ReactDOM from 'react-dom/client'
import { RouterProvider } from 'react-router-dom'
import { router } from './routes/index.tsx'
import '../index.css'
import 'bootstrap/dist/css/bootstrap.min.css';
import { Toaster } from 'react-hot-toast'
import { EventProvider } from '@/contexts/EventContext'

ReactDOM.createRoot(document.getElementById('root')!).render(
    <React.StrictMode>
        <EventProvider>
            <RouterProvider router={router} />
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
        </EventProvider>
    </React.StrictMode>,
)