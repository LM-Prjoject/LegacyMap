// src/components/DebugAuthInfo.tsx
import React from 'react';

const DebugAuthInfo: React.FC = () => {
    const token = localStorage.getItem('authToken');
    const userStr = localStorage.getItem('user');

    let user = null;
    try {
        user = userStr ? JSON.parse(userStr) : null;
    } catch (e) {
        console.error('Error parsing user:', e);
    }

    const testAdminAccess = async () => {
        if (!token) {
            console.error('❌ No token found');
            return;
        }

        console.log('🧪 Testing Admin Access...');

        try {
            // Test proxy connection
            const response = await fetch('/api/admin/users', {
                headers: {
                    'Authorization': `Bearer ${token}`,
                    'Content-Type': 'application/json',
                },
            });

            console.log('🔍 Proxy Test - Status:', response.status);

            if (!response.ok) {
                const errorText = await response.text();
                console.error('🔍 Proxy Test - Error:', errorText);
            } else {
                const data = await response.json();
                console.log('🔍 Proxy Test - Success:', data);
            }
        } catch (error) {
            console.error('🔍 Proxy Test - Exception:', error);
        }
    };

    return (
        <div style={{
            padding: '10px',
            background: '#f0f0f0',
            margin: '10px',
            border: '1px solid #ccc',
            borderRadius: '5px',
            fontSize: '14px'
        }}>
            <h3 style={{ margin: '0 0 10px 0' }}>🔍 Auth Debug Info</h3>
            <p><strong>Token:</strong> {token ? `✅ Present (${token.length} chars)` : '❌ Missing'}</p>
            <p><strong>User:</strong> {user ? '✅ Present' : '❌ Missing'}</p>
            <p><strong>Role:</strong> {user?.role || user?.roleName || '❌ Unknown'}</p>
            <p><strong>User ID:</strong> {user?.id || user?.userId || '❌ Unknown'}</p>

            <div style={{ marginTop: '10px' }}>
                <button
                    onClick={testAdminAccess}
                    style={{
                        padding: '5px 10px',
                        margin: '5px',
                        backgroundColor: '#007bff',
                        color: 'white',
                        border: 'none',
                        borderRadius: '3px',
                        cursor: 'pointer'
                    }}
                >
                    Test Admin Access
                </button>

                <button
                    onClick={() => {
                        localStorage.removeItem('authToken');
                        localStorage.removeItem('user');
                        window.location.reload();
                    }}
                    style={{
                        padding: '5px 10px',
                        margin: '5px',
                        backgroundColor: '#ff4444',
                        color: 'white',
                        border: 'none',
                        borderRadius: '3px',
                        cursor: 'pointer'
                    }}
                >
                    Clear Auth Data
                </button>
            </div>
        </div>
    );
};

export default DebugAuthInfo;