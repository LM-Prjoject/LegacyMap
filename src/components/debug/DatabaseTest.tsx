import { useState, useEffect } from 'react'
import { databaseApi } from '@/api/database'

// Định nghĩa interface cho error
interface DatabaseError {
    message: string;
}

export default function DatabaseTest() {
    const [connectionStatus, setConnectionStatus] = useState<'testing' | 'connected' | 'error'>('testing')
    const [data, setData] = useState<any>(null)
    const [error, setError] = useState<string>('')

    useEffect(() => {
        testDatabase()
    }, [])

    const testDatabase = async () => {
        try {
            setConnectionStatus('testing')

            // Test connection
            const connectionResult = await databaseApi.testConnection()
            console.log('🔗 Connection result:', connectionResult)

            if (!connectionResult.success) {
                const errorMsg = (connectionResult.error as DatabaseError)?.message || 'Connection failed'
                throw new Error(errorMsg)
            }

            // Get users data
            const usersResult = await databaseApi.getUsers()
            console.log('👥 Users result:', usersResult)

            if (usersResult.error) {
                const errorMsg = (usersResult.error as DatabaseError).message || 'Failed to get users'
                throw new Error(errorMsg)
            }

            setData(usersResult.data)
            setConnectionStatus('connected')

        } catch (err) {
            console.error('💥 Database test failed:', err)
            const errorMessage = err instanceof Error ? err.message : 'Unknown error occurred'
            setError(errorMessage)
            setConnectionStatus('error')
        }
    }

    return (
        <div className="p-6 bg-white rounded-lg shadow-md">
            <h2 className="text-xl font-bold mb-4">Database Connection Test</h2>

            <div className="mb-4">
                <span className={`font-semibold ${
                    connectionStatus === 'connected' ? 'text-green-600' :
                        connectionStatus === 'error' ? 'text-red-600' : 'text-yellow-600'
                }`}>
                    Status: {connectionStatus.toUpperCase()}
                </span>
            </div>

            {error && (
                <div className="mb-4 p-3 bg-red-100 border border-red-400 text-red-700 rounded">
                    <strong>Error:</strong> {error}
                </div>
            )}

            {data && (
                <div className="mb-4">
                    <h3 className="font-semibold mb-2">Users Data ({data.length} records):</h3>
                    <pre className="bg-gray-100 p-3 rounded text-sm overflow-auto">
                        {JSON.stringify(data, null, 2)}
                    </pre>
                </div>
            )}

            <button
                onClick={testDatabase}
                className="px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700"
            >
                Test Connection Again
            </button>
        </div>
    )
}