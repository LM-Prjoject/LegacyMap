import { createClient } from '@supabase/supabase-js'

const supabaseUrl = import.meta.env.VITE_SUPABASE_URL
const supabaseAnonKey = import.meta.env.VITE_SUPABASE_ANON_KEY

// Debug: Log để kiểm tra
console.log('🔧 Supabase Config Check:', {
    hasUrl: !!supabaseUrl,
    hasKey: !!supabaseAnonKey,
    url: supabaseUrl ? '✅ Set' : '❌ Missing',
    key: supabaseAnonKey ? '✅ Set' : '❌ Missing'
})

if (!supabaseUrl || !supabaseAnonKey) {
    throw new Error(`
    ❌ Supabase environment variables missing!
    VITE_SUPABASE_URL: ${supabaseUrl ? '✅' : '❌'}
    VITE_SUPABASE_ANON_KEY: ${supabaseAnonKey ? '✅' : '❌'}
    
    Please check your .env file in frontend directory!
  `)
}

export const supabase = createClient(supabaseUrl, supabaseAnonKey, {
    auth: {
        persistSession: true,
        autoRefreshToken: true,
        detectSessionInUrl: true
    },
    realtime: {
        params: {
            eventsPerSecond: 10
        }
    }
})

// Sửa test function để tránh lỗi
export const testSupabaseConnection = async () => {
    try {
        // Test với query đơn giản hơn
        const { data, error } = await supabase.from('users').select('id').limit(1)

        if (error) {
            console.log('❌ Supabase Connection Test Failed:', error)
            return { success: false, error, data: null }
        }

        console.log('✅ Supabase Connection Test Successful:', data)
        return { success: true, error: null, data }
    } catch (err: any) {
        console.error('💥 Supabase Test Failed:', err)
        return { success: false, error: err, data: null }
    }
}