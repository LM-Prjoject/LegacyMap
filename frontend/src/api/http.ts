import axios from 'axios'
import { supabase } from '@/lib/supabaseClient'

export const http = axios.create({ baseURL: import.meta.env.VITE_API_BASE_URL })

http.interceptors.request.use(async (config) => {
    const { data } = await supabase.auth.getSession()
    const token = data.session?.access_token
    if (token) config.headers.Authorization = `Bearer ${token}`
    return config
})
