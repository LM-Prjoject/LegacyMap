import { supabase, testSupabaseConnection } from '@/lib/supabaseClient'

export const databaseApi = {
    // Test connection
    testConnection: testSupabaseConnection,

    // Get all tables
    async getTables() {
        const { data, error } = await supabase
            .from('information_schema.tables')
            .select('table_name')
            .eq('table_schema', 'public')

        return { data, error }
    },

    // Get users - Sửa query để tránh lỗi
    async getUsers() {
        try {
            const { data, error } = await supabase
                .from('users')
                .select('*')
                .limit(10)

            console.log('👥 Users data:', data)
            return { data, error }
        } catch (err) {
            console.error('❌ Error fetching users:', err)
            return { data: null, error: err }
        }
    },

    // Get user_profiles
    async getUserProfiles() {
        try {
            const { data, error } = await supabase
                .from('user_profiles')
                .select('*')
                .limit(10)

            console.log('📋 User profiles:', data)
            return { data, error }
        } catch (err) {
            console.error('❌ Error fetching user profiles:', err)
            return { data: null, error: err }
        }
    },

    // Get family trees
    async getFamilyTrees() {
        try {
            const { data, error } = await supabase
                .from('family_trees')
                .select('*')
                .limit(10)

            console.log('🌳 Family trees:', data)
            return { data, error }
        } catch (err) {
            console.error('❌ Error fetching family trees:', err)
            return { data: null, error: err }
        }
    }
}