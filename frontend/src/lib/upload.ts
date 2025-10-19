import { createClient } from '@supabase/supabase-js';

const supabaseUrl = import.meta.env.VITE_SUPABASE_URL!;
const supabaseAnonKey = import.meta.env.VITE_SUPABASE_ANON_KEY!;
const supabase = createClient(supabaseUrl, supabaseAnonKey);

export async function uploadCoverToSupabase(file: File): Promise<string> {
    const path = `covers/${crypto.randomUUID()}-${file.name}`;
    const { error } = await supabase.storage.from('legacymap').upload(path, file);
    if (error) throw new Error(error.message);
    const { data } = supabase.storage.from('legacymap').getPublicUrl(path);
    return data.publicUrl;
}
export async function uploadAvatarToSupabase(file: File): Promise<string> {
    const ext = file.name.split('.').pop()?.toLowerCase() || 'jpg';
    const filename = `${crypto.randomUUID()}.${ext}`;
    const path = `profiles/${filename}`;

    const { error } = await supabase.storage.from('legacymap').upload(path, file, {
        cacheControl: '3600',
        upsert: false,
    });

    if (error) throw new Error(error.message);

    const { data } = supabase.storage.from('legacymap').getPublicUrl(path);
    return data.publicUrl;
}