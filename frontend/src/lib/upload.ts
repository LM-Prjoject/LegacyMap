import { createClient } from '@supabase/supabase-js';

/**
 * ✅ Khởi tạo Supabase client dùng cho toàn app
 */
const supabaseUrl = import.meta.env.VITE_SUPABASE_URL!;
const supabaseAnonKey = import.meta.env.VITE_SUPABASE_ANON_KEY!;
export const supabase = createClient(supabaseUrl, supabaseAnonKey);

/**
 * ✅ Upload ảnh bìa (cover) lên Supabase
 * @param file - file ảnh người dùng chọn
 * @returns public URL của ảnh vừa upload
 */
export async function uploadCoverToSupabase(file: File): Promise<string> {
    const safeName = file.name.replace(/\s+/g, '_'); // tránh lỗi tên file có dấu cách
    const path = `covers/${crypto.randomUUID()}-${safeName}`;

    const { error } = await supabase.storage.from('legacymap').upload(path, file, {
        cacheControl: '3600', // cache trong 1h
        upsert: false,        // tạo file mới, không ghi đè
    });

    if (error) {
        console.error('Upload cover failed:', error);
        throw new Error('Không thể tải ảnh bìa lên Supabase.');
    }

    const { data } = supabase.storage.from('legacymap').getPublicUrl(path);
    if (!data?.publicUrl) {
        throw new Error('Không thể lấy URL ảnh bìa công khai.');
    }

    return data.publicUrl;
}

/**
 * ✅ Upload avatar người dùng lên Supabase
 * - Lưu trong thư mục `profiles/` của bucket `legacymap`
 * - Cho phép ghi đè avatar cũ để tiết kiệm dung lượng
 * @param file - file ảnh avatar
 * @returns public URL của avatar
 */
export async function uploadAvatarToSupabase(file: File): Promise<string> {
    const ext = file.name.split('.').pop()?.toLowerCase() || 'jpg';
    const filename = `${crypto.randomUUID()}.${ext}`;
    const path = `profiles/${filename}`;

    const { error } = await supabase.storage.from('legacymap').upload(path, file, {
        cacheControl: '3600',
        upsert: true, // ✅ cho phép ghi đè avatar cũ nếu user đổi ảnh
    });

    if (error) {
        console.error('Upload avatar failed:', error);
        throw new Error('Không thể tải ảnh đại diện lên Supabase.');
    }

    const { data } = supabase.storage.from('legacymap').getPublicUrl(path);
    if (!data?.publicUrl) {
        throw new Error('Không thể lấy URL ảnh đại diện công khai.');
    }

    return data.publicUrl;
}