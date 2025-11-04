import { createClient } from '@supabase/supabase-js';

const supabaseUrl = import.meta.env.VITE_SUPABASE_URL!;
const supabaseAnonKey = import.meta.env.VITE_SUPABASE_ANON_KEY!;
export const supabase = createClient(supabaseUrl, supabaseAnonKey);

function sanitizeFileName(name: string) {
    const dot = name.lastIndexOf('.');
    const base = dot >= 0 ? name.slice(0, dot) : name;
    const ext = dot >= 0 ? name.slice(dot).toLowerCase() : '';

    const cleanedBase = base
        .normalize('NFD')
        .replace(/[\u0300-\u036f]/g, '')
        .replace(/[^a-zA-Z0-9._-]/g, '_')
        .replace(/_+/g, '_')
        .replace(/^_+|_+$/g, '');

    return `${cleanedBase || 'file'}${ext}`;
}

export async function uploadCoverToSupabase(file: File): Promise<string> {
    const safeName = sanitizeFileName(file.name);
    const path = `covers/${crypto.randomUUID()}-${safeName}`;

    const { error } = await supabase.storage
        .from('legacymap')
        .upload(path, file, {
            upsert: true, // tránh lỗi trùng key nếu user chọn lại cùng tên
            contentType: file.type || 'application/octet-stream',
        });

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
        upsert: true,
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

export async function uploadMemberAvatarToSupabase(file: File): Promise<string> {
    const ext = file.name.split(".").pop()?.toLowerCase() || "jpg";
    const filename = `${crypto.randomUUID()}.${ext}`;
    const path = `members/${filename}`;

    const type = file.type && file.type.startsWith("image/") ? file.type : `image/${ext === "png" ? "png" : "jpeg"}`;

    const { error } = await supabase.storage
        .from("legacymap")
        .upload(path, file, {
            cacheControl: "3600",
            upsert: false,
            contentType: type,
        });

    if (error) {
        console.error("Upload member avatar failed:", error);
        throw new Error("Không thể tải ảnh thành viên lên Supabase.");
    }

    const { data } = supabase.storage.from("legacymap").getPublicUrl(path);
    if (!data?.publicUrl) throw new Error("Không thể lấy URL ảnh đại diện công khai của thành viên.");

    return data.publicUrl;
}
