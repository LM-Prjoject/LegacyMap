-- 1. Tạo ENUM gender_type nếu chưa có
do $$
begin
  if not exists (select 1 from pg_type where typname = 'gender_type') then
    create type gender_type as enum ('MALE', 'FEMALE', 'OTHER', 'UNKNOWN');
  end if;
end$$;

-- 2. Dọn dẹp các field cũ trong profiles
alter table public.profiles
  drop column if exists display_name,
  drop column if exists avatar_url,
  drop column if exists birth_date,
  drop column if exists updated_at,
  drop column if exists created_at;

-- 3. Thay đổi và thêm mới các field cần thiết
alter table public.profiles
  -- đổi gender sang enum
  alter column gender drop default,
  alter column gender type gender_type using
    case
      when gender::text in ('true','1','MALE','male','Nam') then 'MALE'::gender_type
      when gender::text in ('false','0','FEMALE','female','Nữ') then 'FEMALE'::gender_type
      when gender::text in ('OTHER','other') then 'OTHER'::gender_type
      else 'UNKNOWN'::gender_type
    end,
  alter column gender set default 'UNKNOWN',

  -- thêm các field mới
  add column if not exists username text unique,
  add column if not exists password text,            add column if not exists fullName text,
  add column if not exists email text unique,
  add column if not exists clanName text,
  add column if not exists phone text,
  add column if not exists dob date,
  add column if not exists city text,
  add column if not exists province text,
  add column if not exists houseNumber text;
