-- =========================================
--  Family Tree App - Supabase/Postgres DDL
-- =========================================

-- Extensions (Supabase thường đã bật pgcrypto)
create extension if not exists pgcrypto;

-- =================
-- ENUM definitions
-- =================
do $$
begin
  if not exists (select 1 from pg_type where typname = 'relationship_type') then
    create type relationship_type as enum ('PARENT_OF','SPOUSE');
  end if;

  if not exists (select 1 from pg_type where typname = 'collab_role') then
    create type collab_role as enum ('OWNER','EDITOR','VIEWER');
  end if;

  if not exists (select 1 from pg_type where typname = 'visibility_level') then
    create type visibility_level as enum ('PRIVATE','LINK');
  end if;

  if not exists (select 1 from pg_type where typname = 'gender_type') then
    create type gender_type as enum ('MALE','FEMALE','OTHER','UNKNOWN');
  end if;
end$$;

-- ============================
-- Helper: updated_at trigger
-- ============================
create or replace function public.set_updated_at()
returns trigger
language plpgsql
as $$
begin
  new.updated_at = now();
  return new;
end;
$$;

-- =================================================
-- profiles (1-1 với auth.users), mở rộng thông tin
-- =================================================
create table if not exists public.profiles (
  id uuid primary key references auth.users(id) on delete cascade,
  display_name text not null default '',
  avatar_url text,
  gender gender_type,
  birth_date date,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);

create trigger trg_profiles_updated_at
before update on public.profiles
for each row execute function public.set_updated_at();

-- Auto insert profile khi có user mới
create or replace function public.handle_new_user()
returns trigger
language plpgsql
security definer
set search_path = public
as $$
begin
  if not exists (select 1 from public.profiles where id = new.id) then
    insert into public.profiles(id, display_name)
    values (new.id, coalesce(new.raw_user_meta_data->>'name',''));
  end if;
  return new;
end;
$$;

drop trigger if exists on_auth_user_created on auth.users;
create trigger on_auth_user_created
after insert on auth.users
for each row execute function public.handle_new_user();

-- ===========================================
-- family_trees: mỗi gia phả do 1 owner sở hữu
-- ===========================================
create table if not exists public.family_trees (
  id uuid primary key default gen_random_uuid(),
  owner_id uuid not null references public.profiles(id) on delete cascade,
  name text not null,
  description text,
  visibility visibility_level not null default 'PRIVATE',
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);

create index if not exists idx_family_trees_owner on public.family_trees(owner_id);
create index if not exists idx_family_trees_name on public.family_trees using gin (to_tsvector('simple', name));

create trigger trg_family_trees_updated_at
before update on public.family_trees
for each row execute function public.set_updated_at();

-- =======================================================
-- tree_collaborators: cộng tác theo vai trò (optional+)
-- =======================================================
create table if not exists public.tree_collaborators (
  tree_id uuid not null references public.family_trees(id) on delete cascade,
  user_id uuid not null references public.profiles(id) on delete cascade,
  role collab_role not null default 'VIEWER',
  created_at timestamptz not null default now(),
  primary key (tree_id, user_id)
);

create index if not exists idx_collab_user on public.tree_collaborators(user_id);

-- ==================================
-- tree_members: các thành viên trong cây
-- ==================================
create table if not exists public.tree_members (
  id uuid primary key default gen_random_uuid(),
  tree_id uuid not null references public.family_trees(id) on delete cascade,
  name text not null,
  gender gender_type,
  birth_date date,
  death_date date,
  avatar_url text,
  notes text,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  constraint chk_life_dates check (death_date is null or (birth_date is null or death_date >= birth_date))
);

create index if not exists idx_members_tree on public.tree_members(tree_id);
create index if not exists idx_members_name on public.tree_members using gin (to_tsvector('simple', name));

create trigger trg_tree_members_updated_at
before update on public.tree_members
for each row execute function public.set_updated_at();

-- ====================================
-- relationships: PARENT_OF (directed), SPOUSE (undirected)
-- ====================================
create table if not exists public.relationships (
  id uuid primary key default gen_random_uuid(),
  tree_id uuid not null references public.family_trees(id) on delete cascade,
  person_a_id uuid not null references public.tree_members(id) on delete cascade,
  person_b_id uuid not null references public.tree_members(id) on delete cascade,
  type relationship_type not null,
  start_date date,
  end_date date,
  created_at timestamptz not null default now(),
  constraint chk_people_not_equal check (person_a_id <> person_b_id)
);

create index if not exists idx_rel_tree on public.relationships(tree_id);
create index if not exists idx_rel_people on public.relationships(person_a_id, person_b_id);

-- Unique spouse pair regardless of order
create unique index if not exists uq_spouse_pair
on public.relationships (
  tree_id,
  type,
  LEAST(person_a_id, person_b_id),
  GREATEST(person_a_id, person_b_id)
)
where type = 'SPOUSE';

-- Optional: parent-of duplicate guard (same directed edge only once)
create unique index if not exists uq_parent_of_edge
on public.relationships (tree_id, type, person_a_id, person_b_id)
where type = 'PARENT_OF';

-- Note: chặn vòng lặp tổ tiên cần kiểm tra ở tầng service (graph cycle detection).

-- =====================
-- share_links: link xem
-- =====================
create table if not exists public.share_links (
  id uuid primary key default gen_random_uuid(),
  tree_id uuid not null references public.family_trees(id) on delete cascade,
  token uuid not null unique default gen_random_uuid(),
  role text not null default 'VIEWER',
  expire_at timestamptz,
  revoked_at timestamptz,
  created_at timestamptz not null default now()
);

create index if not exists idx_share_tree on public.share_links(tree_id);

-- ======================
-- audit_logs: lịch sử
-- ======================
create table if not exists public.audit_logs (
  id uuid primary key default gen_random_uuid(),
  actor_id uuid references public.profiles(id) on delete set null,
  tree_id uuid not null references public.family_trees(id) on delete cascade,
  action text not null,
  target_type text not null,
  target_id uuid,
  diff_json jsonb,
  created_at timestamptz not null default now()
);

create index if not exists idx_audit_tree_time on public.audit_logs(tree_id, created_at desc);

-- ==================================================
-- Helper functions for RLS: kiểm tra quyền theo cây
-- ==================================================
create or replace function public.is_tree_viewer(p_tree_id uuid)
returns boolean
language sql
stable
security definer
set search_path = public
as $$
  select exists (
    select 1
    from public.family_trees t
    where t.id = p_tree_id
      and (
        t.owner_id = auth.uid()
        or exists (
          select 1 from public.tree_collaborators c
          where c.tree_id = t.id and c.user_id = auth.uid()
        )
      )
  );
$$;

create or replace function public.is_tree_editor(p_tree_id uuid)
returns boolean
language sql
stable
security definer
set search_path = public
as $$
  select exists (
    select 1
    from public.family_trees t
    where t.id = p_tree_id
      and (
        t.owner_id = auth.uid()
        or exists (
          select 1 from public.tree_collaborators c
          where c.tree_id = t.id
            and c.user_id = auth.uid()
            and c.role in ('OWNER','EDITOR')
        )
      )
  );
$$;

-- ============================
-- Enable RLS + Policies
-- ============================
alter table public.profiles enable row level security;
alter table public.family_trees enable row level security;
alter table public.tree_collaborators enable row level security;
alter table public.tree_members enable row level security;
alter table public.relationships enable row level security;
alter table public.share_links enable row level security;
alter table public.audit_logs enable row level security;

-- profiles: user chỉ xem/sửa chính mình
drop policy if exists "profiles_select_own" on public.profiles;
create policy "profiles_select_own"
on public.profiles for select
to authenticated
using (id = auth.uid());

drop policy if exists "profiles_update_own" on public.profiles;
create policy "profiles_update_own"
on public.profiles for update
to authenticated
using (id = auth.uid())
with check (id = auth.uid());

-- family_trees
drop policy if exists "trees_select_viewable" on public.family_trees;
create policy "trees_select_viewable"
on public.family_trees for select
to authenticated
using (
  owner_id = auth.uid()
  or exists (select 1 from public.tree_collaborators c where c.tree_id = id and c.user_id = auth.uid())
);

drop policy if exists "trees_insert_owner" on public.family_trees;
create policy "trees_insert_owner"
on public.family_trees for insert
to authenticated
with check (owner_id = auth.uid());

drop policy if exists "trees_update_editor" on public.family_trees;
create policy "trees_update_editor"
on public.family_trees for update
to authenticated
using (public.is_tree_editor(id))
with check (public.is_tree_editor(id));

drop policy if exists "trees_delete_owner" on public.family_trees;
create policy "trees_delete_owner"
on public.family_trees for delete
to authenticated
using (owner_id = auth.uid());

-- collaborators: chỉ owner quản lý; user tự xem membership của mình
drop policy if exists "collab_select_related" on public.tree_collaborators;
create policy "collab_select_related"
on public.tree_collaborators for select
to authenticated
using (
  exists (select 1 from public.family_trees t where t.id = tree_id and t.owner_id = auth.uid())
  or user_id = auth.uid()
);

drop policy if exists "collab_upsert_owner" on public.tree_collaborators;
create policy "collab_upsert_owner"
on public.tree_collaborators for all
to authenticated
using (exists (select 1 from public.family_trees t where t.id = tree_id and t.owner_id = auth.uid()))
with check (exists (select 1 from public.family_trees t where t.id = tree_id and t.owner_id = auth.uid()));

-- tree_members
drop policy if exists "members_select_viewer" on public.tree_members;
create policy "members_select_viewer"
on public.tree_members for select
to authenticated
using (public.is_tree_viewer(tree_id));

drop policy if exists "members_cud_editor" on public.tree_members;
create policy "members_cud_editor"
on public.tree_members for all
to authenticated
using (public.is_tree_editor(tree_id))
with check (public.is_tree_editor(tree_id));

-- relationships
drop policy if exists "rels_select_viewer" on public.relationships;
create policy "rels_select_viewer"
on public.relationships for select
to authenticated
using (public.is_tree_viewer(tree_id));

drop policy if exists "rels_cud_editor" on public.relationships;
create policy "rels_cud_editor"
on public.relationships for all
to authenticated
using (public.is_tree_editor(tree_id))
with check (public.is_tree_editor(tree_id));

-- share_links (quản lý bởi editor/owner)
drop policy if exists "shares_select_viewer" on public.share_links;
create policy "shares_select_viewer"
on public.share_links for select
to authenticated
using (public.is_tree_viewer(tree_id));

drop policy if exists "shares_cud_editor" on public.share_links;
create policy "shares_cud_editor"
on public.share_links for all
to authenticated
using (public.is_tree_editor(tree_id))
with check (public.is_tree_editor(tree_id));

-- audit_logs: chỉ viewer của tree xem, hệ thống ghi bằng service role
drop policy if exists "audit_select_viewer" on public.audit_logs;
create policy "audit_select_viewer"
on public.audit_logs for select
to authenticated
using (public.is_tree_viewer(tree_id));

-- ============================
-- Useful partial indexes
-- ============================
create index if not exists idx_members_living on public.tree_members(tree_id) where death_date is null;
create index if not exists idx_shares_valid on public.share_links(tree_id) where revoked_at is null;
