-- =========================================
--  DROP ALL OBJECTS: Family Tree App Schema
--  (Clean Supabase/Postgres before re-run DDL)
-- =========================================

-- Disable RLS to avoid dependency errors
alter table if exists public.profiles disable row level security;
alter table if exists public.family_trees disable row level security;
alter table if exists public.tree_collaborators disable row level security;
alter table if exists public.tree_members disable row level security;
alter table if exists public.relationships disable row level security;
alter table if exists public.share_links disable row level security;
alter table if exists public.audit_logs disable row level security;

-- ================================
-- Drop triggers
-- ================================
drop trigger if exists trg_profiles_updated_at on public.profiles;
drop trigger if exists on_auth_user_created on auth.users;
drop trigger if exists trg_family_trees_updated_at on public.family_trees;
drop trigger if exists trg_tree_members_updated_at on public.tree_members;

-- ================================
-- Drop functions
-- ================================
drop function if exists public.set_updated_at cascade;
drop function if exists public.handle_new_user cascade;
drop function if exists public.is_tree_viewer cascade;
drop function if exists public.is_tree_editor cascade;

-- ================================
-- Drop tables (in dependency order)
-- ================================
drop table if exists public.audit_logs cascade;
drop table if exists public.share_links cascade;
drop table if exists public.relationships cascade;
drop table if exists public.tree_members cascade;
drop table if exists public.tree_collaborators cascade;
drop table if exists public.family_trees cascade;
drop table if exists public.profiles cascade;

-- ================================
-- Drop ENUM types
-- ================================
drop type if exists relationship_type cascade;
drop type if exists collab_role cascade;
drop type if exists visibility_level cascade;
drop type if exists gender_type cascade;

-- ================================
-- Drop indexes (optional, cascade covers)
-- ================================
-- You can skip explicit index drops because `cascade` above removes them.

-- ================================
-- Clean up
-- ================================
-- Check remaining objects (for verification)
-- select * from pg_tables where schemaname='public';
-- select * from pg_type where typcategory='E';

comment on schema public is 'Cleaned Family Tree schema — ready to recreate.';
