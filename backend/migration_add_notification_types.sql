-- Migration: Add new notification types to database constraint
-- Run this SQL in your PostgreSQL database

-- Drop the existing constraint
ALTER TABLE notifications DROP CONSTRAINT IF EXISTS notifications_type_check;

-- Add new constraint with additional types
ALTER TABLE notifications ADD CONSTRAINT notifications_type_check 
CHECK (type::text = ANY (ARRAY[
    'system'::character varying,
    'update'::character varying,
    'invite'::character varying,
    'alert'::character varying,
    'event_reminder'::character varying,
    'access_request'::character varying,
    'access_granted'::character varying,
    'edit_request'::character varying
]::text[]));
