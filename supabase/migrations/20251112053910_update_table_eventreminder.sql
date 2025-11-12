ALTER TABLE event_reminders
ADD COLUMN IF NOT EXISTS recipient_type VARCHAR(10),
ADD COLUMN IF NOT EXISTS recipient_id UUID;

UPDATE event_reminders
SET 
  recipient_type = 'user',
  recipient_id = user_id
WHERE user_id IS NOT NULL;

-- Xóa RLS policy phụ thuộc user_id
DROP POLICY IF EXISTS "Users see own reminders" ON public.event_reminders;
DROP POLICY IF EXISTS "Users can insert reminders for own events" ON public.event_reminders;
DROP POLICY IF EXISTS "Users can update own reminders" ON public.event_reminders;
DROP POLICY IF EXISTS "Users can delete own reminders" ON public.event_reminders;

ALTER TABLE event_reminders
ALTER COLUMN recipient_type SET NOT NULL,
ALTER COLUMN recipient_id SET NOT NULL,
ADD CONSTRAINT chk_recipient_type CHECK (recipient_type IN ('user', 'person'));

ALTER TABLE event_reminders
DROP CONSTRAINT IF EXISTS event_reminders_user_id_fkey,
DROP COLUMN IF EXISTS user_id;

CREATE INDEX IF NOT EXISTS idx_event_reminders_recipient 
ON event_reminders(recipient_type, recipient_id);

-- Người dùng chỉ thấy nhắc nhở của mình
CREATE POLICY "Users see own reminders"
ON public.event_reminders FOR SELECT
USING (
    recipient_type = 'user' AND recipient_id = auth.uid()
);

-- Người dùng chỉ tạo nhắc nhở cho sự kiện họ có quyền
CREATE POLICY "Users can insert reminders for own events"
ON public.event_reminders FOR INSERT
WITH CHECK (
    recipient_type = 'user'
    AND recipient_id = auth.uid()
    AND EXISTS (
        SELECT 1 FROM public.events e
        WHERE e.id = event_id
          AND (
              e.personal_owner_id = auth.uid()
              OR e.created_by = auth.uid()
              OR (
                  e.family_tree_id IS NOT NULL
                  AND EXISTS (
                      SELECT 1 FROM public.tree_access ta
                      WHERE ta.family_tree_id = e.family_tree_id
                        AND ta.user_id = auth.uid()
                        AND ta.access_level IN ('edit','admin')
                  )
              )
          )
    )
);

-- Người dùng chỉ update hoặc xóa nhắc nhở của chính mình
CREATE POLICY "Users can update own reminders"
ON public.event_reminders FOR UPDATE
USING (recipient_type = 'user' AND recipient_id = auth.uid());

CREATE POLICY "Users can delete own reminders"
ON public.event_reminders FOR DELETE
USING (recipient_type = 'user' AND recipient_id = auth.uid());