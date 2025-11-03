-- DROP các policy cũ
DROP POLICY IF EXISTS "Events visible to tree members" ON public.events;
DROP POLICY IF EXISTS "Events editable by editors" ON public.events;
DROP POLICY IF EXISTS "Users see own reminders" ON public.event_reminders;

-- ALTER bảng events
ALTER TABLE events
ALTER COLUMN family_tree_id DROP NOT NULL;

ALTER TABLE events
ADD COLUMN personal_owner_id UUID REFERENCES users(id);

ALTER TABLE events
ADD CONSTRAINT chk_event_type
CHECK (
    (family_tree_id IS NOT NULL AND personal_owner_id IS NULL)
    OR
    (family_tree_id IS NULL AND personal_owner_id IS NOT NULL)
);

-- Xem sự kiện cá nhân hoặc trong cây
CREATE POLICY "Users can view personal or tree events"
ON public.events FOR SELECT
USING (
    personal_owner_id = auth.uid()
    OR created_by = auth.uid()
    OR (
        family_tree_id IS NOT NULL
        AND EXISTS (
            SELECT 1 FROM public.family_trees t
            LEFT JOIN public.tree_access ta
              ON ta.family_tree_id = t.id
            WHERE t.id = family_tree_id
              AND (t.created_by = auth.uid() OR ta.user_id = auth.uid())
        )
    )
);

-- Thêm sự kiện cá nhân hoặc trong cây
CREATE POLICY "Users can insert personal or tree events"
ON public.events FOR INSERT
WITH CHECK (
    (personal_owner_id = auth.uid() AND family_tree_id IS NULL)
    OR (
        family_tree_id IS NOT NULL
        AND EXISTS (
            SELECT 1 FROM public.family_trees t
            LEFT JOIN public.tree_access ta
              ON ta.family_tree_id = t.id
            WHERE t.id = family_tree_id
              AND (t.created_by = auth.uid() OR ta.user_id = auth.uid())
        )
        AND personal_owner_id IS NULL
    )
);

-- Cập nhật sự kiện cá nhân hoặc trong cây
CREATE POLICY "Users can update personal or tree events"
ON public.events FOR UPDATE
USING (
    personal_owner_id = auth.uid()
    OR created_by = auth.uid()
    OR (
        family_tree_id IS NOT NULL
        AND EXISTS (
            SELECT 1
            FROM public.tree_access ta
            WHERE ta.family_tree_id = family_tree_id
              AND ta.user_id = auth.uid()
              AND ta.access_level IN ('edit', 'admin')
        )
    )
);

-- Xóa sự kiện cá nhân hoặc trong cây
CREATE POLICY "Users can delete personal or tree events"
ON public.events FOR DELETE
USING (
    personal_owner_id = auth.uid()
    OR created_by = auth.uid()
    OR (
        family_tree_id IS NOT NULL
        AND EXISTS (
            SELECT 1
            FROM public.tree_access ta
            WHERE ta.family_tree_id = family_tree_id
              AND ta.user_id = auth.uid()
              AND ta.access_level IN ('edit', 'admin')
        )
    )
);

-- Người dùng chỉ thấy nhắc nhở của mình
CREATE POLICY "Users see own reminders"
ON public.event_reminders FOR SELECT
USING (user_id = auth.uid());

-- Người dùng chỉ tạo nhắc nhở cho sự kiện họ có quyền
CREATE POLICY "Users can insert reminders for own events"
ON public.event_reminders FOR INSERT
WITH CHECK (
    EXISTS (
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
USING (user_id = auth.uid());

CREATE POLICY "Users can delete own reminders"
ON public.event_reminders FOR DELETE
USING (user_id = auth.uid());
