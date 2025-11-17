-- Bật RLS
ALTER TABLE chat_room_branches ENABLE ROW LEVEL SECURITY;

-- SELECT: user chỉ được xem branches nếu là thành viên phòng
CREATE POLICY "users_select_chat_room_branches" ON chat_room_branches
FOR SELECT USING (
    EXISTS (
        SELECT 1 
        FROM chat_room_members m
        WHERE m.room_id = chat_room_branches.room_id
          AND m.user_id = auth.uid()
    )
);

-- INSERT: user chỉ tạo branch cho phòng họ là thành viên
CREATE POLICY "users_insert_chat_room_branches" ON chat_room_branches
FOR INSERT
WITH CHECK (
    EXISTS (
        SELECT 1 
        FROM chat_room_members m
        WHERE m.room_id = chat_room_branches.room_id
          AND m.user_id = auth.uid()
    )
);

-- UPDATE: nếu muốn cho phép chỉnh sửa branch, chỉ thành viên phòng mới được update
CREATE POLICY "users_update_chat_room_branches" ON chat_room_branches
FOR UPDATE USING (
    EXISTS (
        SELECT 1 
        FROM chat_room_members m
        WHERE m.room_id = chat_room_branches.room_id
          AND m.user_id = auth.uid()
    )
);

-- DELETE: chỉ thành viên phòng mới được xoá branch
CREATE POLICY "users_delete_chat_room_branches" ON chat_room_branches
FOR DELETE USING (
    EXISTS (
        SELECT 1 
        FROM chat_room_members m
        WHERE m.room_id = chat_room_branches.room_id
          AND m.user_id = auth.uid()
    )
);
