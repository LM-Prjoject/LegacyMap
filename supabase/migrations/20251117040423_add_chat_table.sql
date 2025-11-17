CREATE TABLE chat_rooms (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    family_tree_id UUID REFERENCES family_trees(id) ON DELETE CASCADE,
    name VARCHAR(200) NOT NULL,
    description TEXT,
    room_type VARCHAR(20) NOT NULL CHECK (room_type IN ('family', 'branch', 'private')),
    is_active BOOLEAN DEFAULT TRUE,
    created_by UUID REFERENCES users(id) NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    CONSTRAINT chk_room_type_tree CHECK (
        (room_type IN ('family', 'branch') AND family_tree_id IS NOT NULL)
        OR (room_type = 'private' AND family_tree_id IS NULL)
    )
);

CREATE TABLE chat_room_members (
    room_id UUID REFERENCES chat_rooms(id) ON DELETE CASCADE,
    user_id UUID REFERENCES users(id) ON DELETE CASCADE,
    person_id UUID REFERENCES persons(id) ON DELETE CASCADE,
    role VARCHAR(20) DEFAULT 'member' CHECK (role IN ('member', 'admin', 'moderator')),
    joined_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    last_read_at TIMESTAMP WITH TIME ZONE,
    PRIMARY KEY (room_id, user_id)
);


CREATE TABLE messages (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    room_id UUID REFERENCES chat_rooms(id) ON DELETE CASCADE NOT NULL,
    sender_id UUID REFERENCES users(id) ON DELETE CASCADE NOT NULL,
    message_text TEXT,
    message_type VARCHAR(20) DEFAULT 'text' CHECK (message_type IN ('text','image','file','system')),
    file_url TEXT,
    file_name VARCHAR(255),
    file_size BIGINT,
    file_type VARCHAR(50),
    reply_to_id UUID REFERENCES messages(id) ON DELETE SET NULL,
    is_edited BOOLEAN DEFAULT FALSE,
    is_deleted BOOLEAN DEFAULT FALSE,
    deleted_at TIMESTAMP WITH TIME ZONE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

CREATE TABLE message_status (
    message_id UUID REFERENCES messages(id) ON DELETE CASCADE,
    user_id UUID REFERENCES users(id) ON DELETE CASCADE,
    is_read BOOLEAN DEFAULT FALSE,
    read_at TIMESTAMP WITH TIME ZONE,
    PRIMARY KEY (message_id, user_id)
);


CREATE TABLE chat_room_branches (
    room_id UUID REFERENCES chat_rooms(id) ON DELETE CASCADE,
    branch_person_id UUID REFERENCES persons(id) ON DELETE CASCADE,
    created_by UUID REFERENCES users(id),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    PRIMARY KEY (room_id, branch_person_id)
);


CREATE INDEX idx_messages_room_created ON messages(room_id, created_at DESC);
CREATE INDEX idx_messages_sender ON messages(sender_id);
CREATE INDEX idx_messages_reply_to ON messages(reply_to_id);

CREATE INDEX idx_message_status_user ON message_status(user_id);
CREATE INDEX idx_message_status_message ON message_status(message_id);

CREATE INDEX idx_chat_rooms_family ON chat_rooms(family_tree_id);

CREATE INDEX idx_chat_room_members_user ON chat_room_members(user_id);
CREATE INDEX idx_chat_room_members_room ON chat_room_members(room_id);

CREATE INDEX idx_chat_room_branches_room ON chat_room_branches(room_id);
CREATE INDEX idx_chat_room_branches_branch ON chat_room_branches(branch_person_id);



ALTER TABLE chat_rooms ENABLE ROW LEVEL SECURITY;
ALTER TABLE chat_room_members ENABLE ROW LEVEL SECURITY;
ALTER TABLE messages ENABLE ROW LEVEL SECURITY;
ALTER TABLE message_status ENABLE ROW LEVEL SECURITY;


-- Chat Rooms RLS Policies
-- SELECT
CREATE POLICY "users_select_rooms" ON chat_rooms
FOR SELECT USING (
    EXISTS (
        SELECT 1 FROM chat_room_members m
        WHERE m.room_id = chat_rooms.id
          AND m.user_id = auth.uid()
    )
);

-- INSERT
CREATE POLICY "users_insert_rooms" ON chat_rooms
FOR INSERT
WITH CHECK (created_by = auth.uid());

-- UPDATE
CREATE POLICY "users_update_rooms" ON chat_rooms
FOR UPDATE USING (
    EXISTS (
        SELECT 1 FROM chat_room_members m
        WHERE m.room_id = chat_rooms.id
          AND m.user_id = auth.uid()
    )
);

-- DELETE
CREATE POLICY "users_delete_rooms" ON chat_rooms
FOR DELETE USING (
    EXISTS (
        SELECT 1 FROM chat_room_members m
        WHERE m.room_id = chat_rooms.id
          AND m.user_id = auth.uid()
    )
);

-- Messages RLS Policies
-- SELECT
CREATE POLICY "users_select_messages" ON messages
FOR SELECT USING (
    EXISTS (
        SELECT 1 FROM chat_room_members m
        WHERE m.room_id = messages.room_id
          AND m.user_id = auth.uid()
    )
);

-- INSERT
CREATE POLICY "users_insert_messages" ON messages
FOR INSERT
WITH CHECK (
    EXISTS (
        SELECT 1 FROM chat_room_members m
        WHERE m.room_id = messages.room_id
          AND m.user_id = auth.uid()
    )
);

-- UPDATE
CREATE POLICY "users_update_messages" ON messages
FOR UPDATE USING (
    sender_id = auth.uid()
);

-- DELETE
CREATE POLICY "users_delete_messages" ON messages
FOR DELETE USING (
    sender_id = auth.uid()
);