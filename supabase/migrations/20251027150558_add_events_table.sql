-- Bảng Family_Events
CREATE TABLE events (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    family_tree_id UUID REFERENCES family_trees(id) ON DELETE CASCADE NOT NULL,
    created_by UUID REFERENCES users(id) NOT NULL,

    -- Nội dung sự kiện
    title VARCHAR(200) NOT NULL,
    description TEXT,
    event_type VARCHAR(50) NOT NULL CHECK (event_type IN (
        'death_anniversary', 'wedding_anniversary', 'birthday',
        'funeral', 'wedding', 'family_reunion', 'ceremony', 'other'
    )),

    -- Thời gian (có thể là âm hoặc dương)
    start_date TIMESTAMP WITH TIME ZONE NOT NULL,
    end_date TIMESTAMP WITH TIME ZONE,
    is_full_day BOOLEAN DEFAULT FALSE,
    calendar_type VARCHAR(10) DEFAULT 'solar' CHECK (calendar_type IN ('solar', 'lunar')), -- Dương/Lịch âm

    -- Tái diễn
    is_recurring BOOLEAN DEFAULT FALSE,
    recurrence_rule VARCHAR(20) DEFAULT 'YEARLY' CHECK (recurrence_rule IN ('YEARLY', 'MONTHLY', 'NONE')),

    -- Liên kết người liên quan
    related_persons JSONB, -- [{id:'uuid', name:'...'}, ...]

    -- Địa điểm
    location VARCHAR(300),
    location_coordinates JSONB, -- {lat:..., lng:...}

    -- Nhắc nhở (gộp vào JSON)
    reminder JSONB DEFAULT '{"days_before": 3, "methods": ["notification"]}'::jsonb,
    -- Ví dụ: {"days_before":7, "methods":["email","notification"]}

    -- Trạng thái hiển thị
    is_public BOOLEAN DEFAULT TRUE,
    status VARCHAR(20) DEFAULT 'active' CHECK (status IN ('active', 'cancelled', 'completed')),

    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Bảng quản lý các lịch nhắc thực tế 
CREATE TABLE event_reminders (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    event_id UUID REFERENCES events(id) ON DELETE CASCADE NOT NULL,
    user_id UUID REFERENCES users(id) ON DELETE CASCADE NOT NULL,

    send_method VARCHAR(20) NOT NULL CHECK (send_method IN ('notification', 'email', 'both')),
    scheduled_at TIMESTAMP WITH TIME ZONE NOT NULL,
    sent_at TIMESTAMP WITH TIME ZONE,
    status VARCHAR(20) DEFAULT 'pending' CHECK (status IN ('pending', 'sent', 'failed')),

    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

CREATE INDEX idx_event_reminders_schedule 
ON event_reminders(scheduled_at) WHERE status = 'pending';

-- RLS
ALTER TABLE events ENABLE ROW LEVEL SECURITY;
ALTER TABLE event_reminders ENABLE ROW LEVEL SECURITY;

-- Thành viên cây được xem sự kiện
CREATE POLICY "Events visible to tree members"
ON events FOR SELECT
USING (
    EXISTS (
        SELECT 1 FROM family_trees t
        JOIN tree_access ta ON ta.family_tree_id = t.id
        WHERE events.family_tree_id = t.id
        AND (t.created_by = auth.uid() OR ta.user_id = auth.uid())
    )
);

-- Editor/Admin hoặc người tạo mới được sửa
CREATE POLICY "Events editable by editors"
ON events FOR ALL
USING (
    EXISTS (
        SELECT 1 FROM tree_access
        WHERE tree_access.family_tree_id = events.family_tree_id
        AND tree_access.user_id = auth.uid()
        AND tree_access.access_level IN ('edit','admin')
    )
    OR events.created_by = auth.uid()
);

-- Người dùng chỉ thấy nhắc nhở của mình
CREATE POLICY "Users see own reminders"
ON event_reminders FOR SELECT
USING (user_id = auth.uid());
