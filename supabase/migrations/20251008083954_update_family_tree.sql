-- Enable Row Level Security (RLS)
ALTER TABLE users ENABLE ROW LEVEL SECURITY;
ALTER TABLE user_profiles ENABLE ROW LEVEL SECURITY;
ALTER TABLE auth_tokens ENABLE ROW LEVEL SECURITY;
ALTER TABLE family_trees ENABLE ROW LEVEL SECURITY;
ALTER TABLE persons ENABLE ROW LEVEL SECURITY;
ALTER TABLE relationships ENABLE ROW LEVEL SECURITY;
ALTER TABLE tree_access ENABLE ROW LEVEL SECURITY;
ALTER TABLE audit_logs ENABLE ROW LEVEL SECURITY;
ALTER TABLE media ENABLE ROW LEVEL SECURITY;
ALTER TABLE notifications ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Users can select themselves"
  ON users FOR SELECT
  USING (id = auth.uid());

CREATE POLICY "Users can update themselves"
  ON users FOR UPDATE
  USING (id = auth.uid());

CREATE POLICY "Profiles can select self"
  ON user_profiles FOR SELECT
  USING (user_id = auth.uid());

CREATE POLICY "Profiles can update self"
  ON user_profiles FOR UPDATE
  USING (user_id = auth.uid());

-- Cho phép owner hoặc người có quyền truy cập xem cây
CREATE POLICY "Family trees readable by members"
  ON family_trees FOR SELECT
  USING (
    created_by = auth.uid()
    OR EXISTS (
      SELECT 1 FROM tree_access
      WHERE tree_access.user_id = auth.uid()
      AND tree_access.family_tree_id = family_trees.id
    )
  );

-- Owner và admin/edit mới có quyền sửa
CREATE POLICY "Family trees editable by owner or admin"
  ON family_trees FOR UPDATE
  USING (
    created_by = auth.uid()
    OR EXISTS (
      SELECT 1 FROM tree_access
      WHERE tree_access.user_id = auth.uid()
      AND tree_access.family_tree_id = family_trees.id
      AND tree_access.access_level IN ('edit','admin')
    )
  );

-- Owner được xóa
CREATE POLICY "Family trees deletable by owner"
  ON family_trees FOR DELETE
  USING (created_by = auth.uid());

CREATE POLICY "Persons readable by tree members"
  ON persons FOR SELECT
  USING (
    EXISTS (
      SELECT 1 FROM family_trees t
      JOIN tree_access ta ON ta.family_tree_id = t.id
      WHERE persons.family_tree_id = t.id
      AND (t.created_by = auth.uid() OR ta.user_id = auth.uid())
    )
  );

CREATE POLICY "Persons editable by editors/admins"
  ON persons FOR ALL
  USING (
    EXISTS (
      SELECT 1 FROM tree_access
      WHERE tree_access.family_tree_id = persons.family_tree_id
      AND tree_access.user_id = auth.uid()
      AND tree_access.access_level IN ('edit','admin')
    )
    OR persons.created_by = auth.uid()
  );

CREATE POLICY "Relationships visible to tree members"
  ON relationships FOR SELECT
  USING (
    EXISTS (
      SELECT 1 FROM family_trees t
      JOIN tree_access ta ON ta.family_tree_id = t.id
      WHERE relationships.family_tree_id = t.id
      AND (t.created_by = auth.uid() OR ta.user_id = auth.uid())
    )
  );

CREATE POLICY "Relationships editable by editors"
  ON relationships FOR ALL
  USING (
    EXISTS (
      SELECT 1 FROM tree_access
      WHERE tree_access.family_tree_id = relationships.family_tree_id
      AND tree_access.user_id = auth.uid()
      AND tree_access.access_level IN ('edit','admin')
    )
  );

CREATE POLICY "Tree access managed by owner"
  ON tree_access FOR ALL
  USING (
    EXISTS (
      SELECT 1 FROM family_trees
      WHERE family_trees.id = tree_access.family_tree_id
      AND family_trees.created_by = auth.uid()
    )
  );

CREATE POLICY "Users can view their notifications"
  ON notifications FOR SELECT
  USING (user_id = auth.uid());

CREATE POLICY "Users can delete their notifications"
  ON notifications FOR DELETE
  USING (user_id = auth.uid());

CREATE POLICY "Audit logs readable by tree members"
  ON audit_logs FOR SELECT
  USING (
    EXISTS (
      SELECT 1 FROM family_trees t
      JOIN tree_access ta ON ta.family_tree_id = t.id
      WHERE audit_logs.entity_id = t.id
      AND (t.created_by = auth.uid() OR ta.user_id = auth.uid())
    )
  );

CREATE POLICY "Media readable by members or public"
  ON media FOR SELECT
  USING (
    is_public
    OR EXISTS (
      SELECT 1 FROM family_trees t
      JOIN tree_access ta ON ta.family_tree_id = t.id
      WHERE media.family_tree_id = t.id
      AND (t.created_by = auth.uid() OR ta.user_id = auth.uid())
    )
  );
