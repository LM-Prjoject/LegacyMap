-- Bảng users: quản lý thông tin đăng nhập & trạng thái tài khoản
CREATE TABLE users (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    email VARCHAR(255) UNIQUE NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    username VARCHAR(50) UNIQUE NOT NULL,
    role_name VARCHAR(20) NOT NULL CHECK (role_name IN ('user', 'admin')),
    is_verified BOOLEAN DEFAULT FALSE,
    is_active BOOLEAN DEFAULT TRUE,
    last_login TIMESTAMP WITH TIME ZONE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);
 
-- Bảng user_profiles: lưu thông tin chi tiết người dùng
CREATE TABLE user_profiles (
    user_id UUID PRIMARY KEY REFERENCES users(id) ON DELETE CASCADE,
    full_name VARCHAR(100),
    clan_name VARCHAR(100),
    gender VARCHAR(10) CHECK (gender IN ('male', 'female', 'other')),
    phone VARCHAR(20),
    dob DATE,
    address JSONB, -- {city: '...', ward: '...', house_number: '...'}
    avatar_url TEXT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);
 
-- Bảng auth_tokens: hợp nhất email_verifications + password_resets
CREATE TABLE auth_tokens (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id UUID REFERENCES users(id) ON DELETE CASCADE NOT NULL,
    token VARCHAR(100) UNIQUE NOT NULL,
    type VARCHAR(30) NOT NULL CHECK (type IN ('email_verification', 'password_reset', 'session')),
    expires_at TIMESTAMP WITH TIME ZONE NOT NULL,
    used BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);
-- Bảng family_trees
CREATE TABLE family_trees (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    name VARCHAR(200) NOT NULL,
    description TEXT,
    created_by UUID REFERENCES users(id) NOT NULL,
    is_public BOOLEAN DEFAULT FALSE,
    share_token UUID DEFAULT gen_random_uuid(),
    cover_image_url TEXT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);
 
-- Bảng persons (thành viên trong cây)
CREATE TABLE persons (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    family_tree_id UUID REFERENCES family_trees(id) ON DELETE CASCADE NOT NULL,
    full_name VARCHAR(200) NOT NULL,
    gender VARCHAR(10) CHECK (gender IN ('male', 'female', 'other')),
    birth_date DATE,
    death_date DATE,
    birth_place VARCHAR(200),
    death_place VARCHAR(200),
    biography TEXT,
    avatar_url TEXT,
    created_by UUID REFERENCES users(id),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);
 
-- Bảng relationships
CREATE TABLE relationships (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    family_tree_id UUID REFERENCES family_trees(id) ON DELETE CASCADE NOT NULL,
    person1_id UUID REFERENCES persons(id) ON DELETE CASCADE NOT NULL,
    person2_id UUID REFERENCES persons(id) ON DELETE CASCADE NOT NULL,
    relationship_type VARCHAR(30) CHECK (relationship_type IN ('parent', 'child', 'spouse', 'sibling')),
    notes TEXT,
    created_by UUID REFERENCES users(id),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    CONSTRAINT unique_relationship UNIQUE(family_tree_id, person1_id, person2_id, relationship_type),
    CONSTRAINT different_persons CHECK (person1_id != person2_id)
);
 
-- Bảng phân quyền truy cập cây
CREATE TABLE tree_access (
    user_id UUID REFERENCES users(id) ON DELETE CASCADE,
    family_tree_id UUID REFERENCES family_trees(id) ON DELETE CASCADE,
    access_level VARCHAR(20) DEFAULT 'view' CHECK (access_level IN ('view', 'edit', 'admin')),
    granted_by UUID REFERENCES users(id),
    granted_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    PRIMARY KEY (user_id, family_tree_id)
);
-- Bảng audit_logs
CREATE TABLE audit_logs (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id UUID REFERENCES users(id),
    entity_type VARCHAR(50) NOT NULL, -- 'family_tree', 'person', 'relationship'
    entity_id UUID NOT NULL,
    action VARCHAR(50) NOT NULL, -- 'create', 'update', 'delete'
    old_data JSONB,
    new_data JSONB,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);
 
-- Bảng media (ảnh, tài liệu)
CREATE TABLE media (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    family_tree_id UUID REFERENCES family_trees(id) ON DELETE CASCADE,
    person_id UUID REFERENCES persons(id) ON DELETE CASCADE,
    uploaded_by UUID REFERENCES users(id),
    file_name VARCHAR(255) NOT NULL,
    file_url TEXT NOT NULL,
    file_type VARCHAR(50),
    file_size BIGINT,
    description TEXT,
    is_public BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);
 
-- Bảng notifications
CREATE TABLE notifications (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id UUID REFERENCES users(id) ON DELETE CASCADE NOT NULL,
    title VARCHAR(200) NOT NULL,
    message TEXT NOT NULL,
    type VARCHAR(50) CHECK (type IN ('system', 'update', 'invite', 'alert')),
    related_entity JSONB, -- {type:'family_tree', id:'uuid'}
    is_read BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);
 

