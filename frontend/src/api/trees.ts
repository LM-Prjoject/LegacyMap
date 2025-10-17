export interface ApiResponse<T> {
    data?: T;
    result?: T;
    payload?: T;
    message?: string;
    items?: T extends any[] ? T : never;
}

export interface FamilyTree {
    id: string;
    userId: string;
    name: string;
    description?: string | null;
    coverImageUrl?: string | null;
    isPublic: boolean;
    createdAt?: string;
    updatedAt?: string;
}

export interface FamilyTreeCreateRequest {
    name: string;
    description?: string;
    isPublic?: boolean;
    coverImageUrl?: string;
}

export interface FamilyTreeUpdateRequest {
    name?: string;
    description?: string;
    isPublic?: boolean;
    coverImageUrl?: string;
}

export interface Person {
    id: string;
    fullName: string;
    gender?: string;
    dob?: string;
}

export interface PersonCreateRequest {
    fullName: string;
    gender?: string;
    dob?: string;
}

export interface Relationship {
    id: string;
    fromPersonId: string;
    toPersonId: string;
    type: string;
}

export interface RelationshipCreateRequest {
    fromPersonId: string;
    toPersonId: string;
    type: string;
}

const jsonHeaders = { 'Content-Type': 'application/json' };

async function safeJson<T>(res: Response): Promise<T | undefined> {
    const text = await res.text();
    if (!text) return undefined as any;
    try { return JSON.parse(text) as T; } catch { return undefined as any; }
}

function pickData<T>(json: any): T {
    if (!json) return json as T;
    return (json.data ?? json.result ?? json.payload ?? json.items ?? json) as T;
}

const api = {
    async listTrees(userId: string): Promise<FamilyTree[]> {
        const res = await fetch(`/api/trees?userId=${encodeURIComponent(userId)}`);
        const json = await safeJson<ApiResponse<FamilyTree[]>>(res);
        const picked = pickData<FamilyTree[] | { items: FamilyTree[] }>(json);

        return Array.isArray(picked) ? picked : (picked as any)?.items ?? [];
    },

    async createTree(userId: string, req: FamilyTreeCreateRequest): Promise<FamilyTree> {
        const res = await fetch(`/api/trees?userId=${encodeURIComponent(userId)}`, {
            method: 'POST',
            headers: jsonHeaders,
            body: JSON.stringify(req),
        });

        if (!res.ok) {
            const json = await safeJson<ApiResponse<unknown>>(res);
            throw new Error(json?.message || 'Tạo gia phả thất bại');
        }

        const json = await safeJson<ApiResponse<FamilyTree>>(res);
        const tree = pickData<FamilyTree>(json);
        if (!tree || !tree.id) throw new Error('Phản hồi không hợp lệ');
        return tree;
    },

    async updateTree(userId: string, treeId: string, req: FamilyTreeUpdateRequest): Promise<FamilyTree> {
        const res = await fetch(`/api/trees/${encodeURIComponent(treeId)}?userId=${encodeURIComponent(userId)}`, {
            method: 'PUT',
            headers: jsonHeaders,
            body: JSON.stringify(req),
        });

        if (!res.ok) {
            const json = await safeJson<ApiResponse<unknown>>(res);
            throw new Error(json?.message || 'Cập nhật gia phả thất bại');
        }

        const json = await safeJson<ApiResponse<FamilyTree>>(res);
        const tree = pickData<FamilyTree>(json);
        if (!tree || !tree.id) throw new Error('Phản hồi không hợp lệ');
        return tree;
    },

    async deleteTree(userId: string, treeId: string): Promise<void> {
        const res = await fetch(`/api/trees/${encodeURIComponent(treeId)}?userId=${encodeURIComponent(userId)}`, {
            method: 'DELETE',
        });
        if (!res.ok) {
            const json = await safeJson<ApiResponse<unknown>>(res);
            throw new Error(json?.message || 'Xóa gia phả thất bại');
        }
    },

    async listMembers(userId: string, treeId: string): Promise<Person[]> {
        const res = await fetch(`/api/trees/${encodeURIComponent(treeId)}/members?userId=${encodeURIComponent(userId)}`);
        if (!res.ok) throw new Error('Không lấy được danh sách thành viên');

        const json = await safeJson<ApiResponse<Person[]>>(res);
        const picked = pickData<Person[] | { items: Person[] }>(json);
        return Array.isArray(picked) ? picked : (picked as any)?.items ?? [];
    },

    async addMember(userId: string, treeId: string, req: PersonCreateRequest): Promise<Person> {
        const res = await fetch(`/api/trees/${encodeURIComponent(treeId)}/members?userId=${encodeURIComponent(userId)}`, {
            method: 'POST',
            headers: jsonHeaders,
            body: JSON.stringify(req),
        });

        if (!res.ok) {
            const json = await safeJson<ApiResponse<unknown>>(res);
            throw new Error(json?.message || 'Thêm thành viên thất bại');
        }

        const json = await safeJson<ApiResponse<Person>>(res);
        const person = pickData<Person>(json);
        if (!person || !person.id) throw new Error('Phản hồi không hợp lệ');
        return person;
    },

    async createRelationship(userId: string, treeId: string, req: RelationshipCreateRequest): Promise<Relationship> {
        const res = await fetch(`/api/trees/${encodeURIComponent(treeId)}/relationships?userId=${encodeURIComponent(userId)}`, {
            method: 'POST',
            headers: jsonHeaders,
            body: JSON.stringify(req),
        });

        if (!res.ok) {
            const json = await safeJson<ApiResponse<unknown>>(res);
            throw new Error(json?.message || 'Tạo quan hệ thất bại');
        }

        const json = await safeJson<ApiResponse<Relationship>>(res);
        const rel = pickData<Relationship>(json);
        if (!rel || !rel.id) throw new Error('Phản hồi không hợp lệ');
        return rel;
    },

    async deleteRelationship(userId: string, treeId: string, relationshipId: string): Promise<void> {
        const res = await fetch(
            `/api/trees/${encodeURIComponent(treeId)}/relationships/${encodeURIComponent(relationshipId)}?userId=${encodeURIComponent(userId)}`,
            { method: 'DELETE' }
        );
        if (!res.ok) {
            const json = await safeJson<ApiResponse<unknown>>(res);
            throw new Error(json?.message || 'Xóa quan hệ thất bại');
        }
    },
};

export default api;
