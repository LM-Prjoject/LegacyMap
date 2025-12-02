import { showToast } from "@/lib/toast.ts";

const API_BASE =
    import.meta.env.VITE_API_BASE_URL ||
    (import.meta.env.DEV ? "http://localhost:8080/legacy/api" : "");

export interface ApiResponse<T> {
  data?: T;
  result?: T;
  payload?: T;
  message?: string;
  items?: T extends any[] ? T : never;
}

export async function getPublicUserBasic(userId: string): Promise<{ id: string; username?: string; fullName?: string } | null> {
  const res = await fetch(
      `${API_BASE}/users/${encodeURIComponent(userId)}/basic`,
      { headers: authHeaders() }
  );
  const json = await safeJson<ApiResponse<{ id: string; username?: string; fullName?: string }>>(res);
  if (!res.ok) return null;
  const data = pickData<{ id: string; username?: string; fullName?: string }>(json);
  return data as any;
}

async function getTreeOwner(treeId: string): Promise<string | null> {
  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(treeId)}/owner`,
      { headers: authHeaders() }
  );
  const json = await safeJson<ApiResponse<{ ownerId: string }>>(res);
  if (!res.ok) {
    throw new Error((json as any)?.message || 'Không lấy được chủ sở hữu cây');
  }
  const data = pickData<{ ownerId: string }>(json);
  return (data as any)?.ownerId ?? null;
}

async function listViewableTrees(userId: string): Promise<FamilyTree[]> {
  const res = await fetch(
      `${API_BASE}/trees/viewable?userId=${encodeURIComponent(userId)}`,
      {
        headers: authHeaders(),
      }
  );
  const json = await safeJson<ApiResponse<FamilyTree[]>>(res);
  if (!res.ok) throw new Error(json?.message || 'Không lấy được danh sách cây được liên kết');
  const picked = pickData<FamilyTree[] | { items: FamilyTree[] }>(json);
  return Array.isArray(picked) ? picked : (picked as any)?.items ?? [];
}

async function listMembersForViewer(
    userId: string,
    treeId: string
): Promise<Person[]> {
  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(
          treeId
      )}/viewer/members?userId=${encodeURIComponent(userId)}`,
      {
        headers: authHeaders(),
      }
  );
  const json = await safeJson<ApiResponse<Person[]>>(res);
  if (!res.ok) throw new Error(json?.message || 'Không tải được thành viên (viewer)');
  const picked = pickData<Person[] | { items: Person[] }>(json);
  return Array.isArray(picked) ? picked : (picked as any)?.items ?? [];
}

function getToken(): string | null {
  return localStorage.getItem("authToken");
}

function authHeaders(extra?: Record<string, string>) {
  const headers: Record<string, string> = { Accept: "application/json" };
  const t = getToken();
  if (t) headers.Authorization = `Bearer ${t}`;
  if (extra) Object.assign(headers, extra);
  return headers;
}

async function safeJson<T>(res: Response): Promise<T | undefined> {
  const text = await res.text();
  return text ? (JSON.parse(text) as T) : (undefined as any);
}

function pickData<T>(json: any): T {
  return (json?.data ??
      json?.result ??
      json?.payload ??
      json?.items ??
      json) as T;
}

export type RelationshipType =
    | "PARENT"
    | "CHILD"
    | "SPOUSE"
    | "SIBLING"
    | string;

export interface FamilyTree {
  id: string;
  name: string;
  description?: string | null;
  createdBy?: string;          // ✅ THÊM: từ backend
  createdByEmail?: string;     // ✅ THÊM: từ backend
  createdByUsername?: string;  // ✅ THÊM: từ backend
  isPublic: boolean;
  coverImageUrl?: string | null;
  createdAt?: string;
  updatedAt?: string;
  shareToken?: string;
  shareUrl?: string;
  sharePermission?: 'view' | 'edit';
  memberCount?: number;        // ✅ THÊM: từ backend

  // ✅ GIỮ LẠI cho tương thích
  userId?: string;             // Alias cho createdBy
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
  birthDate?: string;
  deathDate?: string;
  birthPlace?: string;
  deathPlace?: string;
  biography?: string;
  avatarUrl?: string;
  phone?: string;
  email?: string;
}

export interface PersonCreateRequest extends Omit<Person, "id"> {}

export interface Relationship {
  id: string;
  fromPersonId: string;
  toPersonId: string;
  type: RelationshipType;
}

export interface RelationshipCreateRequest {
  person1Id: string;
  person2Id: string;
  relationshipType: string;
  notes?: string;
}

export type RelTypeLower = "parent" | "child" | "spouse" | "sibling";
export type RelTypeUpper = "PARENT" | "CHILD" | "SPOUSE" | "SIBLING";

export interface RelationshipSuggestion {
  type: RelTypeLower;
  confidence?: number;
  reasons?: string[];
}

function mapRelationship(raw: any): Relationship {
  const fromId =
      raw?.fromPersonId ??
      raw?.person1Id ??
      raw?.person1_id ??
      raw?.person1?.id;
  const toId =
      raw?.toPersonId ?? raw?.person2Id ?? raw?.person2_id ?? raw?.person2?.id;
  const type = String(raw?.type ?? raw?.relationshipType ?? "").toUpperCase() as RelTypeUpper;
  return {
    id:
        raw?.id ??
        raw?.relationshipId ??
        (crypto.randomUUID?.() ?? String(Math.random())),
    fromPersonId: String(fromId),
    toPersonId: String(toId),
    type,
  };
}

async function listTrees(userId: string): Promise<FamilyTree[]> {
  const headers = authHeaders({ "Content-Type": "application/json" });

  const tryParse = async (res: Response) => {
    const json = await safeJson<
        ApiResponse<FamilyTree[] | { items: FamilyTree[] }>
    >(res);
    const picked = pickData<FamilyTree[] | { items: FamilyTree[] }>(json);
    return Array.isArray(picked) ? picked : (picked as any)?.items ?? [];
  };

  try {
    const res = await fetch(
        `${API_BASE}/trees?userId=${encodeURIComponent(userId)}`,
        {
          headers: authHeaders(),
        }
    );
    if (res.ok) return await tryParse(res);

    if ([405, 404, 500].includes(res.status)) {
      const r1 = await fetch(`${API_BASE}/trees/search`, {
        method: "POST",
        headers,
        body: JSON.stringify({ userId }),
      });
      if (r1.ok) return await tryParse(r1);

      const r2 = await fetch(`${API_BASE}/trees/list`, {
        method: "POST",
        headers,
        body: JSON.stringify({ userId }),
      });
      if (r2.ok) return await tryParse(r2);

      const r3 = await fetch(
          `${API_BASE}/users/${encodeURIComponent(userId)}/trees`,
          {
            headers: authHeaders(),
          }
      );
      if (r3.ok) return await tryParse(r3);

      const j = await safeJson<ApiResponse<any>>(res);
      throw new Error(
          j?.message || "Không lấy được danh sách cây"
      );
    } else {
      const j = await safeJson<ApiResponse<any>>(res);
      throw new Error(
          j?.message || `Không lấy được danh sách cây`
      );
    }
  } catch (e: any) {
    throw new Error(e?.message || "Không lấy được danh sách cây");
  }
}

async function createTree(
    userId: string,
    req: FamilyTreeCreateRequest
): Promise<FamilyTree> {
  const res = await fetch(
      `${API_BASE}/trees?userId=${encodeURIComponent(userId)}`,
      {
        method: "POST",
        headers: authHeaders({ "Content-Type": "application/json" }),
        body: JSON.stringify(req),
      }
  );
  const json = await safeJson<ApiResponse<FamilyTree>>(res);
  if (!res.ok) throw new Error(json?.message || "Tạo gia phả thất bại");
  return pickData<FamilyTree>(json);
}

async function updateTree(
    userId: string,
    treeId: string,
    req: FamilyTreeUpdateRequest
): Promise<FamilyTree> {
  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(treeId)}?userId=${encodeURIComponent(
          userId
      )}`,
      {
        method: "PUT",
        headers: authHeaders({ "Content-Type": "application/json" }),
        body: JSON.stringify(req),
      }
  );
  const json = await safeJson<ApiResponse<FamilyTree>>(res);
  if (!res.ok) throw new Error(json?.message || "Cập nhật gia phả thất bại");
  return pickData<FamilyTree>(json);
}

async function deleteTree(userId: string, treeId: string): Promise<void> {
  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(treeId)}?userId=${encodeURIComponent(
          userId
      )}`,
      {
        method: "DELETE",
        headers: authHeaders(),
      }
  );
  if (!res.ok) {
    const j = await safeJson<ApiResponse<unknown>>(res);
    throw new Error(j?.message || "Xóa gia phả thất bại");
  }
}

async function listMembers(
    userId: string,
    treeId: string
): Promise<Person[]> {
  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(
          treeId
      )}/members?userId=${encodeURIComponent(userId)}`,
      {
        headers: authHeaders(),
      }
  );
  const json = await safeJson<ApiResponse<Person[]>>(res);
  if (!res.ok) {
    throw new Error((json as any)?.message || 'Không tải được thành viên');
  }
  const picked = pickData<Person[] | { items: Person[] }>(json);
  return Array.isArray(picked) ? picked : (picked as any)?.items ?? [];
}

async function addMember(
    userId: string,
    treeId: string,
    req: PersonCreateRequest
): Promise<Person> {
  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(
          treeId
      )}/members?userId=${encodeURIComponent(userId)}`,
      {
        method: "POST",
        headers: authHeaders({ "Content-Type": "application/json" }),
        body: JSON.stringify(req),
      }
  );
  const json = await safeJson<ApiResponse<Person>>(res);
  if (!res.ok) throw new Error(json?.message || "Thêm thành viên thất bại");
  return pickData<Person>(json);
}

async function updateMember(
    userId: string,
    treeId: string,
    personId: string,
    req: Partial<PersonCreateRequest>
): Promise<Person> {
  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(treeId)}/members/${encodeURIComponent(personId)}?userId=${encodeURIComponent(userId)}`,
      {
        method: "PUT",
        headers: authHeaders({ "Content-Type": "application/json" }),
        body: JSON.stringify(req),
      }
  );
  const json = await safeJson<ApiResponse<Person>>(res);
  if (!res.ok) throw new Error(json?.message || "Cập nhật thông tin thất bại");
  return pickData<Person>(json);
}

async function deleteMember(userId: string, treeId: string, personId: string): Promise<void> {
  const url = `${API_BASE}/trees/${encodeURIComponent(treeId)}/members/${encodeURIComponent(personId)}?userId=${encodeURIComponent(userId)}`;
  const res = await fetch(url, {
    method: "DELETE",
    headers: authHeaders(),
  });
  if (!res.ok) {
    const j = await safeJson<ApiResponse<any>>(res);
    throw new Error(j?.message || "Xóa thành viên thất bại");
  }
}

async function listRelationships(
    userId: string,
    treeId: string
): Promise<Relationship[]> {
  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(
          treeId
      )}/relationships?userId=${encodeURIComponent(userId)}`,
      {
        headers: authHeaders(),
      }
  );
  const json = await safeJson<ApiResponse<any[]>>(res);
  if (!res.ok) {
    throw new Error((json as any)?.message || 'Không tải được mối quan hệ');
  }
  const picked = pickData<any[] | { items: any[] }>(json);
  const raw = Array.isArray(picked) ? picked : (picked as any)?.items ?? [];
  return raw.map(mapRelationship);
}

async function listRelationshipsForViewer(
    userId: string,
    treeId: string
): Promise<Relationship[]> {
  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(
          treeId
      )}/viewer/relationships?userId=${encodeURIComponent(userId)}`,
      {
        headers: authHeaders(),
      }
  );
  const json = await safeJson<ApiResponse<any[]>>(res);
  if (!res.ok) throw new Error(json?.message || 'Không tải được mối quan hệ (viewer)');
  const picked = pickData<any[] | { items: any[] }>(json);
  const raw = Array.isArray(picked) ? picked : (picked as any)?.items ?? [];
  return raw.map(mapRelationship);
}

async function createRelationship(
    userId: string,
    treeId: string,
    body: RelationshipCreateRequest
): Promise<any> {
  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(
          treeId
      )}/relationships?userId=${encodeURIComponent(userId)}`,
      {
        method: "POST",
        headers: authHeaders({ "Content-Type": "application/json" }),
        body: JSON.stringify(body),
      }
  );
  const json = await safeJson<ApiResponse<any>>(res);
  if (!res.ok) throw new Error(json?.message || "Create relationship failed");
  return pickData<any>(json);
}

async function suggestRelationship(
    userId: string,
    treeId: string,
    person1Id: string,
    person2Id: string
): Promise<RelationshipSuggestion[]> {
  const url =
      `${API_BASE}/trees/${encodeURIComponent(treeId)}/relationships/suggest` +
      `?userId=${encodeURIComponent(userId)}` +
      `&person1Id=${encodeURIComponent(person1Id)}` +
      `&person2Id=${encodeURIComponent(person2Id)}`;

  const res = await fetch(url, { headers: authHeaders() });

  const json = await safeJson<
      ApiResponse<RelationshipSuggestion[] | { items: RelationshipSuggestion[] }>
  >(res);
  if (!res.ok)
    throw new Error((json as any)?.message || "Suggest relationship failed");

  const picked = pickData<
      RelationshipSuggestion[] | { items: RelationshipSuggestion[] }
  >(json);
  const arr = Array.isArray(picked) ? picked : picked?.items ?? [];
  return arr.map((s) => ({
    ...s,
    type: String(s.type ?? "").toLowerCase() as RelTypeLower,
  }));
}

export async function suggestForSource(
    userId: string,
    treeId: string,
    sourceId: string,
    persons: Person[]
) {
  const others = persons.filter((p) => p.id !== sourceId);
  if (!others.length) return [] as { candidateId: string; relation: RelTypeUpper; confidence: number }[];

  const url = `${API_BASE}/trees/${encodeURIComponent(treeId)}/relationships/suggest/source?userId=${encodeURIComponent(userId)}`;
  try {
    const res = await fetch(url, {
      method: "POST",
      headers: authHeaders({ "Content-Type": "application/json" }),
      body: JSON.stringify({
        sourceId,
        candidateIds: others.map((p) => p.id),
      }),
    });
    const text = await res.text();
    let json: any = undefined;
    try {
      json = text ? JSON.parse(text) : undefined;
    } catch (parseErr: any) {
      console.error("[suggestForSource] parse error", parseErr?.message || parseErr, "body:", (text || "").slice(0, 400));
      throw new Error("Suggest (batch) parse failed");
    }
    if (!res.ok) throw new Error((json as any)?.message || "Suggest (batch) failed");

    const arr = pickData<Array<{ candidateId: string; type: string; confidence?: number; reasons?: string[] }>>(json) || [];

    const results = arr
        .map((item) => ({
          candidateId: String(item.candidateId),
          relation: String(item.type || "").toUpperCase() as RelTypeUpper,
          confidence: item.confidence ?? 0,
        }))
        .filter((x) => !!x.candidateId && !!x.relation);
    try { console.log("[suggestForSource:batch] size", results.length, "top", results[0]); } catch {}
    return results.sort((a, b) => b.confidence - a.confidence);
  } catch (e: any) {
    console.error("Suggest batch failed", e?.message || e);
    showToast.error(e?.message || "Không gợi ý được quan hệ");
    return [];
  }
}

async function listPersonRelationships(
    userId: string,
    treeId: string,
    personId: string
): Promise<Relationship[]> {
  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(treeId)}/persons/${encodeURIComponent(personId)}/relationships?userId=${encodeURIComponent(userId)}`,
      {
        headers: authHeaders(),
      }
  );
  const json = await safeJson<ApiResponse<any[]>>(res);
  if (!res.ok) {
    throw new Error(json?.message || 'Không tải được danh sách mối quan hệ');
  }
  const picked = pickData<any[] | { items: any[] }>(json);
  const raw = Array.isArray(picked) ? picked : (picked as any)?.items ?? [];
  return raw.map(mapRelationship);
}

export async function exportTreePdfWithImage(
    treeId: string,
    imageBlob: Blob
): Promise<Blob> {
  const formData = new FormData();
  formData.append("treeImage", imageBlob, "tree.png");

  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(treeId)}/export/pdf`,
      {
        method: "POST",
        headers: authHeaders(),
        body: formData,
      }
  );

  if (!res.ok) {
    const json = await safeJson<ApiResponse<any>>(res);
    throw new Error(json?.message || "Xuất PDF thất bại");
  }

  return res.blob();
}

// ==================== SHARING API ====================

export interface TreeShareResponse {
  treeId: string;
  treeName: string;
  shareToken: string;
  shareUrl: string;
  publicShareUrl: string;
  sharedWithCount: number;
  sharePermission?: 'view' | 'edit'; // ✅ THÊM

}

export interface TreeAccessResponse {
  userId: string;
  userEmail: string;
  userName: string;
  accessLevel: "view" | "edit" | "admin";
  grantedBy: string | null;
  grantedByEmail: string | null;
  grantedAt: string;
}

export interface TreeShareRequest {
  email: string;
  accessLevel: "view" | "edit" | "admin";
  message?: string;
}

/**
 * Tạo link chia sẻ PUBLIC (ai cũng xem được)
 */
async function generatePublicShareLink(
    userId: string,
    treeId: string,
    permission: "view" | "edit" = "view" // ✅ Thêm param này
): Promise<TreeShareResponse> {
  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(treeId)}/share/public?userId=${encodeURIComponent(userId)}&permission=${permission}`, // ✅ Thêm &permission=${permission}
      {
        method: "POST",
        headers: authHeaders({ "Content-Type": "application/json" }),
      }
  );
  const json = await safeJson<ApiResponse<TreeShareResponse>>(res);
  if (!res.ok) throw new Error(json?.message || "Tạo link chia sẻ thất bại");
  return pickData<TreeShareResponse>(json);
}

/**
 * Tắt chia sẻ PUBLIC
 */
async function disablePublicSharing(
    userId: string,
    treeId: string
): Promise<void> {
  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(treeId)}/share/public?userId=${encodeURIComponent(userId)}`,
      {
        method: "DELETE",
        headers: authHeaders(),
      }
  );
  if (!res.ok) {
    const j = await safeJson<ApiResponse<unknown>>(res);
    throw new Error(j?.message || "Tắt chia sẻ thất bại");
  }
}

/**
 * Chia sẻ tree với user cụ thể (yêu cầu email)
 */
async function shareWithUser(
    userId: string,
    treeId: string,
    req: TreeShareRequest
): Promise<TreeAccessResponse> {
  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(treeId)}/share/user?userId=${encodeURIComponent(userId)}`,
      {
        method: "POST",
        headers: authHeaders({ "Content-Type": "application/json" }),
        body: JSON.stringify(req),
      }
  );
  const json = await safeJson<ApiResponse<TreeAccessResponse>>(res);
  if (!res.ok) throw new Error(json?.message || "Chia sẻ thất bại");
  return pickData<TreeAccessResponse>(json);
}

/**
 * Lấy danh sách người được chia sẻ
 */
async function getSharedUsers(
    userId: string,
    treeId: string
): Promise<TreeAccessResponse[]> {
  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(treeId)}/share/users?userId=${encodeURIComponent(userId)}`,
      {
        headers: authHeaders(),
      }
  );
  const json = await safeJson<ApiResponse<TreeAccessResponse[]>>(res);
  if (!res.ok) throw new Error(json?.message || "Lấy danh sách thất bại");
  const picked = pickData<TreeAccessResponse[] | { items: TreeAccessResponse[] }>(json);
  return Array.isArray(picked) ? picked : (picked as any)?.items ?? [];
}

/**
 * Thu hồi quyền truy cập
 */
async function revokeAccess(
    userId: string,
    treeId: string,
    targetUserId: string
): Promise<void> {
  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(treeId)}/share/users/${encodeURIComponent(targetUserId)}?userId=${encodeURIComponent(userId)}`,
      {
        method: "DELETE",
        headers: authHeaders(),
      }
  );
  if (!res.ok) {
    const j = await safeJson<ApiResponse<unknown>>(res);
    throw new Error(j?.message || "Thu hồi quyền thất bại");
  }
}

/**
 * PUBLIC: Xem tree qua share token (không cần đăng nhập nếu public)
 */
async function getSharedTree(
    shareToken: string,
    userId?: string | null
): Promise<FamilyTree> {
  const url = userId
      ? `${API_BASE}/trees/shared/${encodeURIComponent(shareToken)}?userId=${encodeURIComponent(userId)}`
      : `${API_BASE}/trees/shared/${encodeURIComponent(shareToken)}`;

  console.log('🔗 Fetching shared tree from:', url);

  const res = await fetch(url, {
    headers: userId ? authHeaders() : { Accept: "application/json" },
  });

  const json = await safeJson<ApiResponse<FamilyTree>>(res);

  if (!res.ok) {
    console.error('❌ API Error:', res.status, json);
    throw new Error(json?.message || "Không thể truy cập cây gia phả");
  }

  // ✅ SỬA: Extract data từ response
  const treeData = pickData<FamilyTree>(json);

  // ✅ CRITICAL FIX: Đảm bảo có ID
  if (!treeData.id) {
    console.error('⚠️ Missing tree.id in response');

    // Thử extract từ nhiều nguồn
    const possibleId =
        json?.data?.id ||
        json?.result?.id ||
        json?.payload?.id ||
        (json as any)?.id;

    if (possibleId) {
      treeData.id = String(possibleId);
      console.log('✅ Recovered tree.id:', possibleId);
    } else {
      throw new Error('Tree ID not found in response'); // ✅ Throw error thay vì tạo temp ID
    }
  }

  console.log('✅ Final tree data:', {
    id: treeData.id,
    name: treeData.name,
    sharePermission: treeData.sharePermission
  });

  return treeData;
}

/**
 * PUBLIC: Lấy members của shared tree
 */
async function getSharedTreeMembers(
    shareToken: string,
    userId?: string | null
): Promise<Person[]> {
  const url = userId
      ? `${API_BASE}/trees/shared/${encodeURIComponent(shareToken)}/members?userId=${encodeURIComponent(userId)}`
      : `${API_BASE}/trees/shared/${encodeURIComponent(shareToken)}/members`;

  const res = await fetch(url, {
    headers: userId ? authHeaders() : { Accept: "application/json" },
  });

  const json = await safeJson<ApiResponse<Person[]>>(res);
  if (!res.ok) throw new Error(json?.message || "Không lấy được danh sách thành viên");
  const picked = pickData<Person[] | { items: Person[] }>(json);
  return Array.isArray(picked) ? picked : (picked as any)?.items ?? [];
}

/**
 * AUTHENTICATED: Thêm member qua share link (cần quyền EDIT)
 */
async function addSharedTreeMember(
    shareToken: string,
    userId: string,
    req: PersonCreateRequest
): Promise<Person> {
  const res = await fetch(
      `${API_BASE}/trees/shared/${encodeURIComponent(shareToken)}/members?userId=${encodeURIComponent(userId)}`,
      {
        method: "POST",
        headers: authHeaders({ "Content-Type": "application/json" }),
        body: JSON.stringify(req),
      }
  );
  const json = await safeJson<ApiResponse<Person>>(res);
  if (!res.ok) throw new Error(json?.message || "Thêm thành viên thất bại");
  return pickData<Person>(json);
}

async function updateSharedTreeMember(
    shareToken: string,
    userId: string,
    personId: string,
    req: Partial<PersonCreateRequest>
): Promise<Person> {
  const res = await fetch(
      `${API_BASE}/trees/shared/${encodeURIComponent(shareToken)}/members/${encodeURIComponent(personId)}?userId=${encodeURIComponent(userId)}`,
      {
        method: "PUT",
        headers: authHeaders({ "Content-Type": "application/json" }),
        body: JSON.stringify(req),
      }
  );
  const json = await safeJson<ApiResponse<Person>>(res);
  if (!res.ok) throw new Error(json?.message || "Cập nhật thông tin thất bại");
  return pickData<Person>(json);
}

async function getSharedTreeRelationships(
    shareToken: string,
    userId?: string | null
): Promise<Relationship[]> {
  const url = userId
      ? `${API_BASE}/trees/shared/${encodeURIComponent(shareToken)}/relationships?userId=${encodeURIComponent(userId)}`
      : `${API_BASE}/trees/shared/${encodeURIComponent(shareToken)}/relationships`;

  const res = await fetch(url, {
    headers: userId ? authHeaders() : { Accept: "application/json" },
  });

  const json = await safeJson<ApiResponse<any[]>>(res);
  if (!res.ok) throw new Error(json?.message || "Không lấy được danh sách quan hệ");

  const picked = pickData<any[] | { items: any[] }>(json);
  const raw = Array.isArray(picked) ? picked : (picked as any)?.items ?? [];
  return raw.map(mapRelationship);
}

async function checkTreeAccess(
    treeId: string,
    userId: string
): Promise<{ accessLevel: 'view' | 'edit' | 'admin' }> {
  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(treeId)}/access?userId=${encodeURIComponent(userId)}`,
      {
        headers: authHeaders(),
      }
  );

  const json = await safeJson<ApiResponse<any>>(res);
  if (!res.ok) {
    return { accessLevel: 'view' }; // Default fallback
  }

  return pickData(json);
}
async function saveSharedTreeToDashboard(
    userId: string,
    treeId: string
): Promise<void> {
  try {
    const res = await fetch(
        `${API_BASE}/trees/${encodeURIComponent(treeId)}/save?userId=${encodeURIComponent(userId)}`,
        {
          method: "POST",
          headers: authHeaders(),
        }
    );

    if (!res.ok) {
      const j = await safeJson<ApiResponse<unknown>>(res);

      // Bỏ qua lỗi nếu tree đã được save
      if (j?.message?.includes('already')) {
        console.log('Tree already saved');
        return;
      }

      throw new Error(j?.message || "Lưu cây thất bại");
    }
  } catch (error: any) {
    // Bỏ qua lỗi 405 (Method Not Allowed)
    if (error?.message?.includes('405')) {
      console.warn('Save endpoint not available, skipping');
      return;
    }
    throw error;
  }
}

export const saveSharedTreeByToken = async (
    userId: string,
    shareToken: string
): Promise<string> => {
  const response = await fetch(
      `${API_BASE}/trees/shared/${encodeURIComponent(shareToken)}/save?userId=${encodeURIComponent(userId)}`,
      {
        method: "POST",
        headers: authHeaders(),
      }
  );

  const json = await safeJson<ApiResponse<string>>(response);
  if (!response.ok) {
    throw new Error(json?.message || "Lưu cây thất bại");
  }

  return pickData<string>(json);
};

export const getSharedTreeRelationshipsExport = async (
    shareToken: string,
    userId?: string
): Promise<Relationship[]> => {
  const params = userId ? `?userId=${userId}` : '';
  const response = await fetch(
      `${API_BASE}/trees/shared/${encodeURIComponent(shareToken)}/relationships${params}`,
      {
        headers: userId ? authHeaders() : { Accept: "application/json" },
      }
  );

  const json = await safeJson<ApiResponse<Relationship[]>>(response);
  if (!response.ok) {
    throw new Error(json?.message || "Không lấy được danh sách quan hệ");
  }

  return pickData<Relationship[]>(json);
};

async function getSharedTreeAccessInfo(
    shareToken: string,
    userId?: string | null
): Promise<{
  treeId: string;
  treeName: string;
  canEdit: boolean;
  canView: boolean;
  role: 'OWNER' | 'EDITOR' | 'VIEWER';
}> {
  const url = userId
      ? `${API_BASE}/trees/shared/${encodeURIComponent(shareToken)}/access-info?userId=${encodeURIComponent(userId)}`
      : `${API_BASE}/trees/shared/${encodeURIComponent(shareToken)}/access-info`;

  const res = await fetch(url, {
    headers: userId ? authHeaders() : { Accept: "application/json" },
  });

  const json = await safeJson<ApiResponse<any>>(res);
  if (!res.ok) {
    throw new Error(json?.message || "Không lấy được thông tin access");
  }

  return pickData(json);
}

// ==================== EDIT ACCESS REQUEST API ====================

async function requestEditAccess(
    userId: string,
    treeId: string
): Promise<void> {
  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(treeId)}/request-edit-access?userId=${encodeURIComponent(userId)}`,
      {
        method: "POST",
        headers: authHeaders(),
      }
  );

  if (!res.ok) {
    const j = await safeJson<ApiResponse<unknown>>(res);
    throw new Error(j?.message || "Gửi yêu cầu thất bại");
  }
}

async function approveEditRequest(
    ownerId: string,
    treeId: string,
    requesterId: string
): Promise<void> {
  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(treeId)}/approve-edit-request?ownerId=${encodeURIComponent(ownerId)}&requesterId=${encodeURIComponent(requesterId)}`,
      {
        method: "POST",
        headers: authHeaders(),
      }
  );

  if (!res.ok) {
    const j = await safeJson<ApiResponse<unknown>>(res);
    throw new Error(j?.message || "Chấp nhận yêu cầu thất bại");
  }
}

async function rejectEditRequest(
    ownerId: string,
    treeId: string,
    requesterId: string
): Promise<void> {
  const res = await fetch(
      `${API_BASE}/trees/${encodeURIComponent(treeId)}/reject-edit-request?ownerId=${encodeURIComponent(ownerId)}&requesterId=${encodeURIComponent(requesterId)}`,
      {
        method: "POST",
        headers: authHeaders(),
      }
  );

  if (!res.ok) {
    const j = await safeJson<ApiResponse<unknown>>(res);
    throw new Error(j?.message || "Từ chối yêu cầu thất bại");
  }
}

const api = {
  listTrees,
  listViewableTrees,
  createTree,
  updateTree,
  deleteTree,
  listMembers,
  listMembersForViewer,
  addMember,
  updateMember,
  deleteMember,
  listRelationships,
  listRelationshipsForViewer,
  listPersonRelationships,
  getTreeOwner,
  getPublicUserBasic,
  createRelationship,
  suggestRelationship,
  suggestForSource,
  generatePublicShareLink,
  disablePublicSharing,
  shareWithUser,
  getSharedUsers,
  revokeAccess,
  getSharedTree,
  getSharedTreeMembers,
  addSharedTreeMember,
  updateSharedTreeMember,
  getSharedTreeRelationships,
  checkTreeAccess,
  saveSharedTreeToDashboard,
  saveSharedTreeByToken,
  getSharedTreeRelationshipsExport,
  getSharedTreeAccessInfo,
  exportTreePdfWithImage,
  requestEditAccess,
  approveEditRequest,
  rejectEditRequest,
};

export default api;