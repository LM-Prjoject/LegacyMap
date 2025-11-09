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
  birthDate?: string;
  deathDate?: string;
  birthPlace?: string;
  deathPlace?: string;
  biography?: string;
  avatarUrl?: string;
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
  const results: { candidateId: string; relation: RelTypeUpper; confidence: number }[] =
      [];

  let anyFailed = false;

  await Promise.all(
      others.map(async (cand) => {
        try {
          const list = await suggestRelationship(userId, treeId, sourceId, cand.id);
          if (!list?.length) return;
          const best = [...list].sort(
              (a, b) => (b.confidence ?? 0) - (a.confidence ?? 0)
          )[0];
          const upper = best.type.toUpperCase() as RelTypeUpper;
          results.push({
            candidateId: cand.id,
            relation: upper,
            confidence: best.confidence ?? 0,
          });
        } catch (err: any) {
          console.error("Suggest failed for", cand.id, err?.message || err);
          anyFailed = true;
        }
      })
  );

  if (anyFailed) {
    showToast.error("Không gợi ý được quan hệ cho một số người");
  }

  return results.sort((a, b) => b.confidence - a.confidence);
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

const api = {
  listTrees,
  createTree,
  updateTree,
  deleteTree,
  listMembers,
  addMember,
  updateMember,
  deleteMember,
  listRelationships,
  listPersonRelationships,
  createRelationship,
  suggestRelationship,
};

export default api;
