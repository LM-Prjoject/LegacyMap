import { useEffect, useMemo, useState } from "react";
import type { Person, Relationship } from "@/api/trees";
import {ArrowDown, ArrowUp, X} from "lucide-react";

interface Props {
  open: boolean;
  onClose: () => void;
  persons: Person[];
  relationships?: Relationship[];
  title?: string;
}

export default function MemberListModal({ open, onClose, persons, relationships = [], title = "Danh sách thành viên" }: Props) {
  const [q, setQ] = useState("");
  const [sortKey, setSortKey] = useState<"name" | "age" | "generation">("name");
  const [sortAsc, setSortAsc] = useState(true);

  useEffect(() => {
    if (!open) return;
    const prev = document.body.style.overflow;
    document.body.style.overflow = "hidden";
    return () => {
      document.body.style.overflow = prev;
    };
  }, [open]);

  useEffect(() => {
    if (!open) return;
    setQ("");
  }, [open]);

  const normalized = (s: string) =>
    (s || "")
      .toString()
      .trim()
      .toLowerCase();

  const year = (d?: string | null) => {
    if (!d) return null;
    const t = new Date(d);
    return isNaN(t.getTime()) ? null : t.getUTCFullYear();
  };

  const generationMap = useMemo(() => {
    const parentOf: Record<string, string[]> = {};
    const indeg: Record<string, number> = {};
    for (const p of persons) {
      indeg[p.id] = 0;
      parentOf[p.id] = parentOf[p.id] || [];
    }
    for (const r of relationships) {
      const t = String(r.type).toUpperCase();
      if (t !== "PARENT") continue;
      parentOf[r.fromPersonId] = parentOf[r.fromPersonId] || [];
      parentOf[r.fromPersonId].push(r.toPersonId);
      indeg[r.toPersonId] = (indeg[r.toPersonId] ?? 0) + 1;
    }
    const roots = Object.keys(indeg).filter((id) => (indeg[id] ?? 0) === 0);
    const depth: Record<string, number> = {};
    const q: string[] = [];
    for (const r of roots) {
      depth[r] = 1;
      q.push(r);
    }
    while (q.length) {
      const u = q.shift()!;
      const du = depth[u] || 1;
      for (const v of parentOf[u] || []) {
        indeg[v] -= 1;
        if ((depth[v] ?? 0) < du + 1) depth[v] = du + 1;
        if (indeg[v] === 0) q.push(v);
      }
    }
    return depth;
  }, [persons, relationships]);

  const list = useMemo(() => {
    const k = normalized(q);
    const arr = persons.slice();
    if (!k) return arr;
    return arr.filter((p) => normalized(p.fullName).includes(k));
  }, [persons, q]);

  const sorted = useMemo(() => {
    const arr = list.slice();
    const cmp = (a: Person, b: Person) => {
      if (sortKey === "name") {
        return (a.fullName || "").localeCompare(b.fullName || "", "vi");
      }
      if (sortKey === "age") {
        const ya = year(a.birthDate);
        const yb = year(b.birthDate);
        if (ya == null && yb == null) return (a.fullName || "").localeCompare(b.fullName || "", "vi");
        if (ya == null) return 1;
        if (yb == null) return -1;
        return ya - yb;
      }
      const ga = generationMap[a.id] ?? 1;
      const gb = generationMap[b.id] ?? 1;
      if (ga !== gb) return ga - gb;
      return (a.fullName || "").localeCompare(b.fullName || "", "vi");
    };
    arr.sort(cmp);
    if (!sortAsc) arr.reverse();
    return arr;
  }, [list, sortKey, sortAsc, generationMap]);

  if (!open) return null;

  return (
    <div className="fixed inset-0 z-50">
      <div className="absolute inset-0 bg-black/40" onClick={onClose} />
      <div className="absolute left-1/2 top-1/2 -translate-x-1/2 -translate-y-1/2 w-[600px] max-w-[95vw] max-h-[90vh] rounded-2xl bg-white shadow-xl flex flex-col">
        <div className="p-4 border-b flex items-center justify-between gap-3">
          <h3 className="text-lg font-semibold text-gray-800">{title}</h3>
          <div className="flex items-center gap-2">
            <label className="text-sm text-gray-600">Sắp xếp:</label>
            <select
              value={sortKey}
              onChange={(e)=> setSortKey(e.target.value as any)}
              className="rounded-lg border px-1 py-1 text-sm text-black bg-white"
            >
              <option value="name">Tên</option>
              <option value="age">Tuổi (năm sinh)</option>
              <option value="generation">Thế hệ</option>
            </select>
            <button
              onClick={()=> setSortAsc(v=>!v)}
              className="px-2 py-1 rounded-lg border text-sm text-black hover:bg-gray-100"
              title={sortAsc?"Tăng dần":"Giảm dần"}
            >
              {sortAsc ? (
                  <ArrowUp size="24" className="w-4 h-4" />
              ) : (
                  <ArrowDown size="24" className="w-4 h-4" />
              )}
            </button>
            <button
              type="button"
              onClick={onClose}
              className="px-2 py-0.5 rounded-lg text-slate-800 border hover:bg-gray-100"
              title="Đóng"
              aria-label="Đóng"
            >
              <X size="24"/>
            </button>
          </div>
        </div>
        <div className="p-3 border-b">
          <input
            placeholder="Tìm theo tên..."
            className="w-full rounded-lg border px-3 py-2 bg-white text-black placeholder-gray-400 caret-black"
            value={q}
            onChange={(e) => setQ(e.target.value)}
          />
        </div>
        <div className="flex-1 min-h-0 overflow-auto">
          {sorted.length === 0 ? (
            <div className="p-4 text-sm text-gray-500">Không có thành viên phù hợp.</div>
          ) : (
            <ul className="divide-y">
              {sorted.map((p) => (
                <li key={p.id} className="px-4 py-3 flex items-center gap-3">
                  <img
                    src={p.avatarUrl || "https://placehold.co/48x48?text=\u{1F464}"}
                    alt={p.fullName}
                    className="w-10 h-10 rounded-full object-cover bg-gray-100"
                    onError={(e:any)=>{ e.currentTarget.src = "https://placehold.co/48x48?text=\u{1F464}"; }}
                  />
                  <div className="flex-1 min-w-0">
                    <div className="font-medium text-gray-900 truncate">{p.fullName}</div>
                    <div className="text-xs text-gray-600 truncate">
                      {p.gender ? (String(p.gender).toLowerCase()==="male"?"Nam":String(p.gender).toLowerCase()==="female"?"Nữ":p.gender) : "—"}
                      {p.birthDate ? ` • Sinh: ${p.birthDate}` : ""}
                      {p.deathDate ? ` • Mất: ${p.deathDate}` : ""}
                      {sortKey === 'generation' ? ` • Thế hệ: ${generationMap[p.id] ?? 1}` : ""}
                    </div>
                  </div>
                </li>
              ))}
            </ul>
          )}
        </div>
      </div>
    </div>
  );
}
