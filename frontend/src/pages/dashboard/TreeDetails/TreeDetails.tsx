import { useEffect, useMemo, useState } from "react";
import { useParams, useNavigate } from "react-router-dom";
import TreeGraph from "@/components/familyTree/TreeGraph";
import RelationshipModal, { type RelationUpper, type PairSuggestion } from "@/components/familyTree/relaModal/RelationshipModal";
import MemberModal, { type MemberFormValues } from "@/components/familyTree/memberModal/MemberModal";
import api, { type Person, type Relationship, suggestForSource } from "@/api/trees";
import { showToast } from "@/lib/toast";
import { uploadMemberAvatarToSupabase } from "@/lib/upload";
import DetailsSidebar from "@/pages/dashboard/TreeDetails/DetailsSidebar";

import bg from "@/assets/bg.jpg";
import Navbar from "@/components/layout/Navbar";
import { ArrowLeft, LucideUserPlus } from "lucide-react";

export default function TreeDetails() {
    const { treeId } = useParams<{ treeId: string }>();
    const navigate = useNavigate();
    const user = JSON.parse(localStorage.getItem("user") || "{}");
    const userId: string = user?.id;

    const [persons, setPersons] = useState<Person[]>([]);
    const [rels, setRels] = useState<Relationship[]>([]);
    const [loading, setLoading] = useState(true);

    const [memberOpen, setMemberOpen] = useState(false);
    const [memberSubmitting, setMemberSubmitting] = useState(false);

    const [modalOpen, setModalOpen] = useState(false);
    const [source, setSource] = useState<Person | null>(null);

    const [tree, setTree] = useState<{
        coverImageUrl?: string | null;
        name?: string;
        description?: string | null;
        createdByName?: string | null;
        createdAt?: string | null;
    } | null>(null);

    useEffect(() => {
        if (!treeId) { setLoading(false); return; }
        if (!userId) { setLoading(false); showToast.error("Bạn cần đăng nhập để xem chi tiết cây"); return; }

        (async () => {
            setLoading(true);
            try {
                const [trees, ps, rs] = await Promise.all([
                    api.listTrees(userId),
                    api.listMembers(userId, treeId),
                    api.listRelationships(userId, treeId),
                ]);

                const found = trees.find((t) => t.id === treeId) || null;
                setTree(found ? {
                    coverImageUrl: found.coverImageUrl ?? null,
                    name: found.name ?? null,
                    description: found.description ?? null,
                    createdByName: (found as any).createdByName ?? user?.username ?? null,
                    createdAt: found.createdAt ?? null,
                } : null);

                setPersons(ps);
                setRels(rs);
            } catch (e:any) {
                showToast.error(e?.message || "Không tải được dữ liệu");
            } finally {
                setLoading(false);
            }
        })();
    }, [treeId, userId]);

    const handleAddClick = () => setMemberOpen(true);

    const handleCreateMember = async (values: MemberFormValues) => {
        if (!treeId || !userId || memberSubmitting) return;
        setMemberSubmitting(true);

        try {
            let avatarUrl = values.avatarUrl;

            if (values.avatarFile) {
                const toastId = showToast.loading("Đang tải ảnh lên…");
                try {
                    avatarUrl = await uploadMemberAvatarToSupabase(values.avatarFile);
                } catch (err: any) {
                    showToast.error(`Upload lỗi: ${err?.message || "Không thể tải ảnh lên."}`);
                    return;
                } finally {
                    showToast.dismiss(toastId);
                }
            }

            const created = await api.addMember(userId, treeId, {
                fullName: values.fullName.trim(),
                gender: values.gender || undefined,
                birthDate: values.birthDate || undefined,
                deathDate: values.deathDate || undefined,
                birthPlace: values.birthPlace || undefined,
                deathPlace: values.deathPlace || undefined,
                biography: values.biography || undefined,
                avatarUrl: avatarUrl || undefined,
            });

            setPersons(prev => {
                const shouldSuggest = prev.length > 0;
                const next = [...prev, created];
                if (shouldSuggest) {
                    setSource(created);
                    setModalOpen(true);
                }
                return next;
            });

            setMemberOpen(false);
            showToast.success("Thêm thành viên thành công");
        } catch (e: any) {
            showToast.error(e?.message || "Thêm thành viên thất bại");
        } finally {
            setMemberSubmitting(false);
        }
    };

    const confirmRelationship = async ({ relation, candidateId }: { relation: RelationUpper; candidateId: string }) => {
        if (!treeId || !source || !userId) return;

        let person1Id = source.id;
        let person2Id = candidateId;
        if (relation === "PARENT") { person1Id = candidateId; person2Id = source.id; }
        else if (relation === "CHILD") { person1Id = source.id; person2Id = candidateId; }

        const existingKeys = new Set(
            rels.map((r) => {
                const t = String(r.type).toUpperCase();
                if (t === "SPOUSE" || t === "SIBLING") {
                    const [a, b] = [r.fromPersonId, r.toPersonId].sort();
                    return `PAIR:${a}-${b}:${t}`;
                }
                const parent = t === "PARENT" ? r.fromPersonId : r.toPersonId;
                const child  = t === "PARENT" ? r.toPersonId   : r.fromPersonId;
                return `PARENT:${parent}->${child}`;
            })
        );

        const makeKey = () => {
            if (relation === "SPOUSE" || relation === "SIBLING") {
                const [a,b] = [person1Id, person2Id].sort();
                return `PAIR:${a}-${b}:${relation}`;
            }
            const parent = relation === "PARENT" ? person1Id : person2Id;
            const child  = relation === "PARENT" ? person2Id : person1Id;
            return `PARENT:${parent}->${child}`;
        };

        if (existingKeys.has(makeKey())) {
            showToast.error("Mối quan hệ này đã tồn tại");
            return;
        }

        const r = await api.createRelationship(userId, treeId, {
            person1Id,
            person2Id,
            relationshipType: relation,
        });

        setRels((prev) => [
            ...prev,
            { id: r?.id ?? crypto.randomUUID?.(), fromPersonId: person1Id, toPersonId: person2Id, type: relation },
        ]);
        showToast.success("Đã nối mối quan hệ");
    };

    const fetchSuggestions = async (sourceId: string): Promise<PairSuggestion[]> => {
        if (!treeId || !userId) return [];
        const list = await suggestForSource(userId, treeId, sourceId, persons);
        const existing = new Set(rels.map((r) => `${r.fromPersonId}-${r.toPersonId}-${String(r.type).toUpperCase()}`));
        return list.filter(
            (s) =>
                !existing.has(`${sourceId}-${s.candidateId}-${s.relation}`) &&
                !existing.has(`${s.candidateId}-${sourceId}-${s.relation}`)
        );
    };

    const generationCount = useMemo(() => {
        if (!persons.length) return 0;
        const parentOf: Record<string, string[]> = {};
        const hasParent: Record<string, boolean> = {};
        for (const r of rels) {
            const t = String(r.type).toUpperCase();
            if (t === "PARENT") {
                (parentOf[r.fromPersonId] ||= []).push(r.toPersonId);
                hasParent[r.toPersonId] = true;
            } else if (t === "CHILD") {
                (parentOf[r.toPersonId] ||= []).push(r.fromPersonId);
                hasParent[r.fromPersonId] = true;
            }
        }
        const ids = persons.map((p) => p.id);
        const roots = ids.filter((id) => !hasParent[id]);
        if (!roots.length) return 1;
        const depth: Record<string, number> = {};
        const q = [...roots];
        roots.forEach((r) => (depth[r] = 1));
        while (q.length) {
            const cur = q.shift()!;
            for (const c of parentOf[cur] || []) {
                if (!depth[c]) { depth[c] = depth[cur] + 1; q.push(c); }
            }
        }
        return ids.reduce((m, id) => Math.max(m, depth[id] || 1), 1);
    }, [persons, rels]);

    const canSuggest = useMemo(() => persons.length >= 1, [persons.length]);

    return (
        <div className="relative min-h-screen">
            <img src={bg} className="absolute inset-0 w-full h-full object-cover -z-10" />
            <div className="absolute inset-0 bg-slate-900/40 -z-10" />

            {/* Navbar */}
            <Navbar />

            {/* Content with padding for fixed navbar */}
            <div className="pt-24 px-4 md:px-6">
                <div className="w-full rounded-xl text-white">
                    <div className="flex items-center justify-between px-4 py-2">
                        <button
                            onClick={() => navigate("/dashboard")}
                            className="inline-flex items-center gap-2 px-4 py-2 rounded-lg bg-white/10 hover:bg-white/20 transition-colors backdrop-blur-sm"
                            title="Quay lại"
                        >
                            <ArrowLeft className="w-4 h-4" />
                            <span>Quay lại</span>
                        </button>
                        <button
                            onClick={handleAddClick}
                            className="inline-flex items-center gap-2 rounded-lg bg-gradient-to-r from-blue-500 to-blue-600 hover:from-blue-600 hover:to-blue-700 px-4 py-2 text-white shadow-lg hover:shadow-xl transition-all"
                            title="Thêm thành viên"
                        >
                            <LucideUserPlus className="w-5 h-5" />
                            <span>Thêm thành viên</span>
                        </button>
                    </div>
                </div>
            </div>

            <div className="grid grid-cols-12 gap-4 px-4 pb-4">
                <DetailsSidebar
                    coverImageUrl={tree?.coverImageUrl}
                    name={tree?.name}
                    description={tree?.description}
                    createdByName={user?.username}
                    createdAt={tree?.createdAt}
                    memberCount={loading ? 0 : persons.length}
                    generationCount={loading ? 0 : generationCount}
                />

                <main className="col-span-12 md:col-span-9 bg-white rounded-xl shadow p-3">
                    {!treeId ? (
                        <div className="text-sm text-slate-500">Không tìm thấy treeId trong URL.</div>
                    ) : loading ? (
                        <div className="text-gray-500 text-sm">Đang tải…</div>
                    ) : (
                        <TreeGraph persons={persons} relationships={rels} onNodeClick={() => {}} />
                    )}
                </main>
            </div>

            <MemberModal
                open={memberOpen}
                onClose={() => setMemberOpen(false)}
                onSubmit={handleCreateMember}
                submitting={memberSubmitting}
                title="Thêm thành viên"
            />
            {source && canSuggest && (
                <RelationshipModal
                    isOpen={modalOpen}
                    onClose={() => setModalOpen(false)}
                    source={source}
                    persons={persons}
                    fetchSuggestions={fetchSuggestions}
                    onConfirm={confirmRelationship}
                />
            )}
        </div>
    );
}