import { useEffect, useMemo, useState } from "react";
import { useParams, useNavigate } from "react-router-dom";
import TreeGraph from "@/components/familyTree/TreeGraph";
import RelationshipModal, { type RelationUpper } from "@/components/familyTree/relaModal/RelationshipModal";
import MemberModal, { type MemberFormValues } from "@/components/familyTree/memberModal/MemberModal";
import PersonDetailsModal from "@/components/familyTree/PersonDetailsModal";
import DetailsSidebar from "@/pages/dashboard/TreeDetails/DetailsSidebar";
import api, { type Person, type Relationship } from "@/api/trees";
import { showToast } from "@/lib/toast";
import { uploadMemberAvatarToSupabase } from "@/lib/upload";
import { authApi, type UserProfile } from "@/api/auth";
import bg from "@/assets/bg.jpg";
import Navbar from "@/components/layout/Navbar";
import { ArrowLeft, LucideUserPlus } from "lucide-react";

type TreeView = {
    coverImageUrl?: string | null;
    name?: string | null;
    description?: string | null;
    createdAt?: string | null;
    createdById?: string | null;
};

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

    const [selectedPerson, setSelectedPerson] = useState<Person | null>(null);
    const [isEditing, setIsEditing] = useState(false);
    const [isViewingDetails, setIsViewingDetails] = useState(false);

    const [tree, setTree] = useState<TreeView | null>(null);
    const [ownerProfile, setOwnerProfile] = useState<UserProfile | null>(null);
    const [graphVersion, setGraphVersion] = useState(0);
    const [pendingNew, setPendingNew] = useState<Person | null>(null);
    const [hoverInspectId, setHoverInspectId] = useState<string | null>(null);
    useEffect(() => {
        if (!treeId) {
            setLoading(false);
            return;
        }
        if (!userId) {
            setLoading(false);
            showToast.error("Bạn cần đăng nhập để xem chi tiết cây");
            return;
        }

        (async () => {
            setLoading(true);
            try {
                const [trees, ps, rs] = await Promise.all([
                    api.listTrees(userId),
                    api.listMembers(userId, treeId),
                    api.listRelationships(userId, treeId),
                ]);

                const found: any = trees.find((t) => t.id === treeId) || null;

                const createdById: string | null =
                    found?.createdBy?.id ??
                    (typeof found?.createdBy === "string" ? found.createdBy : null) ??
                    found?.created_by ??
                    null;

                setTree(
                    found
                        ? {
                            coverImageUrl: found.coverImageUrl ?? null,
                            name: found.name ?? null,
                            description: found.description ?? null,
                            createdAt: found.createdAt ?? null,
                            createdById,
                        }
                        : null
                );

                if (createdById) {
                    try {
                        const owner = await authApi.getUser(createdById);
                        setOwnerProfile(owner?.profile || null);
                    } catch {
                        setOwnerProfile(null);
                    }
                } else {
                    setOwnerProfile(null);
                }

                setPersons(ps);
                setRels(rs);
            } catch (e: any) {
                showToast.error(e?.message || "Không tải được dữ liệu");
            } finally {
                setLoading(false);
            }
        })();
    }, [treeId, userId]);

    const handleAddClick = () => {
        setSelectedPerson(null);
        setMemberOpen(true);
    };

    const handleCreateMember = async (values: MemberFormValues) => {
        if (!treeId || !userId || memberSubmitting) return;
        setMemberSubmitting(true);
        try {
            let avatarUrl = values.avatarUrl;
            if (values.avatarFile) {
                const toastId = showToast.loading("Đang tải ảnh lên…");
                try {
                    avatarUrl = await uploadMemberAvatarToSupabase(values.avatarFile);
                } finally {
                    showToast.dismiss(toastId);
                }
            }
            const hadSomeone = persons.length > 0;

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

            const updatedPersons = [...persons, created];
            setPersons(updatedPersons);
            setGraphVersion((v) => v + 1);

            if (hadSomeone) {
                setPendingNew(created);
                setSource(created);
                setModalOpen(true);
                showToast.success("Đang tìm kiếm mối quan hệ...");
            } else {
                setPendingNew(null);
                setSource(null);
                setModalOpen(false);
                showToast.success("Thêm thành viên thành công.");
            }

            setMemberOpen(false);
        } catch (e: any) {
            showToast.error(e?.message || "Thêm thành viên thất bại");
        } finally {
            setMemberSubmitting(false);
        }
    };

    const closeEditModals = () => {
        setIsEditing(false);
        setMemberOpen(false);
        setIsViewingDetails(false);
        setSelectedPerson(null);
    };

    const handleUpdateMember = async (values: MemberFormValues) => {
        if (!treeId || !userId || !selectedPerson || memberSubmitting) return;
        setMemberSubmitting(true);
        try {
            let avatarUrl = values.avatarUrl;
            if (values.avatarFile) {
                const toastId = showToast.loading("Đang cập nhật ảnh...");
                try {
                    avatarUrl = await uploadMemberAvatarToSupabase(values.avatarFile);
                } finally {
                    showToast.dismiss(toastId);
                }
            }
            const updated = await api.updateMember(userId, treeId, selectedPerson.id, {
                fullName: values.fullName.trim(),
                gender: values.gender || undefined,
                birthDate: values.birthDate || undefined,
                deathDate: values.deathDate || undefined,
                birthPlace: values.birthPlace || undefined,
                deathPlace: values.deathPlace || undefined,
                biography: values.biography || undefined,
                avatarUrl: avatarUrl || undefined,
            });
            setPersons(persons.map((p) => (p.id === updated.id ? updated : p)));
            showToast.success("Cập nhật thông tin thành công");
            closeEditModals();
            setGraphVersion((v) => v + 1);
        } catch (e: any) {
            showToast.error(e?.message || "Có lỗi xảy ra khi cập nhật");
        } finally {
            setMemberSubmitting(false);
        }
    };

    const handleNodeClick = (personId: string) => {
        const person = persons.find((p) => p.id === personId);
        if (person) {
            setSelectedPerson(person);
            setIsViewingDetails(true);
        }
    };

    const handleCloseDetails = () => {
        setIsViewingDetails(false);
        setSelectedPerson(null);
    };

    const handleEditClick = () => {
        setIsViewingDetails(false);
        setIsEditing(true);
    };

    const cancelRelationshipFlow = async () => {
        if (!treeId || !userId || !pendingNew) {
            setModalOpen(false);
            setSource(null);
            return;
        }
        try {
            await api.deleteMember(userId, treeId, pendingNew.id);
        } catch {
        } finally {
            setPersons((prev) => prev.filter((p) => p.id !== pendingNew.id));
            setPendingNew(null);
            setSource(null);
            setModalOpen(false);
            setGraphVersion((v) => v + 1);
            showToast.success("Đã hủy thêm thành viên.");
        }
    };

    const confirmRelationship = async ({
                                           relation,
                                           candidateId,
                                       }: {
        relation: RelationUpper;
        candidateId: string;
    }) => {
        if (!treeId || !source || !userId) return;

        const existingKeys = new Set(
            rels.map((r) => {
                const t = String(r.type).toUpperCase();
                if (t === "SPOUSE" || t === "SIBLING") {
                    const [a, b] = [r.fromPersonId, r.toPersonId].sort();
                    return `PAIR:${a}-${b}:${t}`;
                }
                const parent = t === "PARENT" ? r.fromPersonId : r.toPersonId;
                const child = t === "PARENT" ? r.toPersonId : r.fromPersonId;
                return `PARENT:${parent}->${child}`;
            })
        );

        let person1Id: string;
        let person2Id: string;
        let typeToSend: "PARENT" | "SPOUSE" | "SIBLING";

        if (relation === "PARENT" || relation === "CHILD") {
            const parentId = relation === "PARENT" ? source.id : candidateId;
            const childId = relation === "PARENT" ? candidateId : source.id;
            person1Id = parentId;
            person2Id = childId;
            typeToSend = "PARENT";
        } else {
            const [a, b] = [source.id, candidateId].sort();
            person1Id = a;
            person2Id = b;
            typeToSend = relation;
        }

        const created = await api.createRelationship(userId, treeId, {
            person1Id,
            person2Id,
            relationshipType: typeToSend,
        });

        const newRels: Relationship[] = [
            ...rels,
            {
                id: created?.id ?? crypto.randomUUID?.(),
                fromPersonId: person1Id,
                toPersonId: person2Id,
                type: typeToSend,
            },
        ];

        if (typeToSend === "SPOUSE" || typeToSend === "SIBLING") {
            const [x, y] = [person1Id, person2Id].sort();
            existingKeys.add(`PAIR:${x}-${y}:${typeToSend}`);
        } else {
            existingKeys.add(`PARENT:${person1Id}->${person2Id}`);
        }

        const addParent = async (parentId: string, childId: string) => {
            const k = `PARENT:${parentId}->${childId}`;
            if (existingKeys.has(k)) return;
            const r = await api.createRelationship(userId, treeId, {
                person1Id: parentId,
                person2Id: childId,
                relationshipType: "PARENT",
            });
            newRels.push({
                id: r?.id ?? crypto.randomUUID?.(),
                fromPersonId: parentId,
                toPersonId: childId,
                type: "PARENT",
            });
            existingKeys.add(k);
        };

        const addSiblingPair = async (a: string, b: string) => {
            const [x, y] = [a, b].sort();
            const k = `PAIR:${x}-${y}:SIBLING`;
            if (existingKeys.has(k)) return;
            const r = await api.createRelationship(userId, treeId, {
                person1Id: x,
                person2Id: y,
                relationshipType: "SIBLING",
            });
            newRels.push({
                id: r?.id ?? crypto.randomUUID?.(),
                fromPersonId: x,
                toPersonId: y,
                type: "SIBLING",
            });
            existingKeys.add(k);
        };

        const parentsOf = (id: string) =>
            newRels
                .filter((r) => r.type === "PARENT" && r.toPersonId === id)
                .map((r) => r.fromPersonId);

        const spousesOf = (id: string) =>
            newRels
                .filter((r) => r.type === "SPOUSE" && (r.fromPersonId === id || r.toPersonId === id))
                .map((r) => (r.fromPersonId === id ? r.toPersonId : r.fromPersonId));

        const ensureSiblingClosure = async (childId: string, parentId: string) => {
            const sibs = newRels
                .filter((r) => r.type === "PARENT" && r.fromPersonId === parentId)
                .map((r) => r.toPersonId);
            for (const other of sibs) {
                if (other !== childId) await addSiblingPair(childId, other);
            }
        };

        if (relation === "PARENT" || relation === "CHILD") {
            const parentId = relation === "PARENT" ? person1Id : person2Id;
            const childId = relation === "PARENT" ? person2Id : person1Id;

            const spouseOfParent = rels
                .filter((r) => r.type === "SPOUSE" && (r.fromPersonId === parentId || r.toPersonId === parentId))
                .map((r) => (r.fromPersonId === parentId ? r.toPersonId : r.fromPersonId));

            for (const sp of spouseOfParent) await addParent(sp, childId);

            const parentsOfChild = new Set<string>(
                newRels.filter((r) => r.type === "PARENT" && r.toPersonId === childId).map((r) => r.fromPersonId)
            );
            const sameParentsChildren = new Set<string>([childId]);
            for (const pid of parentsOfChild) {
                newRels
                    .filter((r) => r.type === "PARENT" && r.fromPersonId === pid)
                    .forEach((r) => sameParentsChildren.add(r.toPersonId));
            }
            for (const other of Array.from(sameParentsChildren)) {
                if (other !== childId) await addSiblingPair(childId, other);
            }
        }

        if (relation === "SPOUSE") {
            const childrenOfA = newRels
                .filter((r) => r.type === "PARENT" && r.fromPersonId === person1Id)
                .map((r) => r.toPersonId);
            const childrenOfB = newRels
                .filter((r) => r.type === "PARENT" && r.fromPersonId === person2Id)
                .map((r) => r.toPersonId);

            for (const c of childrenOfA) await addParent(person2Id, c);
            for (const c of childrenOfB) await addParent(person1Id, c);

            const bothChildren = new Set<string>([...childrenOfA, ...childrenOfB]);
            const arr = Array.from(bothChildren);
            for (let i = 0; i < arr.length; i++) {
                for (let j = i + 1; j < arr.length; j++) {
                    await addSiblingPair(arr[i], arr[j]);
                }
            }
        }

        if (relation === "SIBLING") {
            const a = person1Id;
            const b = person2Id;

            const pa = new Set(parentsOf(a));
            const pb = new Set(parentsOf(b));

            if (pa.size) {
                for (const p of pa) {
                    await addParent(p, b);
                    for (const sp of spousesOf(p)) await addParent(sp, b);
                    await ensureSiblingClosure(b, p);
                }
            }

            if (pb.size) {
                for (const p of pb) {
                    await addParent(p, a);
                    for (const sp of spousesOf(p)) await addParent(sp, a);
                    await ensureSiblingClosure(a, p);
                }
            }
        }

        setRels(newRels);
        setGraphVersion((v) => v + 1);
        showToast.success("Đã cập nhật mối quan hệ");
        setModalOpen(false);
        setSource(null);
        setPendingNew(null);
    };

    const fetchSuggestions = useMemo(() => {
        const year = (d?: string | null) => {
            if (!d) return null;
            const t = new Date(d);
            return isNaN(t.getTime()) ? null : t.getUTCFullYear();
        };
        const MIN_PARENT_GAP = 10;

        const existingKeys = new Set(
            rels.map((r) => {
                const t = String(r.type).toUpperCase();
                if (t === "SPOUSE" || t === "SIBLING") {
                    const [a, b] = [r.fromPersonId, r.toPersonId].sort();
                    return `PAIR:${a}-${b}:${t}`;
                }
                const parent = t === "PARENT" ? r.fromPersonId : r.toPersonId;
                const child = t === "PARENT" ? r.toPersonId : r.fromPersonId;
                return `PARENT:${parent}->${child}`;
            })
        );

        return async (
            personId: string
        ): Promise<
            Array<{
                candidateId: string;
                relation: "PARENT" | "CHILD" | "SPOUSE" | "SIBLING";
                confidence: number;
                reasons?: string[];
            }>
        > => {
            if (!treeId || !persons.length) return [];

            const src = persons.find((p) => p.id === personId);
            const ySrc = year(src?.birthDate);

            const others = persons.filter((p) => p.id !== personId);
            if (!others.length) return [];

            const out: Array<{
                candidateId: string;
                relation: any;
                confidence: number;
                reasons?: string[];
            }> = [];

            const BATCH = 5;
            for (let i = 0; i < others.length; i += BATCH) {
                const chunk = others.slice(i, i + BATCH);
                const res = await Promise.all(
                    chunk.map(async (cand) => {
                        try {
                            const raw = (await api.suggestRelationship(userId, treeId, personId, cand.id)) as Array<{
                                type: string;
                                confidence?: number;
                                reasons?: string[];
                            }>;
                            if (!raw?.length) return null;

                            const best = raw.sort((a, b) => (b.confidence ?? 0) - (a.confidence ?? 0))[0];
                            let rel = (best.type || "").toUpperCase() as "PARENT" | "CHILD" | "SPOUSE" | "SIBLING";
                            let conf = best.confidence ?? 0;
                            const reasons = best.reasons ?? [];

                            const yCand = year(cand.birthDate);
                            if (ySrc != null && yCand != null) {
                                const diff = yCand - ySrc;

                                if (rel === "PARENT" || rel === "CHILD") {
                                    if (Math.abs(diff) < MIN_PARENT_GAP) return null;
                                    if (diff > 0 && rel !== "PARENT") {
                                        rel = "PARENT";
                                        conf *= 0.95;
                                    }
                                    if (diff < 0 && rel !== "CHILD") {
                                        rel = "CHILD";
                                        conf *= 0.95;
                                    }
                                }
                            }

                            let key: string;
                            if (rel === "SPOUSE" || rel === "SIBLING") {
                                const [a, b] = [personId, cand.id].sort();
                                key = `PAIR:${a}-${b}:${rel}`;
                            } else {
                                key =
                                    rel === "PARENT"
                                        ? `PARENT:${personId}->${cand.id}`
                                        : `PARENT:${cand.id}->${personId}`;
                            }
                            if (existingKeys.has(key)) return null;

                            if (conf <= 0.3) return null;
                            return {
                                candidateId: cand.id,
                                relation: rel,
                                confidence: conf,
                                reasons,
                            };
                        } catch {
                            return null;
                        }
                    })
                );
                out.push(...(res.filter(Boolean) as any[]));
            }

            return out.sort((a, b) => (b.confidence ?? 0) - (a.confidence ?? 0));
        };
    }, [treeId, userId, persons, rels]);

    const relsNormalized = useMemo(() => {
        return rels.map(r =>
            String(r.type).toUpperCase() === "CHILD"
                ? { ...r, type: "PARENT", fromPersonId: r.toPersonId, toPersonId: r.fromPersonId }
                : { ...r, type: String(r.type).toUpperCase() as any }
        );
    }, [rels]);

    const generationCount = useMemo(() => {
        if (!persons.length) return 0;
        const parentOf: Record<string, string[]> = {};
        const indeg: Record<string, number> = {};
        const ids = persons.map(p => p.id);

        for (const id of ids) indeg[id] = 0;

        for (const r of relsNormalized) {
            if (String(r.type).toUpperCase() !== "PARENT") continue;
            (parentOf[r.fromPersonId] ||= []).push(r.toPersonId);
            indeg[r.toPersonId] = (indeg[r.toPersonId] ?? 0) + 1;
            parentOf[r.fromPersonId] ||= parentOf[r.fromPersonId] || [];
            parentOf[r.toPersonId] ||= parentOf[r.toPersonId] || [];
        }

        const roots = ids.filter(id => (indeg[id] ?? 0) === 0);
        if (roots.length) {
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

            let ans = 1;
            for (const id of ids) ans = Math.max(ans, depth[id] || 1);
            return ans;
        }

        const memo: Record<string, number> = {};
        const visiting = new Set<string>();

        const dfs = (u: string): number => {
            if (memo[u]) return memo[u];
            if (visiting.has(u)) {
                return 1;
            }
            visiting.add(u);
            let best = 1;
            for (const v of parentOf[u] || []) {
                best = Math.max(best, 1 + dfs(v));
            }
            visiting.delete(u);
            memo[u] = best;
            return best;
        };

        let ans = 1;
        for (const id of ids) {
            ans = Math.max(ans, dfs(id));
        }
        return ans;
    }, [persons, relsNormalized]);

    const createdByName =
        ownerProfile?.fullName?.trim() ||
        user?.profile?.fullName?.trim?.() ||
        user?.fullName?.trim?.() ||
        user?.username ||
        "—";

    const anyModalOpen = memberOpen || isEditing || isViewingDetails || modalOpen;

    type Highlight = {
        nodes: Set<string>;
        edges: Set<string>;
        couple: Set<string>;
        focusChild?: string;
    };

    const highlight = useMemo<Highlight>(() => {
        if (!hoverInspectId) return { nodes: new Set(), edges: new Set(), couple: new Set() };

        const parents = relsNormalized
            .filter(r => r.type === "PARENT" && r.toPersonId === hoverInspectId)
            .map(r => r.fromPersonId);

        const children = relsNormalized
            .filter(r => r.type === "PARENT" && r.fromPersonId === hoverInspectId)
            .map(r => r.toPersonId);

        const edges = new Set<string>();
        parents.forEach(p => edges.add(`${p}->${hoverInspectId}`));
        children.forEach(c => edges.add(`${hoverInspectId}->${c}`));

        const couple = new Set<string>();
        if (parents.length === 2 && relsNormalized.some(r => {
            if (r.type !== "SPOUSE") return false;
            const a = r.fromPersonId, b = r.toPersonId;
            return (a === parents[0] && b === parents[1]) || (a === parents[1] && b === parents[0]);
        })) {
            couple.add(parents[0]); couple.add(parents[1]);
        }

        return { nodes: new Set([...parents, ...children]), edges, couple, focusChild: hoverInspectId };
    }, [hoverInspectId, relsNormalized]);

    return (
        <div className="relative min-h-screen">
            <img src={bg} alt="" className="absolute inset-0 w-full h-full object-cover -z-10" />
            <div className="absolute inset-0 bg-slate-900/40 -z-10" />

            <div className="relative z-20">
                <Navbar />
            </div>

            <div className="relative z-10 px-4 md:px-6">
                <div className="w-full rounded-xl text-white">
                    <div className="flex items-center justify-between px-4 py-3">
                        <button
                            onClick={() => navigate("/dashboard")}
                            className="inline-flex items-center gap-2 hover:bg-white/10 px-3 py-1.5 rounded-lg transition-colors"
                            title="Quay lại"
                        >
                            <ArrowLeft className="w-4 h-4" />
                            <span>Quay lại</span>
                        </button>
                        <button
                            onClick={handleAddClick}
                            className="inline-flex items-center gap-2 rounded-lg bg-white/20 hover:bg-white/30 px-4 py-2 shadow-sm hover:shadow transition-all"
                            title="Thêm thành viên"
                        >
                            <LucideUserPlus className="w-5 h-5" />
                        </button>
                    </div>
                </div>
            </div>

            <div className="grid grid-cols-12 gap-4 p-4 pt-24">
                <DetailsSidebar
                    coverImageUrl={tree?.coverImageUrl ?? null}
                    name={tree?.name ?? null}
                    description={tree?.description ?? null}
                    createdByName={createdByName}
                    createdAt={tree?.createdAt ?? null}
                    memberCount={loading ? 0 : persons.length}
                    generationCount={loading ? 0 : generationCount}
                />

                <main className="col-span-12 md:col-span-9 bg-white rounded-xl shadow p-3">
                    {!treeId ? (
                        <div className="text-sm text-slate-500">Không tìm thấy treeId trong URL.</div>
                    ) : loading ? (
                        <div className="text-gray-500 text-sm">Đang tải…</div>
                    ) : (
                        <TreeGraph
                            key={graphVersion}
                            persons={persons}
                            relationships={relsNormalized}
                            onNodeClick={handleNodeClick}
                            selectedNodeId={anyModalOpen ? null : selectedPerson?.id}
                            onEmptyClick={handleAddClick}
                            highlightNodeIds={[...highlight.nodes]}
                            highlightEdges={[...highlight.edges]}
                            onHoverNode={setHoverInspectId}
                            highlightCoupleIds={[...highlight.couple]}
                            focusChildId={highlight.focusChild}
                        />
                    )}
                </main>
            </div>

            <MemberModal
                open={memberOpen || isEditing}
                onClose={closeEditModals}
                onSubmit={isEditing ? handleUpdateMember : handleCreateMember}
                submitting={memberSubmitting}
                title={isEditing ? "Chỉnh sửa thông tin" : "Thêm thành viên"}
                initialValues={
                    isEditing && selectedPerson
                        ? {
                            fullName: selectedPerson.fullName,
                            gender: selectedPerson.gender as any,
                            birthDate: selectedPerson.birthDate,
                            deathDate: selectedPerson.deathDate,
                            birthPlace: selectedPerson.birthPlace,
                            deathPlace: selectedPerson.deathPlace,
                            biography: selectedPerson.biography,
                            avatarUrl: selectedPerson.avatarUrl,
                        }
                        : undefined
                }
            />
            <PersonDetailsModal
                isOpen={isViewingDetails}
                person={selectedPerson}
                persons={persons}
                relationships={rels}
                onClose={handleCloseDetails}
                onEditClick={handleEditClick}
                onHoverPerson={setHoverInspectId}
            />
            {source && (
                <RelationshipModal
                    isOpen={modalOpen}
                    onClose={() => {
                        setModalOpen(false);
                        setSource(null);
                        setPendingNew(null);
                    }}
                    onCancel={cancelRelationshipFlow}
                    source={source}
                    persons={persons}
                    fetchSuggestions={fetchSuggestions}
                    onConfirm={confirmRelationship}
                />
            )}
        </div>
    );
}