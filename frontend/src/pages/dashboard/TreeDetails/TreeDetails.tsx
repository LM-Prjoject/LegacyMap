import { useEffect, useMemo, useRef, useState } from "react";
import { useParams, useNavigate } from "react-router-dom";
import TreeGraph from "@/components/familyTree/TreeGraph";
import RelationshipModal, { type RelationUpper } from "@/components/familyTree/relaModal/RelationshipModal";
import MemberModal, { type MemberFormValues } from "@/components/familyTree/memberModal/MemberModal";
import PersonDetailsModal from "@/components/familyTree/PersonDetailsModal";
import ShareTreeModal from "@/components/familyTree/ShareTreeModal";
import DetailsSidebar from "@/pages/dashboard/TreeDetails/DetailsSidebar";
import { TreeHistoryModal } from '@/components/familyTree/historyModal/TreeHistoryModal';
import api, { type Person, type Relationship, type TreeStatistics, exportTreePdfWithImage } from "@/api/trees";
import { showToast } from "@/lib/toast";
import { uploadMemberAvatarToSupabase } from "@/lib/upload";
import { authApi, type UserProfile } from "@/api/auth";
import Navbar from "@/components/layout/Navbar";
import MemberListModal from "@/components/familyTree/MemberListModal";
import { ArrowLeft, LucideUserPlus, Share2, Download, History, Edit } from "lucide-react";
import * as htmlToImage from "html-to-image";
import { personLinkApi } from "@/api/personLink";

type TreeView = {
    coverImageUrl?: string | null;
    name?: string | null;
    description?: string | null;
    createdAt?: string | null;
    createdById?: string | null;
};

const buildExistingRelationshipKeys = (rels: Relationship[]): Set<string> => {
    return new Set(
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
};


export default function TreeDetails() {
    const { treeId } = useParams<{ treeId: string }>();
    const navigate = useNavigate();
    const user = JSON.parse(localStorage.getItem("user") || "{}");
    const userId: string = user?.id;

    const [persons, setPersons] = useState<Person[]>([]);
    const [rels, setRels] = useState<Relationship[]>([]);
    const [statistics, setStatistics] = useState<TreeStatistics | null>(null);
    const [loading, setLoading] = useState(true);

    const [memberOpen, setMemberOpen] = useState(false);
    const [memberSubmitting, setMemberSubmitting] = useState(false);

    const [modalOpen, setModalOpen] = useState(false);
    const [source, setSource] = useState<Person | null>(null);

    const [selectedPerson, setSelectedPerson] = useState<Person | null>(null);
    const [isEditing, setIsEditing] = useState(false);
    const [isViewingDetails, setIsViewingDetails] = useState(false);
    const [showHistory, setShowHistory] = useState(false);


    const [tree, setTree] = useState<TreeView | null>(null);
    const [ownerProfile, setOwnerProfile] = useState<UserProfile | null>(null);
    const [graphVersion, setGraphVersion] = useState(0);
    const [readOnly, setReadOnly] = useState(false);
    const [ownsTree, setOwnsTree] = useState(false);
    const [pendingNew, setPendingNew] = useState<Person | null>(null);
    const [isInAddFlow, setIsInAddFlow] = useState(false);
    const [shareModalOpen, setShareModalOpen] = useState(false);
    const [pendingInvite, setPendingInvite] = useState<{ email?: string; send?: boolean } | null>(null);
    const treeWrapperRef = useRef<HTMLDivElement | null>(null);
    const [memberListOpen, setMemberListOpen] = useState(false);

    // Helper function để refresh dữ liệu mà không reload trang
    const refreshTreeData = async () => {
        if (!treeId || !userId) return;
        try {
            const [ps, rs, stats] = await Promise.all([
                readOnly ? api.listMembersForViewer(userId, treeId) : api.listMembers(userId, treeId),
                readOnly ? api.listRelationshipsForViewer(userId, treeId) : api.listRelationships(userId, treeId),
                api.getTreeStatistics(userId, treeId),
            ]);
            setPersons(ps);
            setRels(rs);
            setStatistics(stats);
            setGraphVersion((v) => v + 1);
        } catch (e: any) {
            console.warn("Không thể refresh dữ liệu:", e);
        }
    };

    useEffect(() => {

        const urlParams = new URLSearchParams(window.location.search);
        const fromShare = urlParams.get('fromShare') === 'true';

        if (fromShare && treeId && userId) {

            api.saveSharedTreeToDashboard(userId, treeId)
                .then(() => {
                    showToast.success("Đã lưu cây vào dashboard của bạn");
                    const newUrl = window.location.pathname;
                    window.history.replaceState({}, '', newUrl);
                })
                .catch(e => {
                    showToast.error(e?.message || "Không thể lưu cây");
                });
        }

        localStorage.removeItem('pendingTreeId');
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
                const [owned, viewable] = await Promise.all([
                    api.listTrees(userId).catch(() => []),
                    api.listViewableTrees(userId).catch(() => [] as any[]),
                ]);
                const allTrees: any[] = [...owned, ...viewable];
                const found: any = allTrees.find((t) => t.id === treeId) || null;
                setOwnsTree(!!owned.find((t: any) => t?.id === treeId));

                let createdById: string | null;
                try {
                    createdById = await api.getTreeOwner(treeId);
                } catch {
                    createdById = null;
                }

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
                        const basic = await api.getPublicUserBasic(createdById);
                        if (basic && (basic.fullName || basic.username)) {
                            setOwnerProfile({
                                ...(ownerProfile as any),
                                fullName: basic.fullName || basic.username || "",
                            } as any);
                        } else {
                            const owner = await authApi.getUser(createdById);
                            setOwnerProfile((owner as any)?.profile || (owner as any) || null);
                        }
                    } catch {
                        try {
                            const owner = await authApi.getUser(createdById);
                            setOwnerProfile((owner as any)?.profile || (owner as any) || null);
                        } catch {
                            setOwnerProfile(null);
                        }
                    }
                } else {
                    setOwnerProfile(null);
                }

                try {
                    const [ps, rs, stats] = await Promise.all([
                        api.listMembers(userId, treeId),
                        api.listRelationships(userId, treeId),
                        api.getTreeStatistics(userId, treeId),
                    ]);
                    setPersons(ps);
                    setRels(rs);
                    setStatistics(stats);
                    setReadOnly(false);
                } catch (err: any) {
                    const msg = String(err?.message || "").toLowerCase();
                    if (msg.includes("unauthorized") || msg.includes("không") || msg.includes("forbidden")) {
                        const [psV, rsV, statsV] = await Promise.all([
                            api.listMembersForViewer(userId, treeId),
                            api.listRelationshipsForViewer(userId, treeId),
                            api.getTreeStatistics(userId, treeId),
                        ]);
                        setPersons(psV);
                        setRels(rsV);
                        setStatistics(statsV);
                        setReadOnly(true);
                    } else {
                        throw err;
                    }
                }
            } catch (e: any) {
                showToast.error(e?.message || "Không tải được dữ liệu");
            } finally {
                setLoading(false);
            }
        })();
    }, [treeId, userId]);

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
                phone: values.phone || undefined,
                email: values.email || undefined,
            });

            const updatedPersons = [...persons, created];
            setPersons(updatedPersons);
            setGraphVersion((v) => v + 1);

            if (hadSomeone) {
                setPendingNew(created);
                setSource(created);
                setModalOpen(true);
                showToast.success("Đang tìm kiếm mối quan hệ...");
                setMemberOpen(false);
                setIsEditing(false);
                setIsInAddFlow(true);
                // store invite choice to trigger after relationship is created
                setPendingInvite({ email: values.email?.trim(), send: !!values.sendInvite });
            } else {
                setPendingNew(null);
                setSource(null);
                setModalOpen(false);
                setMemberOpen(false);
                showToast.success("Thêm thành viên thành công.");
                // No relationship flow; clear invite choice
                setPendingInvite(null);
                // Cập nhật statistics sau khi thêm thành viên
                await refreshTreeData();
            }

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
                birthPlace: values.birthPlace,
                deathPlace: values.deathPlace,
                biography: values.biography,
                avatarUrl: avatarUrl || undefined,
                phone: values.phone,
                email: values.email,
            });
            setPersons(persons.map((p) => (p.id === updated.id ? updated : p)));
            showToast.success("Cập nhật thông tin thành công");
            closeEditModals();
            setGraphVersion((v) => v + 1);
            // Cập nhật statistics sau khi cập nhật thành viên
            await refreshTreeData();
            if (
                isInAddFlow &&
                pendingNew &&
                selectedPerson &&
                selectedPerson.id === pendingNew.id
            ) {
                setPendingNew(updated);
                setSource(updated);
                setMemberOpen(false);
                setIsEditing(false);
                setModalOpen(true);
            } else {
                closeEditModals();
            }
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
            setIsEditing(false);
            setIsInAddFlow(false);
        }
    };

    const handleCloseDetails = () => {
        setIsViewingDetails(false);
        setSelectedPerson(null);
    };

    const handleEditClick = () => {
        if (readOnly) return;
        setIsViewingDetails(false);
        setIsEditing(true);
        setIsInAddFlow(false);
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
            // Cập nhật toàn bộ dữ liệu sau khi hủy thêm thành viên
            await refreshTreeData();
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

        const existingKeys = buildExistingRelationshipKeys(rels);

        let person1Id: string;
        let person2Id: string;
        let typeToSend: "PARENT" | "SPOUSE" | "SIBLING";
        let parentId: string | null = null;
        let childId: string | null = null;

        if (relation === "PARENT" || relation === "CHILD") {
            const pId = relation === "PARENT" ? source.id : candidateId;
            const cId = relation === "PARENT" ? candidateId : source.id;

            parentId = pId;
            childId = cId;

            person1Id = pId;
            person2Id = cId;
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

        // After relationship is created, send pending invite if requested
        try {
            if (pendingInvite?.send && pendingInvite.email?.trim()) {
                const res = await personLinkApi.invite(source.id, userId, pendingInvite.email.trim());
                const result = res?.result;
                if (result?.status === "APPROVED") {
                    showToast.success(result.message || "Đã tự xác minh thành công");
                } else {
                    showToast.success(result?.message || "Đã gửi thông báo mời liên kết");
                }

            }
        } catch (inviteErr: any) {
            console.warn("Invite after relationship failed", inviteErr?.message || inviteErr);
            showToast.warning("Không gửi được thông báo mời, bạn có thể thử lại sau");
        } finally {
            setPendingInvite(null);
        }

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

        const addParent = async (pId: string, cId: string) => {
            const k = `PARENT:${pId}->${cId}`;
            if (existingKeys.has(k)) return;
            const r = await api.createRelationship(userId, treeId, {
                person1Id: pId,
                person2Id: cId,
                relationshipType: "PARENT",
            });
            newRels.push({
                id: r?.id ?? crypto.randomUUID?.(),
                fromPersonId: pId,
                toPersonId: cId,
                type: "PARENT",
            } as Relationship);

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
            } as Relationship);

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

        if (parentId && childId) {
            const spouseOfParent = rels
                .filter(
                    (r) =>
                        String(r.type).toUpperCase() === "SPOUSE" &&
                        (r.fromPersonId === parentId || r.toPersonId === parentId)
                )
                .map((r) => (r.fromPersonId === parentId ? r.toPersonId : r.fromPersonId));

            // Only auto-add the other parent if there is exactly ONE spouse.
            // If multiple spouses exist for the chosen parent, we defer to the modal
            // to require an explicit selection to avoid assigning all spouses.
            if (spouseOfParent.length === 1) {
                await addParent(spouseOfParent[0], childId);
            }

            const parentsOfChild = new Set<string>(
                newRels
                    .filter((r) => String(r.type).toUpperCase() === "PARENT" && r.toPersonId === childId)
                    .map((r) => r.fromPersonId)
            );
            const sameParentsChildren = new Set<string>([childId]);
            for (const pid of parentsOfChild) {
                newRels
                    .filter((r) => String(r.type).toUpperCase() === "PARENT" && r.fromPersonId === pid)
                    .forEach((r) => sameParentsChildren.add(r.toPersonId));
            }
            for (const other of Array.from(sameParentsChildren)) {
                if (other !== childId) await addSiblingPair(childId, other);
            }

            const otherParents = new Set(
                rels
                    .filter(
                        (r) =>
                            String(r.type).toUpperCase() === "PARENT" &&
                            r.toPersonId === childId
                    )
                    .map((r) => r.fromPersonId)
            );
            otherParents.delete(parentId);

            for (const op of otherParents) {
                const [a, b] = [parentId, op].sort();
                const key = `PAIR:${a}-${b}:SPOUSE`;
                if (existingKeys.has(key)) continue;

                const createdSpouse = await api.createRelationship(userId, treeId, {
                    person1Id: a,
                    person2Id: b,
                    relationshipType: "SPOUSE",
                });

                newRels.push({
                    id: createdSpouse?.id ?? crypto.randomUUID?.(),
                    fromPersonId: a,
                    toPersonId: b,
                    type: "SPOUSE",
                } as Relationship);

                existingKeys.add(key);
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
        // Cập nhật toàn bộ dữ liệu sau khi tạo mối quan hệ
        await refreshTreeData();
    };

    const fetchSuggestions = useMemo(() => {
        const year = (d?: string | null) => {
            if (!d) return null;
            const t = new Date(d);
            return isNaN(t.getTime()) ? null : t.getUTCFullYear();
        };

        const MIN_PARENT_GAP = 18;

        return async (
            personId: string
        ): Promise<
            Array<{ candidateId: string; relation: "PARENT" | "CHILD" | "SPOUSE" | "SIBLING"; confidence: number; reasons?: string[] }>
        > => {
            if (!treeId || !persons.length) return [];

            const src = persons.find((p) => p.id === personId);
            const ySrc = year(src?.birthDate);

            const others = persons.filter((p) => p.id !== personId);
            if (!others.length) return [];

            const existingKeys = buildExistingRelationshipKeys(rels);

            const out: Array<{ candidateId: string; relation: any; confidence: number; reasons?: string[] }> = [];

            try {
                const batch = await api.suggestForSource(userId, treeId, personId, others);
                for (const item of batch) {
                    const cand = others.find(p => p.id === item.candidateId);
                    if (!cand) continue;
                    let rel = (item.relation || "").toUpperCase() as "PARENT" | "CHILD" | "SPOUSE" | "SIBLING";
                    let conf = item.confidence ?? 0;

                    const yCand = year(cand.birthDate);
                    if (ySrc != null && yCand != null) {
                        const diff = yCand - ySrc;
                        if (rel === "PARENT" || rel === "CHILD") {
                            if (Math.abs(diff) < MIN_PARENT_GAP) {
                                try { console.log("[fetchSuggestions] reject age-gap-too-small", { sourceId: personId, candId: cand.id, diff, MIN_PARENT_GAP, rel }); } catch { }
                                continue;
                            }
                            if (diff > 0 && rel !== "PARENT") {
                                rel = "PARENT";
                                conf *= 0.95;
                                try { console.log("[fetchSuggestions] orient->PARENT", { sourceId: personId, candId: cand.id, diff, conf }); } catch { }
                            }
                            if (diff < 0 && rel !== "CHILD") {
                                rel = "CHILD";
                                conf *= 0.95;
                                try { console.log("[fetchSuggestions] orient->CHILD", { sourceId: personId, candId: cand.id, diff, conf }); } catch { }
                            }
                        }
                    }

                    // Enforce spouse suggestion only for opposite genders and when both genders are known
                    if (rel === "SPOUSE") {
                        const gSrc = String(src?.gender || "").toUpperCase();
                        const gCand = String(cand.gender || "").toUpperCase();
                        if (!gSrc || !gCand || gSrc === gCand) {
                            try { console.log("[fetchSuggestions] reject spouse same/missing gender", { gSrc, gCand, personId, candId: cand.id }); } catch { }
                            continue;
                        }
                    }

                    let key: string;
                    if (rel === "SPOUSE" || rel === "SIBLING") {
                        const [x, y] = [personId, cand.id].sort();
                        key = `PAIR:${x}-${y}:${rel}`;
                    } else {
                        key = rel === "PARENT" ? `PARENT:${personId}->${cand.id}` : `PARENT:${cand.id}->${personId}`;
                    }
                    if (existingKeys.has(key)) {
                        try { console.log("[fetchSuggestions] reject duplicate", { key }); } catch { }
                        continue;
                    }

                    if (conf <= 0.3) {
                        try { console.log("[fetchSuggestions] reject low-confidence", { conf }); } catch { }
                        continue;
                    }
                    out.push({ candidateId: cand.id, relation: rel, confidence: conf });
                }
            } catch (e) {
                try { console.log("[fetchSuggestions] batch-error", e); } catch { }
            }

            const sorted = out.sort((a, b) => (b.confidence ?? 0) - (a.confidence ?? 0));
            try { console.log("[fetchSuggestions] final", { sourceId: personId, size: sorted.length, top: sorted[0] }); } catch { }
            return sorted;
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

    const createdByName = (ownerProfile?.fullName || "—").toString().trim();

    const anyModalOpen = memberOpen || isEditing || isViewingDetails || modalOpen;

    const prepareDelete = async (personId: string): Promise<{ count: number }> => {
        // Compute orphan count using anchor roots derived from the target's ancestors (bloodline-only)
        try {
            const ids = persons.map(p => p.id);
            if (!ids.length) return { count: 0 };

            // Build parent-child adjacency (both directions) on FULL graph first (before removal)
            const undirectedFull = new Map<string, Set<string>>();
            const parentOf = new Map<string, Set<string>>(); // parent -> children
            const childOf = new Map<string, Set<string>>();  // child -> parents
            for (const id of ids) {
                undirectedFull.set(id, new Set());
            }
            for (const r of relsNormalized) {
                if (String(r.type).toUpperCase() !== "PARENT") continue;
                const p = r.fromPersonId, c = r.toPersonId;
                if (!undirectedFull.has(p) || !undirectedFull.has(c)) continue;
                undirectedFull.get(p)!.add(c);
                undirectedFull.get(c)!.add(p);
                (parentOf.get(p) || parentOf.set(p, new Set()).get(p))!.add(c);
                (childOf.get(c) || childOf.set(c, new Set()).get(c))!.add(p);
            }

            // Find anchor roots: go UP from the target via parents until nodes without parent
            const ancestorRoots = new Set<string>();
            const upVisited = new Set<string>();
            const upQ: string[] = [personId];
            while (upQ.length) {
                const u = upQ.shift()!;
                if (upVisited.has(u)) continue;
                upVisited.add(u);
                const parents = childOf.get(u);
                if (!parents || parents.size === 0) {
                    // u has no parent -> root candidate
                    ancestorRoots.add(u);
                } else {
                    for (const par of parents) upQ.push(par);
                }
            }

            // Remove the target from the graph and build undirected graph for reachability
            const graph = new Map<string, Set<string>>();
            for (const id of ids) if (id !== personId) graph.set(id, new Set());
            for (const r of relsNormalized) {
                if (String(r.type).toUpperCase() !== "PARENT") continue;
                const p = r.fromPersonId, c = r.toPersonId;
                if (p === personId || c === personId) continue;
                if (graph.has(p) && graph.has(c)) {
                    graph.get(p)!.add(c);
                    graph.get(c)!.add(p);
                }
            }

            // BFS from anchor roots (excluding the target if it happens to be among them)
            const startRoots = Array.from(ancestorRoots).filter(id => id !== personId && graph.has(id));
            if (startRoots.length === 0) return { count: 0 };
            const reachable = new Set<string>();
            const dq: string[] = [];
            for (const r of startRoots) { if (!reachable.has(r)) { reachable.add(r); dq.push(r); } }
            while (dq.length) {
                const u = dq.shift()!;
                for (const v of Array.from(graph.get(u) ?? [])) {
                    if (!reachable.has(v)) { reachable.add(v); dq.push(v); }
                }
            }

            const orphan = ids.filter(id => id !== personId && !reachable.has(id));
            return { count: orphan.length };
        } catch {
            return { count: 0 };
        }
    };

    const confirmDeleteFromDetails = async (personId: string) => {
        if (!treeId || !userId) return;
        const toastId = showToast.loading("Đang xoá an toàn và dọn nhánh…");
        try {
            const result = await api.deleteMemberSafe(userId, treeId, personId);
            
            // Nếu backend trả về dữ liệu đã cập nhật, sử dụng luôn
            if (result.persons && result.relationships && result.statistics) {
                setPersons(result.persons);
                setRels(result.relationships);
                setStatistics(result.statistics);
            } else {
                // Fallback: refresh toàn bộ dữ liệu
                await refreshTreeData();
            }
            
            setGraphVersion((v) => v + 1);
            if (selectedPerson && selectedPerson.id === personId) {
                setIsViewingDetails(false);
                setSelectedPerson(null);
            }
            showToast.success(result.message || "Đã xoá và dọn nhánh không còn nối với gốc");
        } catch (e: any) {
            showToast.error(e?.message || "Xoá an toàn thất bại");
        } finally {
            showToast.dismiss(toastId);
        }
    };

    const handleAddClick = () => {
        if (readOnly) return;
        setSelectedPerson(null);
        setIsEditing(false);
        setIsInAddFlow(true);
        setMemberOpen(true);
    };

    const handleExport = async () => {
        if (!treeId) {
            showToast.error("Thiếu ID cây gia phả.");
            return;
        }
        if (!treeWrapperRef.current) {
            showToast.error("Không tìm thấy khu vực cây để chụp.");
            return;
        }

        try {
            const dataUrl = await htmlToImage.toPng(treeWrapperRef.current, {
                quality: 1,
                pixelRatio: 2,
            });

            const res = await fetch(dataUrl);
            const imgBlob = await res.blob();

            const pdfBlob = await exportTreePdfWithImage(treeId, imgBlob);

            const url = URL.createObjectURL(pdfBlob);
            const a = document.createElement("a");
            a.href = url;
            a.download = `cay-gia-pha-${treeId}.pdf`;
            a.click();
            URL.revokeObjectURL(url);
        } catch (e: any) {
            console.error(e);
            showToast.error(e?.message || "Xuất PDF thất bại, vui lòng thử lại.");
        }
    };
    return (
        <div className="relative min-h-screen">
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

                        <div className="flex items-center gap-2">
                            <button
                                onClick={() => setShowHistory(true)}
                                className="inline-flex items-center gap-2 rounded-lg bg-white/20 hover:bg-white/30 px-3 py-1.5 shadow-sm hover:shadow transition-all"
                                title="Lịch sử"
                            >
                                <History className="w-5 h-5" />
                            </button>
                            <button
                                onClick={handleExport}
                                className="inline-flex items-center gap-2 rounded-lg bg-white/20 hover:bg-white/30 px-3 py-1.5 shadow-sm hover:shadow transition-all"
                                title="Tải xuống"
                                disabled={loading}
                            >
                                <Download size={20} />
                            </button>

                            {!readOnly && (
                                <>
                                    <button
                                        onClick={() => setShareModalOpen(true)}
                                        className="inline-flex items-center gap-2 rounded-lg bg-white/20 hover:bg-white/30 px-3 py-1.5 shadow-sm hover:shadow transition-all"
                                        title="Chia sẻ cây gia phả"
                                    >
                                        <Share2 size={20} />
                                    </button>
                                    <button
                                        onClick={handleAddClick}
                                        className="inline-flex items-center gap-2 rounded-lg bg-white/20 hover:bg-white/30 px-3 py-1.5 shadow-sm hover:shadow transition-all"
                                        title="Thêm thành viên"
                                    >
                                        <LucideUserPlus className="w-5 h-5" />
                                    </button>
                                </>
                            )}

                            {readOnly && (
                                <button
                                    onClick={async () => {
                                        if (!treeId || !userId) return;
                                        try {
                                            await api.requestEditAccess(userId, treeId);
                                            showToast.success("Đã gửi yêu cầu quyền chỉnh sửa");
                                        } catch (e: any) {
                                            showToast.error(e?.message || "Gửi yêu cầu thất bại");
                                        }
                                    }}
                                    className="inline-flex items-center gap-2 rounded-lg bg-blue-600 hover:bg-blue-700 px-4 py-2 shadow-sm hover:shadow transition-all text-white"
                                    title="Yêu cầu quyền chỉnh sửa"
                                >
                                    <Edit size="20" /> Yêu cầu quyền edit
                                </button>
                            )}
                        </div>
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
                    memberCount={loading ? 0 : (statistics?.memberCount ?? persons.length)}
                    generationCount={loading ? 0 : generationCount}
                    onMembersClick={() => setMemberListOpen(true)}
                />

                <main ref={treeWrapperRef}
                    className="col-span-12 md:col-span-9 bg-white rounded-xl shadow p-3">
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
                            onEmptyClick={!loading && !readOnly ? handleAddClick : undefined}
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
                            phone: selectedPerson.phone,
                            email: selectedPerson.email,
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
                onEditClick={readOnly ? () => { } : handleEditClick}
                onPrepareDelete={readOnly ? undefined : prepareDelete}
                onDelete={readOnly ? () => { } : (pId) => confirmDeleteFromDetails(pId)}
                readOnly={readOnly}
                isOwner={!readOnly && (ownsTree || (!!tree?.createdById && tree?.createdById === userId))}
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
                    onBack={() => {
                        if (!pendingNew) return;
                        setSelectedPerson(pendingNew);
                        setIsEditing(true);
                        setMemberOpen(true);
                        setModalOpen(false);
                        setIsInAddFlow(true);
                    }}
                    source={source}
                    persons={persons}
                    relationships={rels}
                    fetchSuggestions={fetchSuggestions}
                    onConfirm={confirmRelationship}
                />
            )}

            <MemberListModal
                open={memberListOpen}
                onClose={() => setMemberListOpen(false)}
                persons={persons}
                relationships={relsNormalized as any}
                title="Danh sách thành viên"
            />

            <ShareTreeModal
                isOpen={shareModalOpen}
                onClose={() => setShareModalOpen(false)}
                treeId={treeId || ""}
                userId={userId}
                treeName={tree?.name || undefined}
            />
            <TreeHistoryModal
                treeId={treeId || ""}
                isOpen={showHistory}
                onClose={() => setShowHistory(false)}
            />
        </div>
    );
}