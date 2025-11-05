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

    const [selectedPerson, setSelectedPerson] = useState<Person | null>(null);
    const [isEditing, setIsEditing] = useState(false);
    const [isViewingDetails, setIsViewingDetails] = useState(false);
    const [tree, setTree] = useState<{
        coverImageUrl?: string | null;
        name?: string | null;
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

            // Update the persons list with the new member
            const updatedPersons = [...persons, created];
            setPersons(updatedPersons);

            // Set the newly created person as the source for relationship suggestions
            setSource(created);

            // Show the relationship modal
            setModalOpen(true);

            // Close the member modal
            setMemberOpen(false);

            // Show success message
            showToast.success("Thêm thành viên thành công. Đang tìm kiếm mối quan hệ...");
        } catch (e: any) {
            showToast.error(e?.message || "Thêm thành viên thất bại");
        } finally {
            setMemberSubmitting(false);
        }
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
                } catch (err: any) {
                    showToast.error(`Lỗi tải ảnh lên: ${err?.message || "Không thể cập nhật ảnh"}`);
                    return;
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

            setPersons(persons.map(p => p.id === updated.id ? updated : p));
            setSelectedPerson(updated);
            showToast.success("Cập nhật thông tin thành công");
            setIsEditing(false);
        } catch (e: any) {
            showToast.error(e?.message || "Có lỗi xảy ra khi cập nhật");
        } finally {
            setMemberSubmitting(false);
        }
    };

    const handleNodeClick = (personId: string) => {
        const person = persons.find(p => p.id === personId);
        if (person) {
            setSelectedPerson(person);
            setIsViewingDetails(true);
        }
    };

    const handleCloseDetails = () => {
        setIsViewingDetails(false);
    };

    const handleEditClick = () => {
        setIsViewingDetails(false);
        setIsEditing(true);
    };

    const confirmRelationship = async ({ relation, candidateId }: { relation: RelationUpper; candidateId: string }) => {
        if (!treeId || !source || !userId) return;

        let person1Id = source.id;
        let person2Id = candidateId;
        if (relation === "PARENT") { person1Id = candidateId; person2Id = source.id; }
        else if (relation === "CHILD") { person1Id = source.id; person2Id = candidateId; }

        // Check for existing relationships
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

        // Create the primary relationship
        const r = await api.createRelationship(userId, treeId, {
            person1Id,
            person2Id,
            relationshipType: relation,
        });

        const newRels = [
            ...rels,
            { id: r?.id ?? crypto.randomUUID?.(), fromPersonId: person1Id, toPersonId: person2Id, type: relation },
        ];

        // If this is a parent-child relationship, check for spouse
        if (relation === "PARENT" || relation === "CHILD") {
            const parentId = relation === "PARENT" ? person1Id : person2Id;
            const childId = relation === "PARENT" ? person2Id : person1Id;

            // Find spouse relationships for the parent
            const spouseRels = rels.filter(rel =>
                rel.type === "SPOUSE" &&
                (rel.fromPersonId === parentId || rel.toPersonId === parentId)
            );

            // Create parent-child relationship for each spouse
            for (const rel of spouseRels) {
                const spouseId = rel.fromPersonId === parentId ? rel.toPersonId : rel.fromPersonId;
                const spouseKey = `PARENT:${spouseId}->${childId}`;

                if (!existingKeys.has(spouseKey)) {
                    try {
                        const spouseRel = await api.createRelationship(userId, treeId, {
                            person1Id: spouseId,
                            person2Id: childId,
                            relationshipType: "PARENT",
                        });

                        newRels.push({
                            id: spouseRel?.id ?? crypto.randomUUID?.(),
                            fromPersonId: spouseId,
                            toPersonId: childId,
                            type: "PARENT"
                        });
                        showToast.success(`Đã thêm quan hệ với ${persons.find(p => p.id === spouseId)?.fullName || 'đối tác'}`);
                    } catch (error) {
                        console.error("Lỗi khi tạo quan hệ với đối tác:", error);
                    }
                }
            }
        }

        setRels(newRels);
        showToast.success("Đã nối mối quan hệ");
    };

    const fetchSuggestions = useMemo(() => {
        return async (personId: string): Promise<Array<{
            candidateId: string;
            relation: 'PARENT' | 'CHILD' | 'SPOUSE' | 'SIBLING';
            confidence: number;
            reasons?: string[];
        }>> => {
            if (!treeId || !persons.length) return [];
            try {
                // Get suggestions for this person with all other persons in the tree
                const results: Array<{
                    candidateId: string;
                    relation: 'PARENT' | 'CHILD' | 'SPOUSE' | 'SIBLING';
                    confidence: number;
                    reasons: string[];
                }> = [];

                // Only check against other persons (not self)
                const otherPersons = persons.filter(p => p.id !== personId);

                // Process suggestions in batches to avoid too many API calls
                const BATCH_SIZE = 5;
                for (let i = 0; i < otherPersons.length; i += BATCH_SIZE) {
                    const batch = otherPersons.slice(i, i + BATCH_SIZE);
                    const batchPromises = batch.map(async (person) => {
                        try {
                            const suggestions = await api.suggestRelationship(
                                userId,
                                treeId,
                                personId,
                                person.id
                            );

                            // Get the best suggestion for this pair
                            const bestSuggestion = suggestions.sort(
                                (a, b) => (b.confidence || 0) - (a.confidence || 0)
                            )[0];

                            if (bestSuggestion && bestSuggestion.confidence && bestSuggestion.confidence > 0.3) {
                                return {
                                    candidateId: person.id,
                                    relation: bestSuggestion.type.toUpperCase() as 'PARENT' | 'CHILD' | 'SPOUSE' | 'SIBLING',
                                    confidence: bestSuggestion.confidence,
                                    reasons: bestSuggestion.reasons || []
                                };
                            }
                            return null;
                        } catch (error) {
                            console.error(`Error getting suggestion for ${personId} and ${person.id}:`, error);
                            return null;
                        }
                    });

                    const batchResults = (await Promise.all(batchPromises)).filter(Boolean) as Array<{
                        candidateId: string;
                        relation: 'PARENT' | 'CHILD' | 'SPOUSE' | 'SIBLING';
                        confidence: number;
                        reasons: string[];
                    }>;

                    results.push(...batchResults);
                }

                // Sort by confidence (highest first)
                return results.sort((a, b) => b.confidence - a.confidence);

            } catch (error) {
                console.error('Error fetching suggestions:', error);
                showToast.error('Có lỗi khi tải gợi ý mối quan hệ');
                return [];
            }
        };
    }, [treeId, userId, persons]);

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

    return (
        <div className="relative min-h-screen">
            <img src={bg} className="absolute inset-0 w-full h-full object-cover -z-10" />
            <div className="absolute inset-0 bg-slate-900/40 -z-10" />

            <div className="relative z-20">
                <Navbar />
            </div>

            <div className="relative z-10 mt-16 px-4 md:px-6">
                <div className="w-full rounded-xl bg-gradient-to-r text-white">
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
                            className="inline-flex items-center gap-2 rounded-lg bg-white/20 hover:bg-white/30 px-4 py-2 text-white shadow-sm hover:shadow transition-all"
                            title="Thêm thành viên"
                        >
                            <LucideUserPlus className="w-5 h-5" />
                            <span className="hidden sm:inline">Thêm thành viên</span>
                        </button>
                    </div>
                </div>
            </div>

            <div className="grid grid-cols-12 gap-4 p-4 pt-24">
                <DetailsSidebar
                    tree={tree}
                    memberCount={loading ? 0 : persons.length}
                    generationCount={loading ? 0 : 1}
                    onAddClick={handleAddClick}
                />

                <main className="col-span-12 md:col-span-9 bg-white rounded-xl shadow p-3">
                    {!treeId ? (
                        <div className="text-sm text-slate-500">Không tìm thấy treeId trong URL.</div>
                    ) : loading ? (
                        <div className="text-gray-500 text-sm">Đang tải…</div>
                    ) : (
                        <TreeGraph
                            persons={persons}
                            relationships={rels}
                            onNodeClick={handleNodeClick}
                            selectedNodeId={selectedPerson?.id}
                        />
                    )}
                </main>
            </div>

            <MemberModal
                open={memberOpen || isEditing}
                onClose={() => {
                    setMemberOpen(false);
                    setIsEditing(false);
                }}
                onSubmit={isEditing ? handleUpdateMember : handleCreateMember}
                submitting={memberSubmitting}
                title={isEditing ? "Chỉnh sửa thông tin" : "Thêm thành viên"}
                initialValues={isEditing && selectedPerson ? {
                    fullName: selectedPerson.fullName,
                    gender: selectedPerson.gender as any,
                    birthDate: selectedPerson.birthDate,
                    deathDate: selectedPerson.deathDate,
                    birthPlace: selectedPerson.birthPlace,
                    deathPlace: selectedPerson.deathPlace,
                    biography: selectedPerson.biography,
                    avatarUrl: selectedPerson.avatarUrl
                } : undefined}
            />

            <PersonDetailsModal
                isOpen={isViewingDetails}
                person={selectedPerson}
                persons={persons}
                relationships={rels}
                onClose={handleCloseDetails}
                onEditClick={handleEditClick}
            />
            {source && (
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