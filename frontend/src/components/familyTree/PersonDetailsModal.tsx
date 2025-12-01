import { X, Pencil, Trash2, Loader2 } from "lucide-react";
import { useEffect, useState } from "react";
import { Relationship } from "@/api/trees";
import PopupModal from "@/components/popupModal/PopupModal"
import { personLinkApi } from "@/api/personLink";
import { showToast } from "@/lib/toast";

interface PersonDetailsModalProps {
    isOpen: boolean;
    person: any;
    persons: any[];
    relationships: Relationship[];
    onClose: () => void;
    onEditClick: () => void;
    onDelete?: (personId: string) => Promise<void> | void;
    onPrepareDelete?: (personId: string) => Promise<{ count: number; orphanMemberIds?: string[] } | { count: number }>;
    readOnly?: boolean;
}

const formatDate = (dateString?: string | null) => {
    if (!dateString) return "Chưa cập nhật";
    const date = new Date(dateString);
    if (isNaN(date.getTime())) return "Chưa cập nhật";
    return date.toLocaleDateString("vi-VN");
};

const getRelationshipText = (rel: Relationship, currentPersonId: string, persons: any[]) => {
    try {
        const otherPersonId = rel.fromPersonId === currentPersonId ? rel.toPersonId : rel.fromPersonId;
        const otherPerson = persons.find((p) => p.id === otherPersonId);
        if (!otherPerson) return null;

        const otherPersonName = otherPerson.fullName || "Không rõ";
        const isFromPerson = rel.fromPersonId === currentPersonId;
        const currentPerson = persons.find((p) => p.id === currentPersonId);

        const parse = (d?: string) => {
            const t = d ? new Date(d) : null;
            return t && !isNaN(t.getTime()) ? t : null;
        };

        const genderOf = (p: any) => String(p?.gender ?? "").toUpperCase();

        let relationshipText = "";
        let isParentOfCurrent = false;

        switch (rel.type) {
            case "SPOUSE":
                relationshipText =
                    genderOf(currentPerson) === "FEMALE" ? `Chồng: ${otherPersonName}` : `Vợ: ${otherPersonName}`;
                break;
            case "PARENT":
                if (isFromPerson)
                    relationshipText =
                        genderOf(otherPerson) === "FEMALE" ? `Con gái: ${otherPersonName}` : `Con trai: ${otherPersonName}`;
                else {
                    relationshipText = genderOf(otherPerson) === "FEMALE" ? `Mẹ: ${otherPersonName}` : `Bố: ${otherPersonName}`;
                    isParentOfCurrent = true;
                }
                break;
            case "CHILD": {
                if (isFromPerson) {
                    relationshipText = genderOf(otherPerson) === "FEMALE" ? `Mẹ: ${otherPersonName}` : `Bố: ${otherPersonName}`;
                    isParentOfCurrent = true;
                } else {
                    relationshipText = `Con: ${otherPersonName}`;
                }
                break;
            }
            case "SIBLING": {
                const curDOB = parse(currentPerson?.birthDate);
                const othDOB = parse(otherPerson?.birthDate);

                const g = genderOf(otherPerson);

                if (curDOB && othDOB) {
                    if (othDOB < curDOB) {
                        relationshipText = g === "FEMALE" ? `Chị gái: ${otherPersonName}` : `Anh trai: ${otherPersonName}`;
                    } else if (othDOB > curDOB) {
                        relationshipText = g === "FEMALE" ? `Em gái: ${otherPersonName}` : `Em trai: ${otherPersonName}`;
                    } else {
                        relationshipText = g === "FEMALE" ?`Chị/Em gái: ${otherPersonName}`:  `Anh/em trai: ${otherPersonName}`;
                    }
                }
                break;
            }
            default:
                relationshipText = `Quan hệ: ${otherPersonName} (${rel.type})`;
        }

        return { text: relationshipText, person: otherPerson, type: rel.type, relationshipId: rel.id, isParent: isParentOfCurrent };
    } catch {
        return null;
    }
};

export default function PersonDetailsModal({
                                               isOpen,
                                               person,
                                               persons,
                                               relationships,
                                               onClose,
                                               onEditClick,
                                               onDelete,
                                               onPrepareDelete,
                                               readOnly = false,
                                           }: PersonDetailsModalProps) {
    const [filteredRelationships, setFilteredRelationships] = useState<
        Array<{ text: string; person: any; type: string; relationshipId?: string; isParent?: boolean }>
    >([]);

    const [showDeleteModal, setShowDeleteModal] = useState(false);
    const [deleting, setDeleting] = useState(false);
    const [prepareDeleting, setPrepareDeleting] = useState(false);
    const [_, setOrphanCount] = useState<number>(0);

    const [inviteOpen, setInviteOpen] = useState(false);
    const [inviteEmail, setInviteEmail] = useState<string>("");
    const [inviting, setInviting] = useState(false);
    const [canInvite, setCanInvite] = useState(true);
    const [invitedPending, setInvitedPending] = useState(false);

    useEffect(() => {
        setInviteEmail(person?.email || "");
        setCanInvite(true);
        setInvitedPending(false);
        const currentId = person?.id as string | undefined;
        let cancelled = false;
        if (currentId) {
            const key = `person_verified_${person.id}`;
            const verified = localStorage.getItem(key) === 'true';
            if (verified) {
                setCanInvite(false);
                setInvitedPending(false);
            }

            personLinkApi.checkVerified(currentId)
                .then((res: any) => {
                    if (cancelled) return;
                    if (person?.id !== currentId) return;
                    const result = res?.result ?? res;
                    if (result === true) {
                        setCanInvite(false);
                        setInvitedPending(false);
                        try { localStorage.setItem(key, 'true'); } catch {}
                    } else if (result === false) {
                        setCanInvite(true);
                        setInvitedPending(false);
                        try { localStorage.removeItem(key); } catch {}
                    }
                })
                .catch(() => {});
        }
        return () => { cancelled = true; };
    }, [person?.email, person?.id]);

    useEffect(() => {
        const handler = (e: any) => {
            const pid = e?.detail?.personId;
            if (pid && person?.id && pid === person.id) {
                setCanInvite(false);
                setInvitedPending(false);
            }
        };
        window.addEventListener('person-verified', handler as EventListener);
        return () => window.removeEventListener('person-verified', handler as EventListener);
    }, [person?.id]);

    const isValidEmail = (value: string) => {
        const v = (value || "").trim();
        return /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(v);
    };

    const handleDeleteConfirm = async () => {
        if (!person?.id) return;
        try {
            setDeleting(true);
            await onDelete?.(person.id);
            setShowDeleteModal(false);
            onClose();
        } finally {
            setDeleting(false);
        }
    };

    const handleInviteConfirm = async () => {
        if (!person?.id) return;
        const email = (inviteEmail || "").trim();
        if (!email) {
            showToast.warning("Vui lòng nhập email người được mời");
            return;
        }
        if (!isValidEmail(email)) {
            showToast.warning("Email không hợp lệ");
            return;
        }
        try {
            setInviting(true);
            const userRaw = localStorage.getItem("user");
            const inviterId = userRaw ? (JSON.parse(userRaw)?.id as string) : undefined;
            if (!inviterId) {
                showToast.error("Không xác định được người mời. Vui lòng đăng nhập lại.");
                return;
            }
            await personLinkApi.invite(person.id, inviterId, email);
            showToast.success("Đã gửi lời mời xác minh (thông báo + email)");
            setInvitedPending(true);
            try {
                if (!person.email) {
                    person.email = email;
                    setInviteEmail(email);
                }
            } catch {}
            setInviteOpen(false);
        } catch (e: any) {
            const code = e?.response?.data?.code;
            const backendMsg = e?.response?.data?.message;
            if (code === 1008) {
                const hint = backendMsg || "Dữ liệu không hợp lệ.";
                showToast.error(hint);
                setCanInvite(false);
            } else {
                const msg = backendMsg || "Gửi lời mời thất bại";
                showToast.error(msg);
            }
        } finally {
            setInviting(false);
        }
    };

    useEffect(() => {
        if (!person?.id || !relationships?.length || !persons?.length) {
            setFilteredRelationships([]);
            return;
        }

        try {
            const processedPersonIds = new Set<string>();

            const allRelationships = relationships
                .filter((rel) => {
                    if (rel.fromPersonId !== person.id && rel.toPersonId !== person.id) return false;
                    const otherPersonId = rel.fromPersonId === person.id ? rel.toPersonId : rel.fromPersonId;
                    if (processedPersonIds.has(otherPersonId)) return false;
                    processedPersonIds.add(otherPersonId);
                    return true;
                })
                .map((rel) => getRelationshipText(rel, person.id, persons))
                .filter((rel): rel is NonNullable<typeof rel> => rel !== null) as Array<{
                text: string;
                person: any;
                type: string;
                relationshipId?: string;
                isParent?: boolean;
            }>;

            try {
                const parentIds = new Set<string>();
                for (const r of relationships) {
                    const t = String(r.type).toUpperCase();
                    if (t === "PARENT" && r.toPersonId === person.id) parentIds.add(r.fromPersonId);
                    if (t === "CHILD" && r.fromPersonId === person.id) parentIds.add(r.toPersonId);
                }

                if (parentIds.size === 1) {
                    const [onlyParentId] = Array.from(parentIds);

                    const spouseIds = relationships
                        .filter(
                            (r) =>
                                String(r.type).toUpperCase() === "SPOUSE" &&
                                (r.fromPersonId === onlyParentId || r.toPersonId === onlyParentId)
                        )
                        .map((r) => (r.fromPersonId === onlyParentId ? r.toPersonId : r.fromPersonId));

                    for (const spId of spouseIds) {
                        if (parentIds.has(spId)) continue;
                        const alreadyListed = allRelationships.some((item) => item.person?.id === spId);
                        if (alreadyListed) continue;

                        const sp = persons.find((p) => p.id === spId);
                        if (!sp) continue;

                        const g = String(sp.gender ?? "").toUpperCase();
                        const text = g === "FEMALE" ? `Mẹ: ${sp.fullName}` : `Bố: ${sp.fullName}`;

                        allRelationships.push({
                            text,
                            person: sp,
                            type: "PARENT",
                            relationshipId: undefined,
                            isParent: true,
                        });
                        break;
                    }
                }
            } catch {}

            const personRelationships = allRelationships.sort((a, b) => {
                const order = { PARENT: 1, SPOUSE: 2, CHILD: 3 } as const;
                const getEffectiveType = (item: typeof a) => {
                    const t = item.text;
                    if (t.startsWith("Mẹ:") || t.startsWith("Bố:")) return "PARENT";
                    if (t.startsWith("Con:")) return "CHILD";
                    if (t.startsWith("Vợ:") || t.startsWith("Chồng:")) return "SPOUSE";
                    return item.type;
                };
                return (order[getEffectiveType(a) as keyof typeof order] ?? 99) - (order[getEffectiveType(b) as keyof typeof order] ?? 99);
            });

            setFilteredRelationships(personRelationships);
        } catch (error) {
            console.error("Error processing relationships:", error);
            setFilteredRelationships([]);
        }
    }, [person?.id, relationships, persons]);

    if (!isOpen || !person) return null;

    return (
        <div className="fixed inset-0 z-50 flex items-center justify-center p-4">
                <div className="absolute inset-0 bg-black/50 transition-opacity" onClick={() => { onClose(); }}></div>
                <div className="relative z-10 w-full max-w-2xl max-h-[90vh] overflow-y-auto rounded-xl bg-white shadow-xl">
                    <div className="sticky top-0 bg-white z-20 flex justify-between items-center p-4 border-b">
                        <h2 className="text-xl font-semibold text-black">Thông tin chi tiết</h2>

                        <div className="flex items-center gap-2">
                            {!readOnly && invitedPending && (
                                <span className="px-2 py-1 rounded-md text-xs font-semibold bg-amber-100 text-amber-700 border border-amber-200">Đang chờ xác minh</span>
                            )}
                            {!readOnly && !canInvite && (
                                <span className="px-2 py-1 rounded-md text-xs font-semibold bg-emerald-100 text-emerald-700 border border-emerald-200">Đã xác minh</span>
                            )}
                            {!readOnly && canInvite && (
                                <button
                                    onClick={() => setInviteOpen(true)}
                                    className="px-3 py-1.5 rounded-md bg-emerald-600 text-white text-sm hover:bg-emerald-700"
                                    aria-label="Mời xác minh hồ sơ"
                                    title="Mời xác minh hồ sơ"
                                >
                                    Mời xác minh hồ sơ
                                </button>
                            )}
                            {!readOnly && (
                                <button
                                    onClick={onEditClick}
                                    className="p-2 rounded-full hover:bg-gray-100 text-gray-700"
                                    aria-label="Chỉnh sửa"
                                    title="Chỉnh sửa"
                                >
                                    <Pencil className="h-5 w-5" />
                                </button>
                            )}
                            {!readOnly && (
                                <button
                                    onClick={async () => {
                                        if (prepareDeleting) return;
                                        setOrphanCount(0);
                                        if (onPrepareDelete && person?.id) {
                                            try {
                                                setPrepareDeleting(true);
                                                const res = await onPrepareDelete(person.id);
                                                const c = (res as any)?.count ?? 0;
                                                setOrphanCount(typeof c === 'number' ? c : 0);
                                            } catch {
                                                setOrphanCount(0);
                                            } finally {
                                                setPrepareDeleting(false);
                                            }
                                        }
                                        setShowDeleteModal(true);
                                    }}
                                    className={`p-2 rounded-full hover:bg-red-50 text-red-600 ${prepareDeleting ? 'opacity-60 cursor-not-allowed' : ''}`}
                                    aria-label="Xoá khỏi cây"
                                    title="Xoá khỏi cây"
                                    aria-busy={prepareDeleting}
                                    disabled={prepareDeleting}
                                >
                                    {prepareDeleting ? (
                                        <Loader2 className="h-5 w-5 animate-spin" />
                                    ) : (
                                        <Trash2 className="h-5 w-5" />
                                    )}
                                </button>
                            )}
                            <button onClick={() => { onClose(); }} className="p-1 rounded-full hover:bg-gray-100" aria-label="Đóng">
                                <X className="h-5 w-5 text-black" />
                            </button>
                        </div>

                    </div>

                    <div className="p-6">
                        <div className="flex flex-col md:flex-row gap-6">
                            <div className="flex-shrink-0">
                                <div className="w-32 h-32 rounded-lg overflow-hidden bg-gray-100">
                                    {person.avatarUrl ? (
                                        <img src={person.avatarUrl} alt={person.fullName} className="w-full h-full object-cover" />
                                    ) : (
                                        <div className="w-full h-full flex items-center justify-center bg-gray-200 text-gray-400">
                                            <span className="text-sm">No photo</span>
                                        </div>
                                    )}
                                </div>
                            </div>

                            <div className="flex-1">
                                <div className="flex justify-between items-start">
                                    <div>
                                        <h1 className="text-2xl font-bold text-gray-700">{person.fullName}</h1>
                                        <p className="text-gray-600">
                                            <span className="font-bold">Giới tính: </span>
                                            {String(person.gender).toUpperCase() === "MALE"
                                                ? "Nam"
                                                : String(person.gender).toUpperCase() === "FEMALE"
                                                    ? "Nữ"
                                                    : "Khác"}
                                        </p>
                                    </div>
                                </div>

                                <div className=" grid grid-cols-1 md:grid-cols-2 gap-1">
                                    <div>
                                        <p className="text-sm font-medium text-gray-500">Ngày sinh</p>
                                        <p className="text-slate-500">{formatDate(person.birthDate)}</p>
                                    </div>

                                    {person.deathDate && (
                                        <div>
                                            <p className="text-sm font-medium text-gray-500">Ngày mất</p>
                                            <p className="text-slate-500">{formatDate(person.deathDate)}</p>
                                        </div>
                                    )}

                                    {person.birthPlace && (
                                        <div>
                                            <p className="text-sm font-medium text-gray-500">Nơi sinh</p>
                                            <p className="text-slate-500">{person.birthPlace}</p>
                                        </div>
                                    )}

                                    {person.deathPlace && (
                                        <div>
                                            <p className="text-sm font-medium text-gray-500">Nơi mất</p>
                                            <p className="text-slate-500">{person.deathPlace}</p>
                                        </div>
                                    )}

                                    {person.phone && (
                                        <div>
                                            <p className="text-sm font-medium text-gray-500">Số điện thoại</p>
                                            <p className="text-slate-500">{person.phone}</p>
                                        </div>
                                    )}

                                    {person.email && (
                                        <div>
                                            <p className="text-sm font-medium text-gray-500">Email</p>
                                            <p className="text-slate-500">{person.email}</p>
                                        </div>
                                    )}
                                </div>

                                {person.biography && (
                                    <div className="mt-6">
                                        <p className="text-sm font-medium text-gray-500 mb-2">Tiểu sử</p>
                                        <p className="whitespace-pre-line text-gray-800">{person.biography}</p>
                                    </div>
                                )}

                                <div className="mt-2">
                                    <p className="text-sm font-medium text-gray-500 mb-3">Mối quan hệ</p>
                                    {filteredRelationships.length > 0 ? (
                                        <div className="space-y-3">
                                            {filteredRelationships.map((rel, index) => (
                                                <div
                                                    key={`${rel.relationshipId || index}-${rel.type}`}
                                                    className="p-3 bg-gray-50 rounded-lg hover:bg-gray-100 transition-colors cursor-pointer"
                                                >
                                                    <div className="flex items-center gap-3">
                                                        {rel.person?.avatarUrl ? (
                                                            <img
                                                                src={rel.person.avatarUrl}
                                                                alt={rel.person.fullName}
                                                                className="w-10 h-10 rounded-full object-cover"
                                                                onError={(e) => {
                                                                    const target = e.target as HTMLImageElement;
                                                                    target.onerror = null;
                                                                    target.src = "";
                                                                }}
                                                            />
                                                        ) : (
                                                            <div className="w-10 h-10 rounded-full bg-gray-200 flex items-center justify-center text-gray-500">
                                                                <span className="text-xs">{rel.person?.fullName?.charAt(0) || "?"}</span>
                                                            </div>
                                                        )}
                                                        <div>
                                                            <p className="text-sm text-gray-800 font-semibold">{rel.text}</p>
                                                        </div>
                                                    </div>
                                                </div>
                                            ))}
                                        </div>
                                    ) : (
                                        <p className="text-gray-500">Người này chưa có thông tin mối quan hệ</p>
                                    )}
                                </div>
                            </div>
                        </div>
                    </div>
                </div>

                <PopupModal
                    show={showDeleteModal}
                    onClose={() => (!deleting ? setShowDeleteModal(false) : undefined)}
                    onConfirm={onDelete ? handleDeleteConfirm : undefined}
                    title="Xoá thành viên khỏi cây"
                    body={
                        <div className="space-y-1">
                            <p>
                                Bạn có chắc muốn xoá <span className="font-semibold">{person?.fullName}</span> khỏi cây gia phả{prepareDeleting ? '' : ','} {prepareDeleting ? '' : (
                                    <>sau khi xóa thì Nếu có thành viên không còn liên kết với thế hệ gốc. Bạn có muốn xoá hết không?</>
                                )}
                            </p>
                        </div>
                    }
                    confirmText="Xoá"
                    cancelText="Huỷ"
                    variant="danger"
                    loading={deleting}
                />
                <PopupModal
                    show={inviteOpen}
                    onClose={() => (!inviting ? setInviteOpen(false) : undefined)}
                    onConfirm={handleInviteConfirm}
                    title="Mời xác minh hồ sơ"
                    confirmButtonProps={{ disabled: inviting || !isValidEmail(inviteEmail) }}
                    body={
                        <div className="space-y-3">
                            <p>Gửi lời mời xác minh danh tính đến email của người này. Hệ thống sẽ gửi thông báo trong ứng dụng (nếu đã có tài khoản) và email.</p>
                            <div>
                                <label className="block text-sm text-gray-600 mb-1">Email</label>
                                <input
                                    type="email"
                                    className="w-full border rounded-md px-3 py-2 focus:outline-none focus:ring-2 focus:ring-emerald-500"
                                    value={inviteEmail}
                                    onChange={(e) => setInviteEmail(e.target.value)}
                                    placeholder="nhapemail@domain.com"
                                />
                            </div>
                        </div>
                    }
                    confirmText="Gửi lời mời xác minh"
                    cancelText="Huỷ"
                    variant="default"
                    loading={inviting}
                />

        </div>
    );
}