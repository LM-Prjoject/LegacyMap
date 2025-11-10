import { X } from "lucide-react";
import { useEffect, useState } from "react";
import { Relationship } from "@/api/trees";

interface PersonDetailsModalProps {
    isOpen: boolean;
    person: any;
    persons: any[];
    relationships: Relationship[];
    onClose: () => void;
    onEditClick: () => void;
    onHoverPerson?: (id: string | null) => void;
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
                relationshipText = genderOf(currentPerson) === "FEMALE" ? `Chồng: ${otherPersonName}` : `Vợ: ${otherPersonName}`;
                break;
            case "PARENT":
                if (isFromPerson)
                    relationshipText = genderOf(otherPerson) === "FEMALE" ? `Con gái: ${otherPersonName}` : `Con trai: ${otherPersonName}`;
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
                        relationshipText = `Anh/Chị/Em: ${otherPersonName}`;
                    }
                } else {
                    relationshipText = g === "FEMALE" ? `Chị/Em gái: ${otherPersonName}` : `Anh/Em trai: ${otherPersonName}`;
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
                                               onHoverPerson,
                                           }: PersonDetailsModalProps) {
    const [filteredRelationships, setFilteredRelationships] = useState<
        Array<{
            text: string;
            person: any;
            type: string;
            relationshipId?: string;
            isParent?: boolean;
        }>
    >([]);

    useEffect(() => {
        if (!isOpen) onHoverPerson?.(null);
        return () => onHoverPerson?.(null);
    }, [isOpen, onHoverPerson]);

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
                .filter((rel): rel is NonNullable<typeof rel> => rel !== null);

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
        <div className="fixed inset-0 z-50 overflow-y-auto">
            <div className="flex min-h-screen items-center justify-center p-4">
                <div className="fixed inset-0 bg-black/50 transition-opacity" onClick={() => { onHoverPerson?.(null); onClose(); }}></div>

                <div className="relative z-10 w-full max-w-2xl max-h-[90vh] overflow-y-auto rounded-xl bg-white shadow-xl">
                    <div className="sticky top-0 bg-white z-20 flex justify-between items-center p-4 border-b">
                        <h2 className="text-xl font-semibold">Thông tin chi tiết</h2>
                        <button onClick={() => { onHoverPerson?.(null); onClose(); }} className="p-1 rounded-full hover:bg-gray-100">
                            <X className="h-5 w-5" />
                        </button>
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
                                        <h1 className="text-2xl font-bold">{person.fullName}</h1>
                                        <p className="text-gray-600">{person.gender === "male" ? "Nam" : person.gender === "female" ? "Nữ" : "Khác"}</p>
                                    </div>
                                    <button
                                        onClick={onEditClick}
                                        className="px-3 py-1.5 text-sm font-medium rounded-md border border-gray-300 bg-white text-gray-700 hover:bg-gray-50"
                                    >
                                        Chỉnh sửa
                                    </button>
                                </div>

                                <div className="mt-6 grid grid-cols-1 md:grid-cols-2 gap-4">
                                    <div>
                                        <p className="text-sm font-medium text-gray-500">Ngày sinh</p>
                                        <p>{formatDate(person.birthDate)}</p>
                                    </div>

                                    {person.deathDate && (
                                        <div>
                                            <p className="text-sm font-medium text-gray-500">Ngày mất</p>
                                            <p>{formatDate(person.deathDate)}</p>
                                        </div>
                                    )}

                                    {person.birthPlace && (
                                        <div>
                                            <p className="text-sm font-medium text-gray-500">Nơi sinh</p>
                                            <p>{person.birthPlace}</p>
                                        </div>
                                    )}

                                    {person.deathPlace && (
                                        <div>
                                            <p className="text-sm font-medium text-gray-500">Nơi mất</p>
                                            <p>{person.deathPlace}</p>
                                        </div>
                                    )}
                                </div>

                                {person.biography && (
                                    <div className="mt-6">
                                        <p className="text-sm font-medium text-gray-500 mb-2">Tiểu sử</p>
                                        <p className="whitespace-pre-line text-gray-800">{person.biography}</p>
                                    </div>
                                )}

                                <div className="mt-6">
                                    <p className="text-sm font-medium text-gray-500 mb-3">Mối quan hệ</p>
                                    {filteredRelationships.length > 0 ? (
                                        <div className="space-y-3">
                                            {filteredRelationships.map((rel, index) => (
                                                <div
                                                    key={`${rel.relationshipId || index}-${rel.type}`}
                                                    className="p-3 bg-gray-50 rounded-lg hover:bg-gray-100 transition-colors cursor-pointer"
                                                    onMouseEnter={() => {
                                                        // chỉ highlight viền mẹ/bố khi hover vào item là PARENT của current
                                                        if (onHoverPerson) onHoverPerson(rel.isParent ? rel.person?.id ?? null : null);
                                                    }}
                                                    onMouseLeave={() => onHoverPerson?.(null)}
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
                                                            <p className="font-medium text-gray-800">{rel.person?.fullName || "Không rõ"}</p>
                                                            <p className="text-sm text-gray-600">{rel.text}</p>
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
            </div>
        </div>
    );
}