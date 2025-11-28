import { useEffect, useMemo, useState } from "react";
import type { Person, Relationship } from "@/api/trees";

import {ArrowLeft} from "lucide-react"

export type RelationUpper = "PARENT" | "CHILD" | "SPOUSE" | "SIBLING";
export interface PairSuggestion {
    candidateId: string;
    relation: RelationUpper;
    confidence: number;
}

interface Props {
    isOpen: boolean;
    onClose: () => void;
    onCancel?: () => void;
    onBack: ()=> void;
    source: Person;
    persons: Person[];
    relationships?: Relationship[];
    fetchSuggestions: (sourceId: string) => Promise<PairSuggestion[]>;
    onConfirm: (picked: { relation: RelationUpper; candidateId: string }) => Promise<void>;
    hasMultiSpouse?: boolean;
}

const RELATION_LABEL: Record<RelationUpper, string> = {
    PARENT: "Cha/Mẹ",
    CHILD: "Con",
    SIBLING: "Anh/Chị/Em",
    SPOUSE: "Vợ/Chồng",
};

export default function RelationshipModal({
                                              isOpen,
                                              onClose,
                                              onCancel,
                                              onBack,
                                              source,
                                              persons,
                                              relationships = [],
                                              fetchSuggestions,
                                              onConfirm,
                                              hasMultiSpouse = false,
                                          }: Props) {

    const [loading, setLoading] = useState(false);
    const [confirming, setConfirming] = useState(false);
    const [suggestions, setSuggestions] = useState<PairSuggestion[]>([]);
    const [picked, setPicked] = useState<{ relation: RelationUpper | ""; candidateId: string | "" }>({
        relation: "",
        candidateId: "",
    });
    const [error, setError] = useState("");
    const [fatherId, setFatherId] = useState<string>("");
    const [motherId, setMotherId] = useState<string>("");
    const [otherParentId, setOtherParentId] = useState<string>("");

    const candidatesMap = useMemo(() => new Map(persons.map((p) => [p.id, p])), [persons]);

    useEffect(() => {
        if (!isOpen) return;
        setError("");
        setPicked({ relation: "", candidateId: "" });
        setFatherId("");
        setMotherId("");
        setOtherParentId("");
        setLoading(true);

        try { console.log("[RelationshipModal] open for", { sourceId: source.id, persons: persons.map(p=>p.id) }); } catch {}
        fetchSuggestions(source.id)
            .then((list) => {
                try { console.log("[RelationshipModal] suggestions length", list?.length, "first", list?.[0]); } catch {}
                setSuggestions(list || []);
            })
            .catch((e) => {
                try { console.error("[RelationshipModal] fetchSuggestions error", e); } catch {}
                setError(String(e?.message || e));
            })
            .finally(() => setLoading(false));
    }, [isOpen, source?.id, fetchSuggestions]);

    const selected =
        picked.candidateId ? suggestions.find((s) => s.candidateId === picked.candidateId) : suggestions[0];

    useEffect(() => {
        if (!picked.candidateId) return;
        const s = suggestions.find((x) => x.candidateId === picked.candidateId);
        setPicked((prev) => ({
            ...prev,
            relation: (s?.relation as RelationUpper) || (prev.relation as any) || "",
        }));
    }, [picked.candidateId, suggestions]);

    // const close  = onClose;
    const cancel = onCancel ?? onClose;

    const effectiveRelation = picked.relation || selected?.relation || "";
    const effectiveCandidateId = picked.candidateId || selected?.candidateId || "";
    const selectedGender = useMemo(() => String(candidatesMap.get(effectiveCandidateId)?.gender || "").toUpperCase(), [effectiveCandidateId, candidatesMap]);
    const candidateSpouseIds = useMemo(() => {
        if (!effectiveCandidateId || !relationships?.length) return [] as string[];
        const ids = relationships
            .filter(r => String(r.type).toUpperCase() === "SPOUSE" && (r.fromPersonId === effectiveCandidateId || r.toPersonId === effectiveCandidateId))
            .map(r => r.fromPersonId === effectiveCandidateId ? r.toPersonId : r.fromPersonId);
        // de-duplicate in case of bi-directional or duplicated records
        return Array.from(new Set(ids));
    }, [effectiveCandidateId, relationships]);

    // Symmetric rule: when linking CHILD, if candidate has exactly one spouse, auto infer.
    // If candidate has 0 spouses: cannot proceed (need to create spouse link first).
    // If candidate has >1 spouses: require picking the other parent explicitly.
    const requireOtherParent = (effectiveRelation === "CHILD") && (candidateSpouseIds.length !== 1);
    const mustPickOther = requireOtherParent && candidateSpouseIds.length > 1; // pick explicitly only when multiple
    const canConfirm = !!effectiveRelation && !!effectiveCandidateId && !loading && (!mustPickOther || !!otherParentId);

    const otherParentLabel = selectedGender === "FEMALE" ? "Bố" : "Mẹ";
    const otherParentAutoNote = selectedGender === "FEMALE" ? "Tự động chọn chồng duy nhất" : "Tự động chọn vợ duy nhất";
    const otherParentMissingMsg = selectedGender === "FEMALE"
        ? "Người mẹ chưa có chồng được liên kết, không thể chọn bố. Vui lòng tạo mối quan hệ Vợ/Chồng trước."
        : "Người cha chưa có vợ được liên kết, không thể chọn mẹ. Vui lòng tạo mối quan hệ Vợ/Chồng trước.";

    // Build quick-pick father/mother lists using gender
    const maleCandidates = useMemo(() => persons.filter(p => String(p.gender || "").toUpperCase() === "MALE" && p.id !== source.id), [persons, source.id]);
    const femaleCandidates = useMemo(() => persons.filter(p => String(p.gender || "").toUpperCase() === "FEMALE" && p.id !== source.id), [persons, source.id]);

    return (
        <div className={`fixed inset-0 z-50 ${isOpen ? "" : "hidden"}`}>
            <div className="absolute inset-0 bg-black/40" onClick={() => {
                if (confirming) return;
                cancel();
            }}
            />
            <div className="absolute left-1/2 top-1/2 -translate-x-1/2 -translate-y-1/2 w-[680px] max-w-[95vw] rounded-2xl bg-white shadow-xl">
                <div className="p-4 border-b flex items-center justify-between">
                    <h3 className="text-lg font-semibold text-gray-700">
                        Xác nhận mối quan hệ cho <span className="text-emerald-700">{source.fullName}</span>
                    </h3>
                    <button onClick={() => !confirming && cancel()} className="px-3 py-1 rounded hover:bg-gray-100" disabled={confirming} >Đóng</button>
                </div>

                <div className="p-4 space-y-4">
                    {loading && <div className="text-sm text-gray-500">Đang gợi ý từ mô hình…</div>}
                    {error && <div className="text-sm text-red-600">{error}</div>}

                    {!loading && !error && (
                        <>
                            {selected ? (
                                <div className="p-3 rounded-xl border bg-gray-50">
                                    <div className="text-sm text-gray-600">Gợi ý tốt nhất</div>
                                    <div className="mt-1 flex items-center justify-between">
                                        <div className="text-base text-black">
                                            <b>{RELATION_LABEL[selected.relation]}</b> với{" "}
                                            <b>{candidatesMap.get(selected.candidateId)?.fullName || "(???)"}</b>
                                        </div>
                                        <div className="text-xs px-2 py-0.5 rounded-full bg-gray-200 text-black">
                                            độ tin cậy {(selected.confidence * 100).toFixed(0)}%
                                        </div>
                                    </div>
                                </div>
                            ) : (
                                <div className="text-sm text-gray-600">Không có gợi ý.</div>
                            )}

                            <div className="grid grid-cols-2 gap-3 text-black">
                                <div>
                                    <label className="text-sm font-medium">Chọn người liên hệ</label>
                                    <select
                                        className="mt-1 w-full rounded-lg border px-3 py-2"
                                        value={picked.candidateId || selected?.candidateId || ""}
                                        onChange={(e) => setPicked((s) => ({ ...s, candidateId: e.target.value }))}
                                    >
                                        <option value="" >-- Chọn một người --</option>
                                        {persons
                                            .filter((p) => p.id !== source.id)
                                            .map((p) => (
                                                <option key={p.id} value={p.id}>
                                                    {p.fullName}
                                                </option>
                                            ))}
                                    </select>
                                </div>
                                <div>
                                    <label className="text-sm font-medium">Chọn loại quan hệ</label>
                                    <select
                                        className="mt-1 w-full rounded-lg border px-3 py-2"
                                        value={picked.relation || selected?.relation || ""}
                                        onChange={(e) => setPicked((s) => ({ ...s, relation: e.target.value as RelationUpper }))}
                                    >
                                        <option value="">-- Chọn quan hệ --</option>
                                        <option value="PARENT">Cha/Mẹ</option>
                                        <option value="CHILD">Con</option>
                                        <option value="SIBLING">Anh/Chị/Em</option>
                                        <option value="SPOUSE">Vợ/Chồng</option>
                                    </select>
                                </div>
                            </div>

                            {requireOtherParent && (
                                <div className="mt-2 p-3 rounded-xl border bg-amber-50 text-black">
                                    <div className="text-sm font-medium">Chọn {otherParentLabel} cho {source.fullName}</div>
                                    {candidateSpouseIds.length === 0 && (
                                        <div className="text-xs text-red-600 mt-1">{otherParentMissingMsg}</div>
                                    )}
                                    {candidateSpouseIds.length >= 1 && (
                                        <select
                                            className="mt-2 w-full rounded-lg border px-3 py-2"
                                            value={otherParentId}
                                            onChange={(e) => setOtherParentId(e.target.value)}
                                        >
                                            <option value="">-- {candidateSpouseIds.length > 1 ? `Chọn một người ${otherParentLabel.toLowerCase()}` : otherParentAutoNote} --</option>
                                            {candidateSpouseIds.map(id => (
                                                <option key={id} value={id}>{candidatesMap.get(id)?.fullName || id}</option>
                                            ))}
                                        </select>
                                    )}
                                    <div className="text-xs text-gray-600 mt-1">Theo luật: Khi ghép Con với phụ huynh có nhiều vợ/chồng, cần xác định phụ huynh còn lại.</div>
                                </div>
                            )}

                            {/* Quick-pick Father/Mother when adding parent links for the source; only show if multi-spouse exists */}
                            {hasMultiSpouse && (picked.relation === "CHILD" || selected?.relation === "CHILD") && (
                                <div className="mt-4 p-3 rounded-xl border bg-gray-50 text-black">
                                    <div className="text-sm font-semibold">Thiết lập nhanh Cha và Mẹ cho {source.fullName}</div>
                                    <div className="grid grid-cols-2 gap-3 mt-2">
                                        <div>
                                            <label className="text-sm">Chọn Cha (nam)</label>
                                            <select
                                                className="mt-1 w-full rounded-lg border px-3 py-2"
                                                value={fatherId}
                                                onChange={(e) => setFatherId(e.target.value)}
                                            >
                                                <option value="">-- Không chọn --</option>
                                                {maleCandidates.map(p => (
                                                    <option key={p.id} value={p.id}>{p.fullName}</option>
                                                ))}
                                            </select>
                                        </div>
                                        <div>
                                            <label className="text-sm">Chọn Mẹ (nữ)</label>
                                            <select
                                                className="mt-1 w-full rounded-lg border px-3 py-2"
                                                value={motherId}
                                                onChange={(e) => setMotherId(e.target.value)}
                                            >
                                                <option value="">-- Không chọn --</option>
                                                {femaleCandidates.map(p => (
                                                    <option key={p.id} value={p.id}>{p.fullName}</option>
                                                ))}
                                            </select>
                                        </div>
                                    </div>
                                    <div className="mt-3 text-xs text-gray-600">Khi bấm "Thêm Cha/Mẹ", hệ thống sẽ tạo quan hệ: Cha → {source.fullName} và Mẹ → {source.fullName} (nếu được chọn).</div>
                                    <div className="mt-3">
                                        <button
                                            className="px-3 py-2 rounded-lg bg-blue-600 text-white disabled:opacity-50"
                                            disabled={confirming || (!fatherId && !motherId)}
                                            onClick={async () => {
                                                if (!fatherId && !motherId) return;
                                                setError("");
                                                setConfirming(true);
                                                try {
                                                    if (fatherId) {
                                                        await onConfirm({ relation: "CHILD", candidateId: fatherId });
                                                    }
                                                    if (motherId) {
                                                        await onConfirm({ relation: "CHILD", candidateId: motherId });
                                                    }
                                                    onClose();
                                                } catch (e: any) {
                                                    setError(e?.message || "Có lỗi khi tạo Cha/Mẹ.");
                                                } finally {
                                                    setConfirming(false);
                                                }
                                            }}
                                        >
                                            {confirming ? "Đang thêm…" : "Thêm Cha/Mẹ"}
                                        </button>
                                    </div>
                                </div>
                            )}
                        </>
                    )}
                </div>

                <div className="p-4 border-t flex items-center justify-between">
                    <div>
                        <button className="px-2 py-2 rounded-lg border text-black" onClick={onBack} disabled={confirming}>
                            <ArrowLeft className="w-4 h-4" />
                        </button>
                    </div>
                    <div className="flex items-center gap-2">
                        <button className="px-3 py-2 rounded-lg border text-black" onClick={cancel} disabled={confirming}>
                            Hủy
                        </button>
                        <button
                            className="px-4 py-2 rounded-lg bg-emerald-600 text-white disabled:opacity-50"
                            disabled={!canConfirm}
                            onClick={async () => {
                                if (!canConfirm) return;
                                setError("");
                                setConfirming(true);
                                try {
                                    // Always create the primary picked relation first
                                    await onConfirm({
                                        relation: effectiveRelation as RelationUpper,
                                        candidateId: effectiveCandidateId,
                                    });

                                    // If required and chosen/available, also create link to the other parent
                                    if (requireOtherParent) {
                                        const autoOther = candidateSpouseIds.length === 1 ? candidateSpouseIds[0] : otherParentId;
                                        if (autoOther) {
                                            await onConfirm({ relation: "CHILD", candidateId: autoOther });
                                        }
                                    }
                                    onClose();
                                } catch (e: any) {
                                    setError(e?.message || "Có lỗi xảy ra khi tạo quan hệ.");
                                } finally {
                                    setConfirming(false);
                                }
                            }}
                        >
                            {confirming ? "Đang xác nhận…" : "Xác nhận"}
                        </button>

                    </div>
                </div>
            </div>
        </div>
    );
}