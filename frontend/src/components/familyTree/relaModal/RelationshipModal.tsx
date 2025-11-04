import { useEffect, useMemo, useState } from "react";
import type { Person } from "@/api/trees";

export type RelationUpper = "PARENT" | "CHILD" | "SPOUSE" | "SIBLING";
export interface PairSuggestion {
    candidateId: string;
    relation: RelationUpper;
    confidence: number;
}

interface Props {
    isOpen: boolean;
    onClose: () => void;
    source: Person;
    persons: Person[];
    fetchSuggestions: (sourceId: string) => Promise<PairSuggestion[]>;
    onConfirm: (picked: { relation: RelationUpper; candidateId: string }) => Promise<void>;
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
                                              source,
                                              persons,
                                              fetchSuggestions,
                                              onConfirm,
                                          }: Props) {
    const [loading, setLoading] = useState(false);
    const [suggestions, setSuggestions] = useState<PairSuggestion[]>([]);
    const [picked, setPicked] = useState<{ relation: RelationUpper | ""; candidateId: string | "" }>({
        relation: "",
        candidateId: "",
    });
    const [error, setError] = useState("");

    const candidatesMap = useMemo(() => new Map(persons.map((p) => [p.id, p])), [persons]);

    useEffect(() => {
        if (!isOpen) return;
        setError("");
        setPicked({ relation: "", candidateId: "" });
        setLoading(true);
        fetchSuggestions(source.id)
            .then((list) => setSuggestions(list))
            .catch((e) => setError(String(e?.message || e)))
            .finally(() => setLoading(false));
    }, [isOpen, source?.id, fetchSuggestions]);

    const selected =
        picked.candidateId
            ? suggestions.find((s) => s.candidateId === picked.candidateId)
            : suggestions[0];

    useEffect(() => {
        if (!picked.candidateId) return;
        const s = suggestions.find((x) => x.candidateId === picked.candidateId);
        setPicked((prev) => ({
            ...prev,
            relation: (s?.relation as RelationUpper) || (prev.relation as any) || "",
        }));
    }, [picked.candidateId, suggestions]);

    return (
        <div className={`fixed inset-0 z-50 ${isOpen ? "" : "hidden"}`}>
            <div className="absolute inset-0 bg-black/40" onClick={onClose} />
            <div className="absolute left-1/2 top-1/2 -translate-x-1/2 -translate-y-1/2 w-[680px] max-w-[95vw] rounded-2xl bg-white shadow-xl">
                <div className="p-4 border-b flex items-center justify-between">
                    <h3 className="text-lg font-semibold">
                        Xác nhận mối quan hệ cho <span className="text-emerald-700">{source.fullName}</span>
                    </h3>
                    <button onClick={onClose} className="px-3 py-1 rounded hover:bg-gray-100">
                        Đóng
                    </button>
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
                                        <div className="text-base">
                                            <b>{RELATION_LABEL[selected.relation]}</b> với{" "}
                                            <b>{candidatesMap.get(selected.candidateId)?.fullName || "(???)"}</b>
                                        </div>
                                        <div className="text-xs px-2 py-0.5 rounded-full bg-gray-200">
                                            độ tin cậy {(selected.confidence * 100).toFixed(0)}%
                                        </div>
                                    </div>
                                </div>
                            ) : (
                                <div className="text-sm text-gray-600">
                                    Không có gợi ý.
                                </div>
                            )}

                            <div className="grid grid-cols-2 gap-3">
                                <div>
                                    <label className="text-sm font-medium">Chọn người liên hệ</label>
                                    <select
                                        className="mt-1 w-full rounded-lg border px-3 py-2"
                                        value={picked.candidateId || selected?.candidateId || ""}
                                        onChange={(e) => setPicked((s) => ({ ...s, candidateId: e.target.value }))}
                                    >
                                        <option value="">-- Chọn một người --</option>
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
                        </>
                    )}
                </div>

                <div className="p-4 border-t flex items-center justify-end gap-2">
                    <button className="px-3 py-2 rounded-lg border" onClick={onClose}>
                        Hủy
                    </button>
                    <button
                        className="px-4 py-2 rounded-lg bg-emerald-600 text-white disabled:opacity-50"
                        disabled={loading}
                        onClick={async () => {
                            const relation = (picked.relation || selected?.relation) as RelationUpper | undefined;
                            const candidateId = (picked.candidateId || selected?.candidateId) as string | undefined;
                            if (!relation || !candidateId) {
                                setError("Hãy chọn đủ người và loại quan hệ");
                                return;
                            }
                            setError("");
                            await onConfirm({ relation, candidateId });
                            onClose();
                        }}
                    >
                        Xác nhận
                    </button>
                </div>
            </div>
        </div>
    );
}
