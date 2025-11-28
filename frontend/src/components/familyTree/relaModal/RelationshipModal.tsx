import { useEffect, useMemo, useState } from "react";
import type { Person } from "@/api/trees";
import { ArrowLeft, Link2, Sparkles, Check } from "lucide-react";

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
    onBack: () => void;
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
                                              onCancel,
                                              onBack,
                                              source,
                                              persons,
                                              fetchSuggestions,
                                              onConfirm,
                                          }: Props) {
    const [loading, setLoading] = useState(false);
    const [confirming, setConfirming] = useState(false);
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
        try { console.log("[RelationshipModal] open for", { sourceId: source.id, persons: persons.map(p => p.id) }); } catch { }
        fetchSuggestions(source.id)
            .then((list) => {
                try { console.log("[RelationshipModal] suggestions length", list?.length, "first", list?.[0]); } catch { }
                setSuggestions(list || []);
            })
            .catch((e) => {
                try { console.error("[RelationshipModal] fetchSuggestions error", e); } catch { }
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

    const cancel = onCancel ?? onClose;

    const effectiveRelation = picked.relation || selected?.relation || "";
    const effectiveCandidateId = picked.candidateId || selected?.candidateId || "";
    const canConfirm = !!effectiveRelation && !!effectiveCandidateId && !loading;

    if (!isOpen) return null;

    return (
        <div className="fixed inset-0 z-50 flex items-center justify-center p-4">
            {/* Backdrop with gradient */}
            <div
                className="absolute inset-0 bg-gradient-to-br from-[#0f1419]/95 via-[#1e2a3a]/95 to-[#0f1419]/95 backdrop-blur-sm"
                onClick={() => {
                    if (confirming) return;
                    cancel();
                }}
            />

            {/* Ambient glow effects */}
            <div className="absolute top-1/4 left-1/4 w-96 h-96 bg-[#ffd89b]/10 rounded-full blur-[120px] pointer-events-none" />
            <div className="absolute bottom-1/4 right-1/4 w-96 h-96 bg-[#d4af7a]/10 rounded-full blur-[120px] pointer-events-none" />

            <div className="relative bg-gradient-to-br from-[#1e2a3a]/95 via-[#0f1419]/90 to-[#1e2a3a]/95 backdrop-blur-2xl rounded-2xl shadow-[0_0_60px_rgba(255,216,155,0.15),0_20px_80px_rgba(0,0,0,0.6)] w-full max-w-2xl flex flex-col border-2 border-[#ffd89b]/20 overflow-hidden">
                {/* Decorative top border glow */}
                <div className="absolute top-0 left-0 right-0 h-[2px] bg-gradient-to-r from-transparent via-[#ffd89b] to-transparent opacity-60" />

                {/* Header */}
                <div className="relative flex items-center justify-between p-6 border-b border-[#ffd89b]/20 bg-gradient-to-r from-[#ffd89b]/5 to-transparent">
                    <div className="flex items-center gap-3">
                        <div className="relative p-2.5 bg-gradient-to-br from-[#ffd89b]/20 to-[#d4af7a]/20 rounded-xl border border-[#ffd89b]/30 shadow-[0_0_20px_rgba(255,216,155,0.2)]">
                            <Link2 className="w-5 h-5 text-[#ffd89b]" />
                            <div className="absolute inset-0 bg-[#ffd89b]/10 rounded-xl blur-sm" />
                        </div>
                        <div>
                            <h3 className="text-xl font-bold text-[#ffd89b]" style={{
                                textShadow: '0 0 15px rgba(255,216,155,0.3), 0 2px 4px rgba(0,0,0,0.5)'
                            }}>
                                Xác nhận mối quan hệ
                            </h3>
                            <p className="text-sm text-gray-400">
                                cho <span className="text-[#ffd89b] font-medium">{source.fullName}</span>
                            </p>
                        </div>
                    </div>
                    <button
                        onClick={() => !confirming && cancel()}
                        disabled={confirming}
                        className="p-2 hover:bg-[#ffd89b]/10 rounded-lg transition-all duration-300 text-gray-400 hover:text-[#ffd89b] border border-transparent hover:border-[#ffd89b]/30 disabled:opacity-50 disabled:cursor-not-allowed"
                    >
                        Đóng
                    </button>
                </div>

                {/* Content */}
                <div className="p-6 space-y-4">
                    {loading && (
                        <div className="flex items-center gap-2 text-gray-300">
                            <div className="w-4 h-4 border-2 border-[#ffd89b]/30 border-t-[#ffd89b] rounded-full animate-spin" />
                            <span className="text-sm">Đang gợi ý từ mô hình…</span>
                        </div>
                    )}

                    {error && (
                        <div className="p-3 bg-red-500/10 border border-red-500/30 rounded-lg text-red-400 text-sm">
                            {error}
                        </div>
                    )}

                    {!loading && !error && (
                        <>
                            {/* AI Suggestion */}
                            {selected ? (
                                <div className="p-4 rounded-xl bg-gradient-to-r from-[#ffd89b]/10 to-[#d4af7a]/10 border border-[#ffd89b]/30">
                                    <div className="flex items-center gap-2 mb-2">
                                        <Sparkles className="w-4 h-4 text-[#ffd89b]" />
                                        <span className="text-sm font-medium text-[#ffd89b]">Gợi ý tốt nhất từ AI</span>
                                    </div>
                                    <div className="flex items-center justify-between">
                                        <div className="text-white">
                                            <span className="font-semibold text-[#ffd89b]">
                                                {RELATION_LABEL[selected.relation]}
                                            </span>
                                            {" "}với{" "}
                                            <span className="font-semibold">
                                                {candidatesMap.get(selected.candidateId)?.fullName || "(???)"}
                                            </span>
                                        </div>
                                        <div className="flex items-center gap-1.5 px-3 py-1 rounded-full bg-[#ffd89b]/20 border border-[#ffd89b]/30">
                                            <Check className="w-3 h-3 text-[#ffd89b]" />
                                            <span className="text-xs font-medium text-[#ffd89b]">
                                                {(selected.confidence * 100).toFixed(0)}%
                                            </span>
                                        </div>
                                    </div>
                                </div>
                            ) : (
                                <div className="p-4 text-center text-gray-400 text-sm bg-white/5 rounded-lg border border-[#ffd89b]/10">
                                    Không có gợi ý.
                                </div>
                            )}

                            {/* Selection Form */}
                            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                                <div className="space-y-2">
                                    <label className="text-sm font-medium text-gray-300">
                                        Chọn người liên hệ
                                    </label>
                                    <select
                                        className="w-full px-4 py-2.5 bg-white/5 border border-[#ffd89b]/30 rounded-lg focus:ring-2 focus:ring-[#ffd89b]/50 focus:border-[#ffd89b] transition-all text-white outline-none"
                                        style={{
                                            colorScheme: 'dark'
                                        }}
                                        value={picked.candidateId || selected?.candidateId || ""}
                                        onChange={(e) => setPicked((s) => ({ ...s, candidateId: e.target.value }))}
                                    >
                                        <option value="" className="bg-[#1e2a3a] text-white">-- Chọn một người --</option>
                                        {persons
                                            .filter((p) => p.id !== source.id)
                                            .map((p) => (
                                                <option key={p.id} value={p.id} className="bg-[#1e2a3a] text-white">
                                                    {p.fullName}
                                                </option>
                                            ))}
                                    </select>
                                </div>

                                <div className="space-y-2">
                                    <label className="text-sm font-medium text-gray-300">
                                        Chọn loại quan hệ
                                    </label>
                                    <select
                                        className="w-full px-4 py-2.5 bg-white/5 border border-[#ffd89b]/30 rounded-lg focus:ring-2 focus:ring-[#ffd89b]/50 focus:border-[#ffd89b] transition-all text-white outline-none"
                                        style={{
                                            colorScheme: 'dark'
                                        }}
                                        value={picked.relation || selected?.relation || ""}
                                        onChange={(e) => setPicked((s) => ({ ...s, relation: e.target.value as RelationUpper }))}
                                    >
                                        <option value="" className="bg-[#1e2a3a] text-white">-- Chọn quan hệ --</option>
                                        <option value="PARENT" className="bg-[#1e2a3a] text-white">Cha/Mẹ</option>
                                        <option value="CHILD" className="bg-[#1e2a3a] text-white">Con</option>
                                        <option value="SIBLING" className="bg-[#1e2a3a] text-white">Anh/Chị/Em</option>
                                        <option value="SPOUSE" className="bg-[#1e2a3a] text-white">Vợ/Chồng</option>
                                    </select>
                                </div>
                            </div>
                        </>
                    )}
                </div>

                {/* Footer */}
                <div className="p-6 border-t border-[#ffd89b]/20 bg-gradient-to-r from-[#ffd89b]/5 to-transparent flex items-center justify-between">
                    <button
                        onClick={onBack}
                        disabled={confirming}
                        className="p-2.5 bg-white/5 hover:bg-white/10 border border-[#ffd89b]/30 hover:border-[#ffd89b]/50 rounded-lg transition-all duration-300 text-gray-300 hover:text-[#ffd89b] disabled:opacity-50 disabled:cursor-not-allowed"
                        title="Quay lại"
                    >
                        <ArrowLeft className="w-4 h-4" />
                    </button>

                    <div className="flex items-center gap-3">
                        <button
                            onClick={cancel}
                            disabled={confirming}
                            className="px-6 py-2.5 bg-white/5 hover:bg-white/10 border border-[#ffd89b]/30 hover:border-[#ffd89b]/50 text-gray-300 hover:text-white rounded-lg transition-all duration-300 font-medium disabled:opacity-50 disabled:cursor-not-allowed"
                        >
                            Hủy
                        </button>
                        <button
                            disabled={!canConfirm || confirming}
                            onClick={async () => {
                                if (!canConfirm) return;
                                setError("");
                                setConfirming(true);
                                try {
                                    await onConfirm({
                                        relation: effectiveRelation as RelationUpper,
                                        candidateId: effectiveCandidateId,
                                    });
                                    onClose();
                                } catch (e: any) {
                                    setError(e?.message || "Có lỗi xảy ra khi tạo quan hệ.");
                                } finally {
                                    setConfirming(false);
                                }
                            }}
                            className="relative overflow-hidden px-6 py-2.5 bg-gradient-to-r from-[#d4af7a] via-[#ffd89b] to-[#d4af7a] bg-[length:200%_100%] hover:bg-[position:100%] text-[#0f1419] rounded-lg transition-all duration-500 font-semibold shadow-[0_8px_30px_rgba(255,216,155,0.3)] hover:shadow-[0_12px_40px_rgba(255,216,155,0.5)] hover:scale-105 disabled:opacity-50 disabled:cursor-not-allowed disabled:hover:scale-100 group"
                        >
                            {confirming ? (
                                <span className="inline-flex items-center gap-2">
                                    <div className="w-4 h-4 border-2 border-[#0f1419]/30 border-t-[#0f1419] rounded-full animate-spin" />
                                    Đang xác nhận…
                                </span>
                            ) : (
                                "Xác nhận"
                            )}
                            <div className="absolute inset-0 -translate-x-full group-hover:translate-x-full transition-transform duration-1000 bg-gradient-to-r from-transparent via-white/30 to-transparent skew-x-12" />
                        </button>
                    </div>
                </div>

                {/* Decorative bottom border glow */}
                <div className="absolute bottom-0 left-0 right-0 h-[2px] bg-gradient-to-r from-transparent via-[#ffd89b] to-transparent opacity-60" />
            </div>
        </div>
    );
}