import { useMemo, useRef, useState, useCallback, useEffect } from "react";
import Tree, { RawNodeDatum, type RenderCustomNodeElementFn } from "react-d3-tree";
import type { Person, Relationship } from "@/api/trees";
import { MemberCard } from "@/components/familyTree/memberModal/MemberCard.tsx";
import avtFallback from "@/assets/avt.jpg";

interface Props {
    persons: Person[];
    relationships: Relationship[];
    onNodeClick?: (id: string) => void;
}

type XY = { x: number; y: number };

const lifeText = (p: Person) => {
    const b = p.birthDate ? String(p.birthDate).slice(0, 4) : "";
    const d = (p as any).deathDate ? String((p as any).deathDate).slice(0, 4) : "";
    if (b && d) return `${b} – ${d}`;
    if (b) return b;
    return "—";
};

export default function TreeGraph({ persons, relationships, onNodeClick }: Props) {
    const containerRef = useRef<HTMLDivElement>(null);
    const [translate] = useState<{ x: number; y: number }>({ x: 400, y: 100 });

    const [nodePos, setNodePos] = useState<Record<string, XY>>({});
    const posRef = useRef<Record<string, XY>>({});
    const rafRef = useRef<number | null>(null);

    const recordPos = useCallback((id: string, xy: XY) => {
        const prev = posRef.current[id];
        if (!prev || prev.x !== xy.x || prev.y !== xy.y) {
            posRef.current[id] = xy;
            if (rafRef.current == null) {
                rafRef.current = requestAnimationFrame(() => {
                    setNodePos({ ...posRef.current });
                    rafRef.current = null;
                });
            }
        }
    }, []);

    useEffect(() => {
        return () => {
            if (rafRef.current != null) cancelAnimationFrame(rafRef.current);
        };
    }, []);

    const data = useMemo<RawNodeDatum[]>(() => {
        const byId = new Map(persons.map((p) => [p.id, p]));
        const children = new Map<string, string[]>();
        persons.forEach((p) => children.set(p.id, []));

        relationships.forEach((r) => {
            const t = String(r.type).toUpperCase();
            if (t === "PARENT") children.get(r.fromPersonId)?.push(r.toPersonId);
            if (t === "CHILD") children.get(r.toPersonId)?.push(r.fromPersonId);
        });

        const hasParent = new Set<string>();
        relationships.forEach((r) => {
            const t = String(r.type).toUpperCase();
            if (t === "PARENT") hasParent.add(r.toPersonId);
            if (t === "CHILD") hasParent.add(r.fromPersonId);
        });

        const roots = persons.filter((p) => !hasParent.has(p.id));

        const toNode = (id: string): RawNodeDatum => {
            const p = byId.get(id)!;
            return {
                name: p?.fullName || "(unknown)",
                attributes: {
                    id,
                    avatar: (p as any).avatarUrl || avtFallback,
                    life: lifeText(p),
                },
                children: (children.get(id) || []).map(toNode),
            };
        };

        const forest = roots.map((r) => toNode(r.id));
        return forest.length ? forest : [{ name: "(trống)", attributes: { id: "" }, children: [] }];
    }, [persons, relationships]);

    const renderNode: RenderCustomNodeElementFn = (args) => {
        const id = String(args.nodeDatum?.attributes?.id ?? "");
        if (id) {
            recordPos(id, { x: args.hierarchyPointNode.x, y: args.hierarchyPointNode.y });
            return MemberCard(onNodeClick)(args);
        }
        return <g />;
    };

    const lines = useMemo(() => {
        return relationships
            .map((r) => {
                const type = String(r.type).toUpperCase();
                const a = nodePos[r.fromPersonId];
                const b = nodePos[r.toPersonId];
                if (!a || !b) return null;

                const p1 = { x: translate.x + a.y, y: translate.y + a.x };
                const p2 = { x: translate.x + b.y, y: translate.y + b.x };

                if (type === "PARENT" || type === "CHILD") {
                    return (
                        <line
                            key={`${r.fromPersonId}-${r.toPersonId}-${r.type}`}
                            x1={p1.x}
                            y1={p1.y}
                            x2={p2.x}
                            y2={p2.y}
                            stroke="#222"
                            strokeWidth={2}
                        />
                    );
                }

                if (type === "SPOUSE" || type === "SIBLING") {
                    const y = (p1.y + p2.y) / 2;
                    return (
                        <line
                            key={`${r.fromPersonId}-${r.toPersonId}-${r.type}`}
                            x1={p1.x}
                            y1={y}
                            x2={p2.x}
                            y2={y}
                            stroke="#7c3aed"
                            strokeWidth={2.4}
                        />
                    );
                }
                return null;
            })
            .filter(Boolean);
    }, [nodePos, relationships, translate]);


    return (
        <div
            ref={containerRef}
            className="relative w-full h-[70vh] bg-white rounded-xl shadow-inner overflow-hidden"
        >
            <svg className="absolute inset-0 w-full h-full pointer-events-none">{lines}</svg>

            <Tree
                data={data}
                translate={translate}
                orientation="vertical"
                zoomable
                nodeSize={{ x: 160, y: 160 }}
                separation={{ siblings: 1.8, nonSiblings: 2.4 }}
                renderCustomNodeElement={renderNode}
            />
        </div>
    );
}