import { useMemo, useRef, useState, useCallback, useEffect } from "react";
import Tree, { type RawNodeDatum, type RenderCustomNodeElementFn, type CustomNodeElementProps } from "react-d3-tree";
import type { Person, Relationship } from "@/api/trees";
import { MemberCard } from "@/components/familyTree/memberModal/MemberCard.tsx";
import avtFallback from "@/assets/avt.jpg";

export interface CustomNodeAttributes {
    id: string;
    couple?: boolean;
    husbandId?: string;
    wifeId?: string;
    avatars?: string[];
    names?: string[];
    lifes?: string[];
    avatar?: string;
    life?: string;
    [key: string]: string | number | boolean | string[] | undefined;
}

export interface CustomNodeDatum extends Omit<RawNodeDatum, "children" | "attributes"> {
    attributes: CustomNodeAttributes;
    children?: CustomNodeDatum[];
    __rd3t?: { depth?: number; collapsed?: boolean };
}

interface Props {
    persons: Person[];
    relationships: Relationship[];
    onNodeClick?: (id: string) => void;
    selectedNodeId?: string | null;
    onEmptyClick?: () => void;
}

type XY = { x: number; y: number };

const lifeTextOf = (p: Person) => {
    const b = p.birthDate ? String(p.birthDate).slice(0, 4) : "";
    const d = (p as any).deathDate ? String((p as any).deathDate).slice(0, 4) : "";
    if (b && d) return `${b} – ${d}`;
    if (b) return b;
    return "—";
};

const toRawAttrs = (a: Partial<CustomNodeAttributes>): Record<string, string | number | boolean> => ({
    id: String(a.id ?? ""),
    avatar: String(a.avatar ?? ""),
    life: String(a.life ?? ""),
});

export default function TreeGraph({ persons, relationships, onNodeClick, selectedNodeId, onEmptyClick }: Props) {
    const containerRef = useRef<HTMLDivElement>(null);
    const [translate] = useState<{ x: number; y: number }>({ x: 500, y: 100 });

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

    const data = useMemo<CustomNodeDatum[]>(() => {
        if (!persons.length) return [{ name: "(trống)", attributes: { id: "" }, children: [] }];

        const byId = new Map(persons.map((p) => [p.id, p]));
        const children = new Map<string, Set<string>>();
        const parentMap = new Map<string, Set<string>>();
        const spouseMap = new Map<string, string>();

        persons.forEach((p) => {
            children.set(p.id, new Set());
            parentMap.set(p.id, new Set());
        });

        relationships.forEach((r) => {
            const type = String(r.type).toUpperCase();

            if (type === "PARENT") {
                children.get(r.fromPersonId)?.add(r.toPersonId);
                parentMap.get(r.toPersonId)?.add(r.fromPersonId);
            } else if (type === "CHILD") {
                children.get(r.toPersonId)?.add(r.fromPersonId);
                parentMap.get(r.fromPersonId)?.add(r.toPersonId);
            } else if (type === "SPOUSE") {
                spouseMap.set(r.fromPersonId, r.toPersonId);
                spouseMap.set(r.toPersonId, r.fromPersonId);
            }
        });

        const rootsRaw = persons.filter((p) => !(parentMap.get(p.id)?.size ?? 0));
        const seenCouple = new Set<string>();
        const roots = rootsRaw.filter((p) => {
            const s = spouseMap.get(p.id);
            if (!s) return true;
            const key = [p.id, s].sort().join("|");
            if (seenCouple.has(key)) return false;
            seenCouple.add(key);
            return true;
        });

        const built = new Set<string>();
        const buildNode = (id: string): CustomNodeDatum => {
            if (built.has(id)) return { name: "(dup)", attributes: { id }, children: [] };
            built.add(id);

            const p = byId.get(id)!;
            const spouseId = spouseMap.get(id);

            if (spouseId && !built.has(spouseId)) {
                built.add(spouseId);
                const spouse = byId.get(spouseId)!;
                const husband = p.gender === "MALE" ? p : spouse;
                const wife = p.gender === "MALE" ? spouse : p;

                const coupleChildren = Array.from(
                    new Set([...(children.get(husband.id) || []), ...(children.get(wife.id) || [])])
                ).map(buildNode);

                return {
                    name: husband.fullName + " & " + wife.fullName,
                    attributes: {
                        id: husband.id,
                        couple: true,
                        husbandId: husband.id,
                        wifeId: wife.id,
                        avatars: [(husband as any).avatarUrl || avtFallback, (wife as any).avatarUrl || avtFallback],
                        names: [husband.fullName, wife.fullName],
                        lifes: [lifeTextOf(husband), lifeTextOf(wife)],
                    },
                    children: coupleChildren,
                };
            }

            const childIds = Array.from(children.get(id) || []);
            return {
                name: p.fullName,
                attributes: {
                    id: p.id,
                    couple: false,
                    avatar: (p as any).avatarUrl || avtFallback,
                    life: lifeTextOf(p),
                    names: [p.fullName],
                    lifes: [lifeTextOf(p)],
                },
                children: childIds.map(buildNode),
            };
        };

        return roots.map((r) => buildNode(r.id));
    }, [persons, relationships]);

    const EmptyCard: RenderCustomNodeElementFn = () => (
        <g onClick={onEmptyClick} style={{ cursor: onEmptyClick ? "pointer" : "default" }}>
            <rect x={-60} y={-80} width={120} height={160} rx={10} ry={10} fill="#e5e7eb" stroke="#cbd5e1" strokeWidth={1.5} />
            <text x={0} y={0} textAnchor="middle" fontSize={14} fill="#111827">Trống</text>
        </g>
    );

    const renderNode: RenderCustomNodeElementFn = (rd3tNode: CustomNodeElementProps) => {
        const nodeDatum = rd3tNode.nodeDatum as unknown as { attributes?: CustomNodeAttributes };
        const attrs = (nodeDatum.attributes ?? { id: "" }) as CustomNodeAttributes;
        const id = attrs.id;
        if (!id) return EmptyCard(rd3tNode);

        recordPos(id, { x: rd3tNode.hierarchyPointNode.x, y: rd3tNode.hierarchyPointNode.y });
        const isSelected = id === selectedNodeId;

        if (attrs.couple && Array.isArray(attrs.avatars) && attrs.avatars.length === 2) {
            const [husbandName = "", wifeName = ""] = attrs.names || [];
            const [husbandAvatar, wifeAvatar] = attrs.avatars;
            const [husbandLife = "—", wifeLife = "—"] = attrs.lifes || [];
            const husbandId = attrs.husbandId || "";
            const wifeId = attrs.wifeId || "";

            return (
                <g>
                    <g transform="translate(-45,-40)">
                        {MemberCard(onNodeClick, selectedNodeId === husbandId)({
                            ...rd3tNode,
                            nodeDatum: {
                                ...rd3tNode.nodeDatum,
                                name: husbandName,
                                attributes: toRawAttrs({ id: husbandId, avatar: husbandAvatar || avtFallback, life: husbandLife }),
                            } as RawNodeDatum,
                        } as CustomNodeElementProps)}
                    </g>
                    <g transform="translate(45,-40)">
                        {MemberCard(onNodeClick, selectedNodeId === wifeId)({
                            ...rd3tNode,
                            nodeDatum: {
                                ...rd3tNode.nodeDatum,
                                name: wifeName,
                                attributes: toRawAttrs({ id: wifeId, avatar: wifeAvatar || avtFallback, life: wifeLife }),
                            } as RawNodeDatum,
                        } as CustomNodeElementProps)}
                    </g>
                </g>
            );
        }

        return MemberCard(onNodeClick, isSelected)({
            ...rd3tNode,
            nodeDatum: {
                ...rd3tNode.nodeDatum,
                attributes: toRawAttrs({ id, avatar: (attrs.avatar as string) || avtFallback, life: (attrs.life as string) || "—" }),
            } as RawNodeDatum,
        } as CustomNodeElementProps);
    };

    const hasEdges = useMemo(
        () => relationships.some((r) => {
            const t = String(r.type).toUpperCase();
            return t === "PARENT" || t === "CHILD";
        }),
        [relationships]
    );

    const [viewport, setViewport] = useState<{ k: number; x: number; y: number }>({ k: 1, x: translate.x, y: translate.y });
    const lastViewport = useRef<{ k: number; x: number; y: number }>(viewport);

    const handleUpdate = useCallback((state: any) => {
        const k = state.zoom ?? state.scale ?? 1;
        const tr = state.translate ?? {};
        const next = { k, x: tr.x ?? translate.x, y: tr.y ?? translate.y };
        const prev = lastViewport.current;
        if (prev.k === next.k && prev.x === next.x && prev.y === next.y) return;
        lastViewport.current = next;
        setViewport(next);
    }, [translate.x, translate.y]);

    const lines = useMemo(() => {
        if (!hasEdges) return null as any;
        return relationships.map((r) => {
            const type = String(r.type).toUpperCase();
            if (type !== "PARENT" && type !== "CHILD") return null;
            const a = nodePos[r.fromPersonId];
            const b = nodePos[r.toPersonId];
            if (!a || !b) return null;
            const p1 = { x: a.y, y: a.x };
            const p2 = { x: b.y, y: b.x };
            return <line key={`${r.fromPersonId}-${r.toPersonId}-${r.type}`} x1={p1.x} y1={p1.y} x2={p2.x} y2={p2.y} stroke="#222" strokeWidth={2} />;
        }).filter(Boolean);
    }, [nodePos, relationships, hasEdges]);

    return (
        <div ref={containerRef} className="relative w-full h-[70vh] bg-white rounded-xl shadow-inner overflow-hidden">
            {hasEdges && (
                <svg className="absolute inset-0 w-full h-full pointer-events-none">
                    <g transform={`translate(${viewport.x},${viewport.y}) scale(${viewport.k})`}>{lines}</g>
                </svg>
            )}
            <Tree
                data={data as any}
                translate={translate}
                orientation="vertical"
                zoomable
                nodeSize={{ x: 220, y: 180 }}
                separation={{ siblings: 2, nonSiblings: 2.4 }}
                renderCustomNodeElement={renderNode}
                onUpdate={handleUpdate}
                collapsible={false}
                transitionDuration={0}
            />
        </div>
    );
}