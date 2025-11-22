import { useMemo, useRef, useState, useCallback, useEffect } from "react";
import Tree, { type RawNodeDatum, type RenderCustomNodeElementFn, type CustomNodeElementProps } from "react-d3-tree";
import type { Person, Relationship } from "@/api/trees";
import { MemberCard } from "@/components/familyTree/memberModal/MemberCard.tsx";

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

const COLOR_FATHER = "#3b82f6";
const COLOR_MOTHER = "#ec4899";
const COLOR_OTHER = "#cc33ff";

export default function TreeGraph({ persons, relationships, onNodeClick, selectedNodeId, onEmptyClick }: Props) {
    const containerRef = useRef<HTMLDivElement>(null);
    const [translate] = useState<{ x: number; y: number }>({ x: 500, y: 120 });
    const [nodePos, setNodePos] = useState<Record<string, XY>>({});
    const posRef = useRef<Record<string, XY>>({});
    const rafRef = useRef<number | null>(null);
    const [hoveredId, setHoveredId] = useState<string | null>(null);

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

    const genderOf = useMemo(() => {
        const m = new Map<string, string>();
        for (const p of persons) m.set(p.id, String(p.gender ?? "").toUpperCase());
        return m;
    }, [persons]);

    const parentsByChild = useMemo(() => {
        const out = new Map<string, { fathers: Set<string>; mothers: Set<string>; others: Set<string> }>();
        const ensure = (id: string) => {
            if (!out.has(id)) {
                out.set(id, {
                    fathers: new Set<string>(),
                    mothers: new Set<string>(),
                    others: new Set<string>(),
                });
            }
            return out.get(id)!;
        };
        for (const r of relationships) {
            const t = String(r.type).toUpperCase();
            if (t !== "PARENT") continue;
            const parentId = r.fromPersonId;
            const childId = r.toPersonId;
            const g = genderOf.get(parentId);
            const bucket = ensure(childId);
            if (g === "MALE") bucket.fathers.add(parentId);
            else if (g === "FEMALE") bucket.mothers.add(parentId);
            else if (g === "OTHER") bucket.others.add(parentId);
            else bucket.others.add(parentId);
        }

        const spouses = new Map<string, Set<string>>();
        const ensureSpouse = (id: string) => {
            if (!spouses.has(id)) spouses.set(id, new Set<string>());
            return spouses.get(id)!;
        };

        for (const r of relationships) {
            const t = String(r.type).toUpperCase();
            if (t !== "SPOUSE") continue;
            ensureSpouse(r.fromPersonId).add(r.toPersonId);
            ensureSpouse(r.toPersonId).add(r.fromPersonId);
        }

        for (const [_childId, buckets] of out.entries()) {
            const allParents = new Set<string>([
                ...buckets.fathers,
                ...buckets.mothers,
                ...buckets.others,
            ]);

            for (const pid of Array.from(allParents)) {
                const spSet = spouses.get(pid);
                if (!spSet) continue;

                for (const sp of spSet) {
                    if (allParents.has(sp)) continue;

                    const g = genderOf.get(sp);
                    if (g === "MALE") {
                        buckets.fathers.add(sp);
                    } else if (g === "FEMALE") {
                        buckets.mothers.add(sp);
                    } else {
                        buckets.others.add(sp);
                    }
                }
            }
        }

        return out;
    }, [relationships, genderOf]);

    const highlightSets = useMemo(() => {
        if (!hoveredId) return { fathers: new Set<string>(), mothers: new Set<string>(), others: new Set<string>() };
        return parentsByChild.get(hoveredId) ?? { fathers: new Set(), mothers: new Set(), others: new Set() };
    }, [hoveredId, parentsByChild]);

    const colorForNode = useCallback(
        (id: string): string | null => {
            if (!id) return null;
            if (id === hoveredId) {
                const g = genderOf.get(id);
                return g === "FEMALE"
                    ? COLOR_MOTHER
                    : g === "MALE"
                        ? COLOR_FATHER
                        : g === "OTHER"
                            ? COLOR_OTHER
                            : null;
            }
            if (highlightSets.fathers.has(id)) return COLOR_FATHER;
            if (highlightSets.mothers.has(id)) return COLOR_MOTHER;
            if (highlightSets.others.has(id)) return COLOR_OTHER;
            return null;
        },
        [hoveredId, genderOf, highlightSets]
    );

    const HighlightBox = ({ color }: { color: string }) => (
        <rect
            x={-42.5}
            y={-66}
            width={85}
            height={118}
            rx={5}
            ry={5}
            fill={color}
            stroke={color}
            strokeWidth={3}
        />
    );

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
            } else if (type === "SPOUSE") {
                spouseMap.set(r.fromPersonId, r.toPersonId);
                spouseMap.set(r.toPersonId, r.fromPersonId);
            }
        });

        const rootsRaw = persons.filter((p) => {
            const hasParents = (parentMap.get(p.id)?.size ?? 0) > 0;
            if (hasParents) return false;

            const spouseId = spouseMap.get(p.id);
            if (spouseId) {
                const spouseHasParents = (parentMap.get(spouseId)?.size ?? 0) > 0;
                if (spouseHasParents) return false;
            }
            return true;
        });

        const seenCouple = new Set<string>();
        const roots = rootsRaw.filter((p) => {
            const s = spouseMap.get(p.id);
            if (!s) return true;
            const key = [p.id, s].sort().join("|");
            if (seenCouple.has(key)) return false;
            seenCouple.add(key);
            return true;
        });

        const buildNode = (id: string, path: Set<string>): CustomNodeDatum => {
            if (path.has(id)) {
                return {
                    name: "(loop)",
                    attributes: { id },
                    children: [],
                };
            }

            const p = byId.get(id);
            if (!p) {
                return {
                    name: "(missing)",
                    attributes: { id },
                    children: [],
                };
            }

            const nextPath = new Set(path);
            nextPath.add(id);

            const spouseId = spouseMap.get(id);

            if (spouseId) {
                const spouse = byId.get(spouseId);
                if (spouse) {
                    const husband = p.gender === "MALE" ? p : spouse;
                    const wife = p.gender === "MALE" ? spouse : p;

                    const coupleChildren = Array.from(
                        new Set([
                            ...(children.get(husband.id) || []),
                            ...(children.get(wife.id) || []),
                        ])
                    ).map((cid) => buildNode(cid, nextPath));

                    return {
                        name: husband.fullName + " & " + wife.fullName,
                        attributes: {
                            id: husband.id,
                            couple: true,
                            husbandId: husband.id,
                            wifeId: wife.id,
                            avatars: [
                                (husband as any).avatarUrl ?? "",
                                (wife as any).avatarUrl ?? "",
                            ],
                            names: [husband.fullName, wife.fullName],
                            lifes: [lifeTextOf(husband), lifeTextOf(wife)],
                        },
                        children: coupleChildren,
                    };
                }
            }

            const childIds = Array.from(children.get(id) || []);
            return {
                name: p.fullName,
                attributes: {
                    id: p.id,
                    couple: false,
                    avatar: (p as any).avatarUrl ?? "",
                    life: lifeTextOf(p),
                    names: [p.fullName],
                    lifes: [lifeTextOf(p)],
                },
                children: childIds.map((cid) => buildNode(cid, nextPath)),
            };
        };

        return roots.map((r) => buildNode(r.id, new Set()));
    }, [persons, relationships]);

    const EmptyCard: RenderCustomNodeElementFn = () => (
        <g onClick={onEmptyClick} style={{ cursor: onEmptyClick ? "pointer" : "default" }}>
            <rect
                x={-60}
                y={-80}
                width={120}
                height={160}
                rx={10}
                ry={10}
                fill="#e5e7eb"
                stroke="#cbd5e1"
                strokeWidth={1.5}
            />
            <text x={0} y={0} textAnchor="middle" fontSize={14} fill="#111827">
                Trống
            </text>
        </g>
    );

    const renderNode: RenderCustomNodeElementFn = (rd3tNode: CustomNodeElementProps) => {
        const nodeDatum = rd3tNode.nodeDatum as unknown as { attributes?: CustomNodeAttributes };
        const attrs = (nodeDatum.attributes ?? { id: "" }) as CustomNodeAttributes;
        const id = attrs.id;
        if (!id) return EmptyCard(rd3tNode);

        recordPos(id, { x: rd3tNode.hierarchyPointNode.x, y: rd3tNode.hierarchyPointNode.y });
        const color = colorForNode(id);

        if (attrs.couple && Array.isArray(attrs.avatars) && attrs.avatars.length === 2) {
            const [husbandName, wifeName] = attrs.names || ["", ""];
            const [husbandAvatar, wifeAvatar] = attrs.avatars;
            const [husbandLife, wifeLife] = attrs.lifes || ["—", "—"];
            const husbandId = attrs.husbandId || "";
            const wifeId = attrs.wifeId || "";

            return (
                <g>
                    <g
                        transform="translate(-45,-40)"
                        onMouseEnter={() => setHoveredId(husbandId)}
                        onMouseLeave={() => setHoveredId(null)}
                    >
                        {colorForNode(husbandId) && <HighlightBox color={colorForNode(husbandId)!} />}
                        {MemberCard(onNodeClick, selectedNodeId === husbandId)({
                            ...rd3tNode,
                            nodeDatum: {
                                ...rd3tNode.nodeDatum,
                                name: husbandName,
                                attributes: toRawAttrs({ id: husbandId, avatar: husbandAvatar, life: husbandLife }),
                            } as RawNodeDatum,
                        } as CustomNodeElementProps)}
                    </g>

                    <g
                        transform="translate(45,-40)"
                        onMouseEnter={() => setHoveredId(wifeId)}
                        onMouseLeave={() => setHoveredId(null)}
                    >
                        {colorForNode(wifeId) && <HighlightBox color={colorForNode(wifeId)!} />}
                        {MemberCard(onNodeClick, selectedNodeId === wifeId)({
                            ...rd3tNode,
                            nodeDatum: {
                                ...rd3tNode.nodeDatum,
                                name: wifeName,
                                attributes: toRawAttrs({ id: wifeId, avatar: wifeAvatar, life: wifeLife }),
                            } as RawNodeDatum,
                        } as CustomNodeElementProps)}
                    </g>
                </g>
            );
        }

        return (
            <g onMouseEnter={() => setHoveredId(id)} onMouseLeave={() => setHoveredId(null)}>
                {color && <HighlightBox color={color} />}
                {MemberCard(onNodeClick, selectedNodeId === id)({
                    ...rd3tNode,
                    nodeDatum: {
                        ...rd3tNode.nodeDatum,
                        attributes: toRawAttrs({
                            id,
                            avatar: (attrs.avatar as string) || "",
                            life: (attrs.life as string) || "—",
                        }),
                    } as RawNodeDatum,
                } as CustomNodeElementProps)}
            </g>
        );
    };

    const hasEdges = useMemo(
        () => relationships.some((r) => ["PARENT", "CHILD"].includes(String(r.type).toUpperCase())),
        [relationships]
    );

    const [viewport, setViewport] = useState<{ k: number; x: number; y: number }>({
        k: 1,
        x: translate.x,
        y: translate.y,
    });
    const lastViewport = useRef(viewport);

    const handleUpdate = useCallback(
        (state: any) => {
            const k = state.zoom ?? state.scale ?? 1;
            const tr = state.translate ?? {};
            const next = { k, x: tr.x ?? translate.x, y: tr.y ?? translate.y };
            const prev = lastViewport.current;
            if (prev.k === next.k && prev.x === next.x && prev.y === next.y) return;
            lastViewport.current = next;
            setViewport(next);
        },
        [translate.x, translate.y]
    );

    const lines = useMemo(() => {
        if (!hasEdges) return null;
        return relationships
            .map((r) => {
                const type = String(r.type).toUpperCase();
                if (type !== "PARENT" && type !== "CHILD") return null;

                const parentId = type === "PARENT" ? r.fromPersonId : r.toPersonId;
                const childId = type === "PARENT" ? r.toPersonId : r.fromPersonId;

                const a = nodePos[parentId];
                const b = nodePos[childId];
                if (!a || !b) return null;
                const p1 = { x: a.y, y: a.x };
                const p2 = { x: b.y, y: b.x };

                let stroke = "#222";
                let strokeWidth = 2;

                if (hoveredId && hoveredId === childId) {
                    const g = genderOf.get(parentId);
                    if (g === "MALE") stroke = COLOR_FATHER;
                    else if (g === "FEMALE") stroke = COLOR_MOTHER;
                    strokeWidth = 3.5;
                }

                return (
                    <line
                        key={`${r.fromPersonId}-${r.toPersonId}-${r.type}`}
                        x1={p1.x}
                        y1={p1.y}
                        x2={p2.x}
                        y2={p2.y}
                        stroke={stroke}
                        strokeWidth={strokeWidth}
                    />
                );
            })
            .filter(Boolean);
    }, [nodePos, relationships, hasEdges, hoveredId, genderOf]);

    return (
        <div
            ref={containerRef}
            className="relative w-full h-[70vh] bg-white rounded-xl shadow-inner overflow-hidden"
            onMouseLeave={() => setHoveredId(null)}
        >
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