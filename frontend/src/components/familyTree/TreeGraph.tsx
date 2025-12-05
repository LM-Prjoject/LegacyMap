import { useMemo, useRef, useState, useCallback, useEffect } from "react";
import Tree, { type RawNodeDatum, type RenderCustomNodeElementFn, type CustomNodeElementProps } from "react-d3-tree";
import type { Person, Relationship } from "@/api/trees";
import { MemberCard } from "@/components/familyTree/memberModal/MemberCard.tsx";

export interface CustomNodeAttributes {
    id: string;
    couple?: boolean;
    husbandId?: string;
    wifeId?: string;
    memberIds?: string[];
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

    const normalizedRels = useMemo(
        () =>
            relationships.map((r) => {
                const t = String(r.type).toUpperCase();
                if (t === "CHILD") {
                    return {
                        ...r,
                        type: "PARENT",
                        fromPersonId: r.toPersonId,
                        toPersonId: r.fromPersonId,
                    };
                }
                return {
                    ...r,
                    type: t as any,
                };
            }),
        [relationships]
    );

    const parentsByChild = useMemo(() => {
        const out = new Map<
            string,
            { fathers: Set<string>; mothers: Set<string>; others: Set<string> }
        >();

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

        // 1) Ghi nhận các parent thật từ quan hệ PARENT
        for (const r of normalizedRels) {
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

        for (const [, bucket] of out.entries()) {
            const allParents = new Set<string>([
                ...bucket.fathers,
                ...bucket.mothers,
                ...bucket.others,
            ]);

            if (allParents.size !== 1) continue;

            const [onlyParentId] = Array.from(allParents);

            const spouseIds = normalizedRels
                .filter(
                    (r) =>
                        String(r.type).toUpperCase() === "SPOUSE" &&
                        (r.fromPersonId === onlyParentId ||
                            r.toPersonId === onlyParentId)
                )
                .map((r) =>
                    r.fromPersonId === onlyParentId ? r.toPersonId : r.fromPersonId
                );

            for (const spId of spouseIds) {
                if (allParents.has(spId)) continue;

                const g = genderOf.get(spId);
                if (g === "MALE") bucket.fathers.add(spId);
                else if (g === "FEMALE") bucket.mothers.add(spId);
                else if (g === "OTHER") bucket.others.add(spId);
                else bucket.others.add(spId);
            }
        }

        return out;
    }, [normalizedRels, genderOf]);

    const highlightSets = useMemo(() => {
        if (!hoveredId)
            return {
                fathers: new Set<string>(),
                mothers: new Set<string>(),
                others: new Set<string>(),
            };
        return (
            parentsByChild.get(hoveredId) ?? {
                fathers: new Set(),
                mothers: new Set(),
                others: new Set(),
            }
        );
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
        const spouseMap = new Map<string, Set<string>>();

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
                if (!spouseMap.has(r.fromPersonId)) spouseMap.set(r.fromPersonId, new Set<string>());
                if (!spouseMap.has(r.toPersonId)) spouseMap.set(r.toPersonId, new Set<string>());
                spouseMap.get(r.fromPersonId)!.add(r.toPersonId);
                spouseMap.get(r.toPersonId)!.add(r.fromPersonId);
            }
        });

        const rootsRaw = persons.filter((p) => {
            const hasParents = (parentMap.get(p.id)?.size ?? 0) > 0;
            if (hasParents) return false;
            const spSet = spouseMap.get(p.id);
            if (spSet && spSet.size) {
                for (const spId of spSet) {
                    const spouseHasParents = (parentMap.get(spId)?.size ?? 0) > 0;
                    if (spouseHasParents) return false;
                }
            }
            return true;
        });

        const getSpouseGroup = (startId: string): Set<string> => {
            const visited = new Set<string>();
            const stack: string[] = [];
            visited.add(startId);
            stack.push(startId);
            while (stack.length) {
                const cur = stack.pop()!;
                const neighbors = spouseMap.get(cur);
                if (!neighbors) continue;
                for (const nb of neighbors) {
                    if (!visited.has(nb)) {
                        visited.add(nb);
                        stack.push(nb);
                    }
                }
            }
            return visited;
        };

        const seenGroup = new Set<string>();
        const roots = rootsRaw.filter((p) => {
            const group = Array.from(getSpouseGroup(p.id)).sort();
            const key = group.join("|");
            if (!key) return true;
            if (seenGroup.has(key)) return false;
            seenGroup.add(key);
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

            const groupSet = getSpouseGroup(id);
            if (groupSet && groupSet.size > 1) {
                const memberIds = Array.from(groupSet);
                const members = memberIds.map(mid => byId.get(mid)).filter(Boolean) as Person[];
                if (members.length) {

                    const allChildren = new Set<string>();
                    for (const m of members) {
                        for (const cid of Array.from(children.get(m.id) || [])) allChildren.add(cid);
                    }
                    const groupChildren = Array.from(allChildren).map((cid) => buildNode(cid, nextPath));

                    return {
                        name: members.map(m => m.fullName).join(" & "),
                        attributes: {
                            id: members[0].id,
                            couple: true,
                            memberIds: members.map(m => m.id),
                            avatars: members.map(m => (m as any).avatarUrl ?? ""),
                            names: members.map(m => m.fullName),
                            lifes: members.map(m => lifeTextOf(m)),
                        } as any,
                        children: groupChildren,
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

        if (attrs.couple && Array.isArray(attrs.names) && attrs.names.length >= 1) {
            const names = attrs.names || [];
            const avatars = attrs.avatars || [];
            const lifes = attrs.lifes || [];
            const memberIds = (attrs as any).memberIds as string[] | undefined;

            // Record same layout position for every member in the group so edges can map
            if (Array.isArray(memberIds)) {
                for (const mid of memberIds) {
                    if (mid) recordPos(mid, { x: rd3tNode.hierarchyPointNode.x, y: rd3tNode.hierarchyPointNode.y });
                }
            }

            const count = names.length;
            const spacing = 95; // horizontal spacing between spouse cards
            const totalWidth = (count - 1) * spacing;
            const startX = -totalWidth / 2;

            return (
                <g>
                    {names.map((nm, i) => {
                        const mid = memberIds?.[i] || "";
                        const av = avatars[i] || "";
                        const lf = lifes[i] || "—";
                        const tx = startX + i * spacing;
                        return (
                            <g
                                key={mid || i}
                                transform={`translate(${tx},-40)`}
                                onMouseEnter={() => mid && setHoveredId(mid)}
                                onMouseLeave={() => setHoveredId(null)}
                            >
                                {mid && colorForNode(mid) && <HighlightBox color={colorForNode(mid)!} />}
                                {MemberCard(onNodeClick, mid ? selectedNodeId === mid : false)({
                                    ...rd3tNode,
                                    nodeDatum: {
                                        ...rd3tNode.nodeDatum,
                                        name: nm,
                                        attributes: toRawAttrs({ id: mid, avatar: av, life: lf }),
                                    } as RawNodeDatum,
                                } as CustomNodeElementProps)}
                            </g>
                        );
                    })}
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
        () =>
            normalizedRels.some((r) =>
                ["PARENT", "CHILD"].includes(String(r.type).toUpperCase())
            ),
        [normalizedRels]
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

        return normalizedRels
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
                    else if (g === "OTHER") stroke = COLOR_OTHER;
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
    }, [nodePos, normalizedRels, hasEdges, hoveredId, genderOf]);


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