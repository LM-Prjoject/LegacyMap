import { useMemo, useRef, useState, useCallback, useEffect } from "react";
import Tree, { type RawNodeDatum, type RenderCustomNodeElementFn } from "react-d3-tree";
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

export interface CustomNodeDatum extends Omit<RawNodeDatum, 'children' | 'attributes'> {
    attributes: CustomNodeAttributes;
    children?: CustomNodeDatum[];
    __rd3t?: {
        depth?: number;
        collapsed?: boolean;
    };
}

interface Props {
    persons: Person[];
    relationships: Relationship[];
    onNodeClick?: (id: string) => void;
    selectedNodeId?: string | null;
}

type XY = { x: number; y: number };

const lifeText = (p: Person) => {
    const b = p.birthDate ? String(p.birthDate).slice(0, 4) : "";
    const d = (p as any).deathDate ? String((p as any).deathDate).slice(0, 4) : "";
    if (b && d) return `${b} – ${d}`;
    if (b) return b;
    return "—";
};

export default function TreeGraph({
                                      persons,
                                      relationships,
                                      onNodeClick,
                                      selectedNodeId,
                                  }: Props) {
    const containerRef = useRef<HTMLDivElement>(null);
    const [translate] = useState<{ x: number; y: number }>({x: 500, y: 100});


    const [nodePos, setNodePos] = useState<Record<string, XY>>({});
    const posRef = useRef<Record<string, XY>>({});
    const rafRef = useRef<number | null>(null);

    const recordPos = useCallback((id: string, xy: XY) => {
        const prev = posRef.current[id];
        if (!prev || prev.x !== xy.x || prev.y !== xy.y) {
            posRef.current[id] = xy;
            if (rafRef.current == null) {
                rafRef.current = requestAnimationFrame(() => {
                    setNodePos({...posRef.current});
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
        if (!persons.length) {
            return [{name: "(trống)", attributes: {id: ""}, children: []}];
        }

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
            }
            if (type === "SPOUSE") {
                spouseMap.set(r.fromPersonId, r.toPersonId);
                spouseMap.set(r.toPersonId, r.fromPersonId);
            }
        });


        const roots = persons.filter(
            (p) => !(parentMap.get(p.id)?.size ?? 0)
        );

        const built = new Set<string>();

        const buildNode = (id: string): CustomNodeDatum => {
            if (built.has(id)) {
                return {name: "(dup)", attributes: {id}, children: []};
            }
            built.add(id);

            const p = byId.get(id);
            if (!p) return {name: "(unknown)", attributes: {id}, children: []};

            const spouseId = spouseMap.get(id);
            if (spouseId && !built.has(spouseId)) {
                const spouse = byId.get(spouseId)!;
                const husband = p.gender === "MALE" ? p : spouse;
                const wife = p.gender === "MALE" ? spouse : p;

                const coupleChildren = Array.from(
                    new Set([
                        ...(children.get(husband.id) || []),
                        ...(children.get(wife.id) || []),
                    ])
                ).map(buildNode);

                // Chỗ tạo node couple
                return {
                    name: `${husband.fullName} & ${wife.fullName}`,
                    attributes: {
                        id: husband.id,
                        couple: true,
                        husbandId: husband.id,
                        wifeId: wife.id,
                        avatars: [
                            (husband as any).avatarUrl || avtFallback,
                            (wife as any).avatarUrl || avtFallback,
                        ],
                        names: [husband.fullName, wife.fullName],
                        lifes: [lifeText(husband), lifeText(wife)],
                    } satisfies CustomNodeAttributes, // ✅ thêm dòng này
                    children: coupleChildren,
                };

            }

            const childIds = Array.from(children.get(id) || []);
// Chỗ node đơn
            return {
                name: p.fullName,
                attributes: {
                    id: p.id,
                    couple: false,
                    avatar: (p as any).avatarUrl || avtFallback,
                    life: lifeText(p),
                    names: [p.fullName],
                    lifes: [lifeText(p)],
                } satisfies CustomNodeAttributes, // ✅ thêm dòng này
                children: childIds.map(buildNode),
            };
        };

        return roots.map((r) => buildNode(r.id));
    }, [persons, relationships]);


    const renderNode: RenderCustomNodeElementFn = (rd3tNode) => {
        // Type assertion to access our custom properties
        const nodeDatum = rd3tNode.nodeDatum as unknown as CustomNodeDatum;
        const attrs = nodeDatum.attributes;
        const id = attrs.id;

        if (!id) return <g />;

        recordPos(id, { x: rd3tNode.hierarchyPointNode.x, y: rd3tNode.hierarchyPointNode.y });
        const isSelected = id === selectedNodeId;

        if (attrs.couple && attrs.avatars?.length === 2) {
            const husbandName = attrs.names?.[0] || '';
            const wifeName = attrs.names?.[1] || '';
            const husbandAvatar = attrs.avatars[0];
            const wifeAvatar = attrs.avatars[1];
            const husbandLife = attrs.lifes?.[0] || '';
            const wifeLife = attrs.lifes?.[1] || '';
            const husbandId = attrs.husbandId || '';
            const wifeId = attrs.wifeId || '';

            return (
                <g>
                    {/* Chồng bên trái */}
                    <g transform="translate(-45,-40)">
                        {MemberCard(onNodeClick, selectedNodeId === husbandId)({
                            ...rd3tNode,
                            nodeDatum: {
                                ...nodeDatum,
                                name: husbandName,
                                attributes: {
                                    ...attrs,
                                    id: husbandId,
                                    avatar: husbandAvatar,
                                    life: husbandLife,
                                },
                            },
                        })}
                    </g>

                    {/* Vợ bên phải */}
                    <g transform="translate(45,-40)">
                        {MemberCard(onNodeClick, selectedNodeId === wifeId)({
                            ...rd3tNode,
                            nodeDatum: {
                                ...nodeDatum,
                                name: wifeName,
                                attributes: {
                                    ...attrs,
                                    id: wifeId,
                                    avatar: wifeAvatar,
                                    life: wifeLife,
                                },
                            },
                        })}
                    </g>
                </g>
            );
        }

        // For non-couple nodes, ensure life text is properly passed
        const lifeText = attrs.life || '';
        return MemberCard(onNodeClick, isSelected)({
            ...rd3tNode,
            nodeDatum: {
                ...nodeDatum,
                attributes: {
                    ...attrs,
                    life: lifeText,
                },
            },
        });
    };

    const lines = useMemo(() => {
        return relationships
            .map((r) => {
                const type = String(r.type).toUpperCase();
                if (type !== "PARENT" && type !== "CHILD") return null;

                const a = nodePos[r.fromPersonId];
                const b = nodePos[r.toPersonId];
                if (!a || !b) return null;

                const p1 = {x: translate.x + a.y, y: translate.y + a.x};
                const p2 = {x: translate.x + b.y, y: translate.y + b.x};

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
            })
            .filter(Boolean);
    }, [nodePos, relationships, translate]);

    return (
        <div
            ref={containerRef}
            className="relative w-full h-[70vh] bg-white rounded-xl shadow-inner overflow-hidden"
        >
            <svg className="absolute inset-0 w-full h-full pointer-events-none">
                {lines}
            </svg>

            <Tree
                data={data as any} // Type assertion to bypass the type checking for now
                translate={translate}
                orientation="vertical"
                zoomable
                nodeSize={{x: 220, y: 180}}
                separation={{siblings: 2, nonSiblings: 2.4}}
                renderCustomNodeElement={renderNode}
            />
        </div>
    );

}