import type { RenderCustomNodeElementFn, CustomNodeElementProps } from "react-d3-tree";
import type { CustomNodeDatum } from "../TreeGraph";
import avt from "@/assets/avt.jpg";

const uniqueClipId = (base?: string) =>
    `avatarClip-${(base && String(base)) || Math.random().toString(36).slice(2)}`;


export const MemberCard = (
    onClick?: (id: string) => void,
    isSelected: boolean = false
): RenderCustomNodeElementFn => {
    return (rd3tNode: CustomNodeElementProps) => {
        const nodeDatum = rd3tNode.nodeDatum as unknown as CustomNodeDatum;
        const attrs = nodeDatum.attributes || {};
        const id = attrs.id || "";
        const avatar = attrs.avatar || avt;
        const life = attrs.life || "—";
        const name = nodeDatum.name || '';
        const cid = uniqueClipId(id);

        return (
            <g
                onClick={() => (onClick && id ? onClick(id) : rd3tNode.toggleNode?.())}
                style={{ cursor: "pointer" }}
            >
                <defs>
                    <clipPath id={cid} clipPathUnits="userSpaceOnUse">
                        <rect x={-40} y={-64} width={80} height={80} rx={6} ry={6} />
                    </clipPath>
                </defs>

                <rect
                    x={-40}
                    y={-64}
                    width={80}
                    height={80}
                    rx={6}
                    ry={6}
                    fill="#fff"
                    stroke={isSelected ? "#4f46e5" : "#374151"}
                    strokeWidth={isSelected ? 2.5 : 1.4}
                    className={isSelected ? "shadow-lg" : ""}
                    style={{
                        filter: isSelected ? 'drop-shadow(0 0 8px rgba(99, 102, 241, 0.5))' : 'none'
                    }}
                />

                <image
                    href={avatar}
                    x={-40}
                    y={-64}
                    width={80}
                    height={80}
                    clipPath={`url(#${cid})`}
                    preserveAspectRatio="xMidYMid slice"
                />

                <rect
                    x={-40}
                    y={18}
                    width={80}
                    height={32}
                    rx={4}
                    ry={4}
                    fill={isSelected ? "#eef2ff" : "#f3f4f6"}
                    stroke={isSelected ? "#4f46e5" : "#374151"}
                    strokeWidth={isSelected ? 2 : 1.4}
                />

                <text
                    x={0}
                    y={34}
                    textAnchor="middle"
                    fontSize={10}
                    fill="#111827"
                    style={{ fontWeight: 300 }}
                >
                    {name}
                </text>

                <text
                    x={0}
                    y={47}
                    textAnchor="middle"
                    fontSize={9}
                    fill="#4b5563"
                    style={{ fontWeight: 200 }}
                >
                    {life}
                </text>
            </g>
        );
    };
};