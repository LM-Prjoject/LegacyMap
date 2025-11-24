import type { RenderCustomNodeElementFn, CustomNodeElementProps } from "react-d3-tree";
import type { CustomNodeDatum } from "../TreeGraph";
import { truncateByWidth } from "@/lib/truncate";
import { useAvatarDataUrl } from "@/hooks/useAvatarDataUrl";

const uniqueClipId = (base?: string) =>
    `avatarClip-${(base && String(base)) || Math.random().toString(36).slice(2)}`;

function MemberNodeCard({
                            rd3tNode,
                            onClick,
                            isSelected,
                        }: {
    rd3tNode: CustomNodeElementProps;
    onClick?: (id: string) => void;
    isSelected: boolean;
}) {
    const nodeDatum = rd3tNode.nodeDatum as unknown as CustomNodeDatum;
    const attrs = nodeDatum.attributes || {};
    const id = (attrs.id as string) || "";
    const rawAvatar = (attrs.avatar as string) || "";
    const hasAvatar = rawAvatar && rawAvatar.trim().length > 0 ? rawAvatar : null;

    const dataUrl = useAvatarDataUrl(hasAvatar || undefined);

    const displayAvatar = dataUrl || hasAvatar;

    const life = (attrs.life as string) || "—";
    const name = nodeDatum.name || "";
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
                    filter: isSelected
                        ? "drop-shadow(0 0 8px rgba(99, 102, 241, 0.5))"
                        : "none",
                }}
            />

            {displayAvatar && (
                <image
                    href={displayAvatar}
                    x={-40}
                    y={-64}
                    width={80}
                    height={80}
                    clipPath={`url(#${cid})`}
                    preserveAspectRatio="xMidYMid slice"
                />
            )}

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
                style={{ fontWeight: 100 }}
            >
                {truncateByWidth(name, 70)}
            </text>

            <text
                x={0}
                y={47}
                textAnchor="middle"
                fontSize={9}
                fill="#4b5563"
                style={{ fontWeight: 100 }}
            >
                {life}
            </text>
        </g>
    );
}

export const MemberCard = (
    onClick?: (id: string) => void,
    isSelected: boolean = false
): RenderCustomNodeElementFn => {
    return (rd3tNode: CustomNodeElementProps) => (
        <MemberNodeCard rd3tNode={rd3tNode} onClick={onClick} isSelected={isSelected} />
    );
};