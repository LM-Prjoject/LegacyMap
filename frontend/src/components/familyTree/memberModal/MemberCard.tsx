import type { RenderCustomNodeElementFn } from "react-d3-tree";
import avt from "@/assets/avt.jpg";

const uniqueClipId = (base?: string) =>
    `avatarClip-${(base && String(base)) || Math.random().toString(36).slice(2)}`;

export const MemberCard =
    (onClick?: (id: string) => void): RenderCustomNodeElementFn =>
        ({ nodeDatum, toggleNode }) => {
            const id = String(nodeDatum.attributes?.id || "");
            const avatar = String(nodeDatum.attributes?.avatar || avt);

            const life = nodeDatum.attributes?.life || "—";
            const cid = uniqueClipId(id);

            return (
                <g
                    onClick={() => (onClick && id ? onClick(id) : toggleNode?.())}
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
                        stroke="#374151"
                        strokeWidth={1.4}
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
                        fill="#f3f4f6"
                        stroke="#374151"
                        strokeWidth={1.4}
                    />

                    <text x={0} y={34} textAnchor="middle" fontSize={10} fontWeight={600} fill="#111827">
                        {nodeDatum.name}
                    </text>

                    <text x={0} y={47} textAnchor="middle" fontSize={9} fill="#4b5563">
                        {life}
                    </text>
                </g>
            );
        };