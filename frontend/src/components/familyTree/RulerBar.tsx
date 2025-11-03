import { memo, useMemo } from "react";

type Props = {
    width: number;
    zoom: number;
    translateX: number;
    unitPx?: number;
    height?: number;
};

export default memo(function RulerBar({
                                          width,
                                          zoom,
                                          translateX,
                                          unitPx = 60,
                                          height = 40,
                                      }: Props) {
    const spacing = Math.max(10, unitPx * (zoom || 1));
    const offset = ((translateX % spacing) + spacing) % spacing;

    const count = Math.ceil(width / spacing) + 2;

    const ticks = useMemo(
        () => Array.from({ length: count }, (_, i) => i),
        [count]
    );

    return (
        <div
            className="relative w-full border-b border-white/30 bg-white/15 overflow-hidden select-none"
            style={{ height }}
            aria-label="ruler"
        >
            {spacing >= 40 && (
                <div className="absolute inset-0 pointer-events-none">
                    {ticks.map((i) => {
                        const x = i * spacing + offset + spacing / 2;
                        return (
                            <div
                                key={`m${i}`}
                                className="absolute top-0 w-px bg-white/40"
                                style={{ left: x, height: height * 0.35 }}
                            />
                        );
                    })}
                </div>
            )}

            <div className="absolute inset-0 pointer-events-none">
                {ticks.map((i) => {
                    const x = i * spacing + offset;
                    return (
                        <div key={i} className="absolute" style={{ left: x }}>
                            <div className="w-px bg-white/70" style={{ height: height * 0.55 }} />
                            <div className="text-[10px] text-white/80 mt-1 text-center translate-x-[-50%]">
                                {i}
                            </div>
                        </div>
                    );
                })}
            </div>
        </div>
    );
});
