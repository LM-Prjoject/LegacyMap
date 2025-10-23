import React, { lazy, memo, Suspense, useMemo } from 'react';
import styles from './DragonsBackground.module.css';

const LottiePlayer = lazy(() =>
    import('@lottiefiles/react-lottie-player').then((m) => ({ default: m.Player }))
);

type SideConfig = {
    enabled?: boolean;
    flipX?: boolean;
    delayMs?: number;
};

export type DragonsBackgroundProps = {
    src?: string;
    size?: number;
    showGrid?: boolean;
    gridOpacity?: number;
    gridSize?: number;
    gridDotColor?: string;
    className?: string;
    left?: SideConfig;
    right?: SideConfig;
};

const DEFAULT_SRC = '/lottie/Chinese_Dragon_Cartoon_Character2.json';

const DragonsBackground = memo(function DragonsBackground({
                                                              src = DEFAULT_SRC,
                                                              size = 380,
                                                              showGrid = true,
                                                              gridOpacity = 0.06,
                                                              gridSize = 40,
                                                              gridDotColor = '#d4af37',
                                                              className = '',
                                                              left = { enabled: true, flipX: true, delayMs: 0 },
                                                              right = { enabled: true, flipX: false, delayMs: 200 },
                                                          }: DragonsBackgroundProps) {
    const leftStyle = useMemo<React.CSSProperties>(
        () => ({ width: size, height: size, transform: left.flipX ? 'scaleX(-1)' : undefined, pointerEvents: 'none' }),
        [size, left.flipX]
    );
    const rightStyle = useMemo<React.CSSProperties>(
        () => ({ width: size, height: size, pointerEvents: 'none' }),
        [size]
    );
    const delayStyleLeft  = useMemo<React.CSSProperties>(() => (left.delayMs ? { animationDelay: `${left.delayMs}ms` } : {}), [left.delayMs]);
    const delayStyleRight = useMemo<React.CSSProperties>(() => (right.delayMs ? { animationDelay: `${right.delayMs}ms` } : {}), [right.delayMs]);

    return (
        <div className={`pointer-events-none absolute inset-0 ${className}`}>
            {left.enabled && (
                <div className="hidden sm:block absolute left-2 md:left-6 top-1/2 -translate-y-1/2">
                    <div className={styles.dragonMoveInLeft} style={delayStyleLeft}>
                        <Suspense fallback={null}>
                            <LottiePlayer autoplay loop src={src} style={leftStyle} />
                        </Suspense>
                    </div>
                </div>
            )}

            {right.enabled && (
                <div className="hidden sm:block absolute right-2 md:right-6 top-1/2 -translate-y-1/2">
                    <div className={styles.dragonMoveInRight} style={delayStyleRight}>
                        <Suspense fallback={null}>
                            <LottiePlayer autoplay loop src={src} style={rightStyle} />
                        </Suspense>
                    </div>
                </div>
            )}

            {showGrid && (
                <svg className="absolute inset-0 w-full h-full pointer-events-none" style={{ opacity: gridOpacity }}>
                    <defs>
                        <pattern id="dragons-grid" width={gridSize} height={gridSize} patternUnits="userSpaceOnUse">
                            <circle cx={gridSize / 2} cy={gridSize / 2} r="1" fill={gridDotColor} />
                        </pattern>
                    </defs>
                    <rect width="100%" height="100%" fill="url(#dragons-grid)" />
                </svg>
            )}
        </div>
    );
});

export default DragonsBackground;
