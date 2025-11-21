let measureCanvas: HTMLCanvasElement | null = null;

export const truncateByWidth = (
    text: string,
    maxWidth: number,
    font = "12px system-ui, sans-serif"
) => {
    if (!text) return "";
    if (typeof document === "undefined") return text;

    if (!measureCanvas) {
        measureCanvas = document.createElement("canvas");
    }
    const ctx = measureCanvas.getContext("2d");
    if (!ctx) return text;

    ctx.font = font;
    if (ctx.measureText(text).width <= maxWidth) return text;

    let truncated = text;
    while (ctx.measureText(truncated + "…").width > maxWidth && truncated.length > 0) {
        truncated = truncated.slice(0, -1);
    }
    return truncated + "…";
};
