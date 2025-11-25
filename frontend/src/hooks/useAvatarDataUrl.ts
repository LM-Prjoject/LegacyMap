import { useEffect, useState } from "react";

const cache = new Map<string, string>();
const inFlight = new Map<string, Promise<string>>();

async function fetchToDataUrl(src: string): Promise<string> {
    const res = await fetch(src, { mode: "cors" });
    const blob = await res.blob();

    return await new Promise<string>((resolve, reject) => {
        const reader = new FileReader();
        reader.onloadend = () => resolve(reader.result as string);
        reader.onerror = reject;
        reader.readAsDataURL(blob);
    });
}

export function useAvatarDataUrl(src?: string | null): string | null {
    const [dataUrl, setDataUrl] = useState<string | null>(null);

    useEffect(() => {
        if (!src) {
            setDataUrl(null);
            return;
        }

        if (cache.has(src)) {
            setDataUrl(cache.get(src)!);
            return;
        }

        let cancelled = false;

        const promise =
            inFlight.get(src) ??
            fetchToDataUrl(src)
                .then((url) => {
                    cache.set(src, url);
                    inFlight.delete(src);
                    return url;
                })
                .catch((err) => {
                    console.error("Failed to fetch avatar", src, err);
                    inFlight.delete(src);
                    throw err;
                });

        inFlight.set(src, promise);

        promise
            .then((url) => {
                if (!cancelled) setDataUrl(url);
            })
            .catch(() => {
                if (!cancelled) setDataUrl(null);
            });

        return () => {
            cancelled = true;
        };
    }, [src]);

    return dataUrl;
}
