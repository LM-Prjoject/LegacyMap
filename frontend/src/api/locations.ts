export type Ward = { code: number; name: string };
export type Province = { code: number; name: string };

let provCache: Province[] | null = null;
const wardCache: Record<number, Ward[]> = {};

async function fetchProvinces_VNAppMob(): Promise<Province[]> {
    const res = await fetch("https://vapi.vnappmob.com/api/province/");
    const data = await res.json();
    return (data?.results ?? []).map((p: any) => ({
        code: Number(p.province_id),
        name: String(p.province_name),
    }));
}

async function fetchWardsByProvince_VNAppMob(pCode: number): Promise<Ward[]> {
    const dRes = await fetch(`https://vapi.vnappmob.com/api/province/district/${pCode}`);
    const dData = await dRes.json();
    const districts: Array<{ district_id: string }> = dData?.results ?? [];

    const wardLists: Ward[][] = await Promise.all(
        districts.map(async (d) => {
            const wRes = await fetch(`https://vapi.vnappmob.com/api/province/ward/${d.district_id}`);
            const wData = await wRes.json();
            // { results: [{ ward_id, ward_name }] }
            return (wData?.results ?? []).map((w: any) => ({
                code: Number(w.ward_id),
                name: String(w.ward_name),
            })) as Ward[];
        })
    );

    const flat = wardLists.flat();
    const seen = new Set<number>();
    const unique = flat.filter((w) => (seen.has(w.code) ? false : (seen.add(w.code), true)));
    unique.sort((a, b) => a.name.localeCompare(b.name, "vi"));
    return unique;
}

async function fetchProvinces_OpenAPI(): Promise<Province[]> {
    const res = await fetch("https://provinces.open-api.vn/api/p/");
    const data = await res.json();
    return (data ?? []).map((p: any) => ({ code: Number(p.code), name: String(p.name) }));
}

async function fetchWardsByProvince_OpenAPI(pCode: number): Promise<Ward[]> {
    const res = await fetch(`https://provinces.open-api.vn/api/p/${pCode}?depth=3`);
    const data = await res.json();
    const wards: Ward[] = (data?.districts ?? [])
        .flatMap((d: any) => d?.wards ?? [])
        .map((w: any) => ({ code: Number(w.code), name: String(w.name) }));
    wards.sort((a, b) => a.name.localeCompare(b.name, "vi"));
    return wards;
}

export async function fetchProvinces(): Promise<Province[]> {
    if (provCache) return provCache;
    try {
        const list = await fetchProvinces_VNAppMob();
        provCache = list;
        return list;
    } catch {
        const list = await fetchProvinces_OpenAPI();
        provCache = list;
        return list;
    }
}

export async function fetchWardsByProvince(pCode: number): Promise<Ward[]> {
    if (wardCache[pCode]) return wardCache[pCode];
    try {
        const ws = await fetchWardsByProvince_VNAppMob(pCode);
        wardCache[pCode] = ws;
        return ws;
    } catch {
        const ws = await fetchWardsByProvince_OpenAPI(pCode);
        wardCache[pCode] = ws;
        return ws;
    }
}