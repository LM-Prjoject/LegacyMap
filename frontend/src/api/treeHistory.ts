import http from './http';

export interface TreeHistoryItem {
    id: string;
    userName: string;
    userAvatar: string | null;
    action: 'CREATED' | 'UPDATED' | 'DELETED';
    entityType: 'MEMBER' | 'RELATIONSHIP' | 'TREE_INFO';
    entityName: string;
    description: string;
    oldValue: string | null;
    newValue: string | null;
    createdAt: string;
}

export interface TreeHistoryResponse {
    content: TreeHistoryItem[];
    totalElements: number;
    totalPages: number;
    number: number;
    size: number;
}

export const treeHistoryApi = {
    getHistory: async (treeId: string, page: number = 0, size: number = 20) => {
        // ✅ FIXED: Bỏ /api prefix vì base URL đã có
        const response = await http.get<TreeHistoryResponse>(
            `/trees/${treeId}/history?page=${page}&size=${size}&sort=createdAt,desc`
        );
        return response.data;
    }
};