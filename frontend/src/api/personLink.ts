import { http } from '@/api/http'

export const personLinkApi = {
    async invite(personId: string, inviterId: string, email: string) {
        const payload = { email } as { email: string; linkType?: string }
        const { data } = await http.post(`/persons/${personId}/invite`, payload, {
            params: { userId: inviterId }
        })
        return data
    },
    async acceptClaim(personId: string, currentUserId: string) {
        const { data } = await http.post(`/persons/${personId}/claims/accept`, null, {
            params: { userId: currentUserId }
        })
        return data
    },
    async rejectClaim(personId: string, currentUserId: string) {
        const { data } = await http.post(`/persons/${personId}/claims/reject`, null, {
            params: { userId: currentUserId }
        })
        return data
    },
    async getMyClaims(currentUserId: string) {
        const { data } = await http.get(`/persons/me/claims`, {
            params: { userId: currentUserId }
        })
        return data
    },
    async checkVerified(personId: string) {
        const { data } = await http.get(`/persons/${personId}/links/self/verified`)
        return data
    },
    async unlinkSelf(personId: string, requesterId: string) {
        const { data } = await http.delete(`/persons/${personId}/links/self`, {
            params: { userId: requesterId }
        })
        return data
    },
}
