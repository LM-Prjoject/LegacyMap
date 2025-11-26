import { http } from '@/api/http'

export const usersApi = {
  async checkEmail(email: string) {
    const { data } = await http.get('/users/check-email', {
      params: { email }
    })
    // Server may use data/result/payload
    const payload = (data?.data ?? data?.result ?? data?.payload) as
      | { exists: boolean; userId?: string; username?: string }
      | undefined
    return payload ?? { exists: false }
  }
}
