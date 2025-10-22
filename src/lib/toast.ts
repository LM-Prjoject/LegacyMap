import toast from 'react-hot-toast'

export const showToast = {
    success: (msg: string) =>
        toast.success(msg, {
            style: { borderRadius: '10px', background: '#ecfdf5', color: '#065f46' },
        }),

    error: (msg: string) =>
        toast.error(msg, {
            style: { borderRadius: '10px', background: '#fef2f2', color: '#991b1b' },
        }),

    info: (msg: string) =>
        toast(msg, {
            icon: 'ℹ️',
            style: { borderRadius: '10px', background: '#eff6ff', color: '#1e3a8a' },
        }),

    warning: (msg: string) =>
        toast(msg, {
            icon: '⚠️',
            style: { borderRadius: '10px', background: '#fff7ed', color: '#92400e' },
        }),
}
