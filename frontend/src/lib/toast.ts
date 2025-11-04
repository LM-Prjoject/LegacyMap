import toast from 'react-hot-toast'

export const showToast = {
    success: (msg: string) => toast.success(msg),
    error:   (msg: string) => toast.error(msg),
    info: (msg: string) => toast(msg, { icon: "ℹ️" }),
    warning: (msg: string) => toast(msg, { icon: "⚠️" }),
    loading: (msg: string) => toast.loading(msg),
    dismiss: (id?: string) => toast.dismiss(id),
}
