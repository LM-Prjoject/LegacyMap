// // src/pages/auth/ForgotPassword.tsx
// import React from 'react';
// import { useForm } from 'react-hook-form';
// import { z } from 'zod';
// import { zodResolver } from '@hookform/resolvers/zod';
// import { requestPasswordReset } from '../../api/auth';

// const schema = z.object({
//   email: z.string().email('Email không hợp lệ'),
// });

// type FormValues = z.infer<typeof schema>;

// export default function ForgotPassword() {
//   const [submitted, setSubmitted] = React.useState(false);
//   const [loading, setLoading] = React.useState(false);
//   const [error, setError] = React.useState<string | null>(null);

//   const { register, handleSubmit, formState: { errors } } = useForm<FormValues>({
//     resolver: zodResolver(schema),
//   });

//   const onSubmit = async (values: FormValues) => {
//     setLoading(true);
//     setError(null);
//     try {
//       await requestPasswordReset({ email: values.email });
//       setSubmitted(true);
//     } catch (e: any) {
//       setError(e?.response?.data?.message || 'Có lỗi xảy ra. Vui lòng thử lại.');
//     } finally {
//       setLoading(false);
//     }
//   };

//   if (submitted) {
//     return (
//       <div className="max-w-md mx-auto p-6">
//         <h1 className="text-xl font-semibold mb-2">Kiểm tra email của bạn</h1>
//         <p className="text-sm text-gray-600">
//           Nếu email tồn tại trong hệ thống, chúng tôi đã gửi hướng dẫn đặt lại mật khẩu.
//         </p>
//       </div>
//     );
//   }

//   return (
//     <div className="max-w-md mx-auto p-6">
//       <h1 className="text-2xl font-bold mb-4">Quên mật khẩu</h1>
//       <form onSubmit={handleSubmit(onSubmit)} className="space-y-4">
//         <div>
//           <label className="block text-sm font-medium mb-1">Email</label>
//           <input
//             type="email"
//             className="w-full border rounded px-3 py-2"
//             placeholder="you@example.com"
//             {...register('email')}
//           />
//           {errors.email && <p className="text-red-600 text-sm mt-1">{errors.email.message}</p>}
//         </div>

//         {error && <div className="text-red-600 text-sm">{error}</div>}

//         <button
//           type="submit"
//           disabled={loading}
//           className="w-full bg-blue-600 text-white rounded px-4 py-2 disabled:opacity-60"
//         >
//           {loading ? 'Đang gửi...' : 'Gửi yêu cầu'}
//         </button>
//       </form>
//     </div>
//   );
// }