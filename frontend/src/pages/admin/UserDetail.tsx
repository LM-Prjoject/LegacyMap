import React, { useState, useEffect } from 'react';
import { useParams, useNavigate } from 'react-router-dom';
import { User } from '@/types/ts_user';
import { useUsers } from '@/hooks/useUsers';
import { adminApi, UserDetail as AdminUserDetail } from '@/api/ts_admin';
import PopupModal from "@/components/popupModal/PopupModal";

const UserDetail: React.FC = () => {
    const { userId } = useParams<{ userId: string }>();
    const navigate = useNavigate();
    const { users, banUser, unbanUser, loading } = useUsers();
    const [user, setUser] = useState<User | null>(null);
    const [userDetail, setUserDetail] = useState<AdminUserDetail | null>(null);
    const [showBanModal, setShowBanModal] = useState(false);
    const [actionLoading, setActionLoading] = useState(false);

    useEffect(() => {
        if (users.length > 0 && userId) {
            const foundUser = users.find((u: User) => u.id === userId);
            if (foundUser) {
                setUser(foundUser);
                loadUserDetail(userId);
            } else {
                navigate('/admin/users');
            }
        }
    }, [users, userId, navigate]);

    const loadUserDetail = async (userId: string) => {
        try {
            const detail = await adminApi.getUserDetail(userId);
            setUserDetail(detail);
        } catch (error) {
            console.error('Error loading user detail:', error);
        }
    };

    const handleBanToggle = async () => {
        if (!user) return;
        setActionLoading(true);
        try {
            if (user.isBanned) {
                await unbanUser(user.id);
            } else {
                await banUser(user.id);
            }
            setShowBanModal(false);
        } catch (error) {
            console.error('Error toggling ban status:', error);
        } finally {
            setActionLoading(false);
        }
    };

    const getInitials = (user: User): string => {
        const first = user.firstName?.charAt(0) || '';
        const last = user.lastName?.charAt(0) || '';
        return (first + last || user.email?.charAt(0) || 'U').toUpperCase();
    };

    const getDisplayName = (user: User): string =>
        user.firstName && user.lastName
            ? `${user.firstName} ${user.lastName}`
            : user.firstName || user.lastName || user.username || user.email || 'Unknown User';

    const getRoleDisplay = (user: User): string => user.role || user.roleName || 'USER';

    if (loading || !user) {
        return (
            <div className="flex justify-center items-center h-64">
                <div className="w-16 h-16 border-4 border-[#d1b98a] border-t-transparent rounded-full animate-spin"></div>
            </div>
        );
    }

    return (
        <div className="text-[#f4e9c8]">
            {/* Header */}
            <div className="mb-8">
                <button
                    onClick={() => navigate('/admin/users')}
                    className="flex items-center text-[#f4e9c8]/70 hover:text-[#f4e9c8] mb-4 transition-all"
                >
                    <svg className="w-5 h-5 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M15 19l-7-7 7-7" />
                    </svg>
                    Quay lại danh sách
                </button>

                <div className="flex items-center justify-between bg-gradient-to-br from-[#1b2233] to-[#2e3a57] p-6 rounded-xl border border-[#2e3a57]/80 shadow-lg">
                    <div className="flex items-center">
                        <div className="w-16 h-16 bg-gradient-to-br from-[#d1b98a] to-[#f4e9c8] rounded-full flex items-center justify-center text-[#20283d] text-2xl font-bold mr-4 shadow-md">
                            {getInitials(user)}
                        </div>
                        <div>
                            <h2 className="text-3xl font-bold">{getDisplayName(user)}</h2>
                            <p className="text-[#f4e9c8]/70">{user.email}</p>
                        </div>
                    </div>
                    <div className="flex items-center space-x-3">
                        <span
                            className={`px-4 py-2 rounded-lg text-sm font-medium ${user.isBanned
                                    ? 'bg-red-700/30 text-red-300 border border-red-600/40'
                                    : 'bg-green-700/30 text-green-300 border border-green-500/40'
                                }`}
                        >
                            {user.isBanned ? '🚫 Đã khóa' : '✅ Hoạt động'}
                        </span>
                        <button
                            onClick={() => setShowBanModal(true)}
                            className={`px-5 py-2 rounded-lg font-semibold transition-all shadow-md shadow-black/20 ${user.isBanned
                                    ? 'bg-gradient-to-br from-[#d1b98a] to-[#f4e9c8] text-[#20283d] hover:scale-105'
                                    : 'bg-gradient-to-br from-[#7f1d1d] to-[#b91c1c] text-[#f4e9c8] hover:scale-105'
                                }`}
                        >
                            {user.isBanned ? 'Mở khóa' : 'Khóa tài khoản'}
                        </button>
                    </div>
                </div>
            </div>

            {/* Main Content Grid */}
            <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
                <div className="lg:col-span-2 space-y-6">
                    <InfoSection title="Thông tin cơ bản">
                        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                            <InfoField label="Họ" value={user.firstName || 'Chưa cung cấp'} />
                            <InfoField label="Tên" value={user.lastName || 'Chưa cung cấp'} />
                            <InfoField label="Email" value={user.email} />
                            <InfoField label="Tên đăng nhập" value={user.username || 'Chưa cung cấp'} />
                            <InfoField label="Vai trò" value={getRoleDisplay(user)} />
                            <InfoField
                                label="Ngày tạo tài khoản"
                                value={user.createdAt ? new Date(user.createdAt).toLocaleDateString('vi-VN') : 'Không rõ'}
                            />
                            <InfoField
                                label="Cập nhật lần cuối"
                                value={user.updatedAt ? new Date(user.updatedAt).toLocaleDateString('vi-VN') : 'Không rõ'}
                            />
                            <InfoField
                                label="Đăng nhập lần cuối"
                                value={user.lastLogin ? new Date(user.lastLogin).toLocaleDateString('vi-VN') : 'Chưa đăng nhập'}
                            />
                        </div>
                    </InfoSection>

                    <InfoSection title="Trạng thái tài khoản">
                        <StatusItem label="Trạng thái" value={user.isBanned ? 'Đã khóa' : 'Hoạt động'} status={user.isBanned ? 'danger' : 'success'} />
                        <StatusItem label="Vai trò" value={getRoleDisplay(user)} status="warning" />
                        <StatusItem label="Email xác thực" value={user.isVerified ? 'Đã xác thực' : 'Chưa xác thực'} status={user.isVerified ? 'success' : 'danger'} />
                        <StatusItem label="Tài khoản kích hoạt" value={user.isActive ? 'Có' : 'Không'} status={user.isActive ? 'success' : 'danger'} />
                    </InfoSection>

                    <InfoSection title="Hoạt động gần đây">
                        <ActivityItem action="Tạo tài khoản" date={user.createdAt} icon="🎉" />
                        {user.updatedAt && <ActivityItem action="Cập nhật hồ sơ" date={user.updatedAt} icon="✏️" />}
                        {user.isBanned && user.bannedAt && <ActivityItem action="Tài khoản bị khóa" date={user.bannedAt} icon="🚫" />}
                        {user.lastLogin && <ActivityItem action="Đăng nhập lần cuối" date={user.lastLogin} icon="🔓" />}
                    </InfoSection>
                </div>

                {/* Sidebar */}
                <div className="space-y-6">
                    {/* Phần "Hành động nhanh" đã được bỏ */}

                    <SidebarSection title="Thống kê người dùng">
                        <StatItem
                            label="Cây gia phả"
                            value={userDetail?.statistics?.familyTreeCount?.toString() || '0'}
                        />
                        <StatItem
                            label="Đăng nhập lần cuối"
                            value={userDetail?.statistics?.lastLoginText || 'Chưa bao giờ'}
                        />
                        <StatItem
                            label="Thời gian sử dụng"
                            value={userDetail?.statistics?.usageDays ? `${userDetail.statistics.usageDays} ngày` : 'Không rõ'}
                        />
                    </SidebarSection>
                </div>
            </div>

            <PopupModal
                show={showBanModal}
                onClose={() => !actionLoading && setShowBanModal(false)}
                onConfirm={handleBanToggle}
                title={user.isBanned ? "Mở khóa tài khoản" : "Khóa tài khoản"}
                body={
                    user.isBanned
                        ? `Bạn có chắc chắn muốn mở khóa tài khoản của ${getDisplayName(user)}?`
                        : `Bạn có chắc chắn muốn khóa tài khoản của ${getDisplayName(user)}?`
                }
                confirmText={user.isBanned ? "Mở khóa" : "Khóa"}
                cancelText="Hủy"
                variant={user.isBanned ? "default" : "danger"}
                loading={actionLoading}
                disableCloseWhileLoading={true}
            />
        </div>
    );
};

/* === Helper Components === */
const InfoSection: React.FC<{ title: string; children: React.ReactNode }> = ({ title, children }) => (
    <div className="bg-gradient-to-br from-[#1b2233] to-[#2e3a57] border border-[#2e3a57]/80 rounded-xl p-6 shadow-lg">
        <h3 className="text-xl font-semibold mb-4">{title}</h3>
        {children}
    </div>
);

const InfoField: React.FC<{ label: string; value: string }> = ({ label, value }) => (
    <div>
        <label className="text-sm text-[#f4e9c8]/60">{label}</label>
        <p className="text-[#f4e9c8] mt-1">{value}</p>
    </div>
);

const StatusItem: React.FC<{ label: string; value: string; status: 'success' | 'danger' | 'warning' | 'info' }> = ({
    label,
    value,
    status,
}) => {
    const colorMap = {
        success: 'bg-green-700/30 text-green-300 border border-green-600/30',
        danger: 'bg-red-700/30 text-red-300 border border-red-600/30',
        warning: 'bg-yellow-700/30 text-yellow-300 border border-yellow-600/30',
        info: 'bg-blue-700/30 text-blue-300 border border-blue-600/30',
    };
    return (
        <div className="flex justify-between items-center p-3 bg-[#2e3a57]/60 rounded-lg">
            <span className="font-medium">{label}</span>
            <span className={`px-3 py-1 rounded-lg text-sm font-semibold ${colorMap[status]}`}>
                {value}
            </span>
        </div>
    );
};

const ActivityItem: React.FC<{ action: string; date?: string; icon: string }> = ({ action, date, icon }) => (
    <div className="flex items-start p-3 bg-[#2e3a57]/40 hover:bg-[#2e3a57]/60 rounded-lg transition-all">
        <div className="text-2xl mr-3">{icon}</div>
        <div>
            <p className="font-medium">{action}</p>
            <p className="text-sm text-[#f4e9c8]/70">
                {date
                    ? new Date(date).toLocaleDateString('vi-VN', {
                        year: 'numeric',
                        month: 'long',
                        day: 'numeric',
                        hour: '2-digit',
                        minute: '2-digit',
                    })
                    : 'Không rõ ngày'}
            </p>
        </div>
    </div>
);


const SidebarSection: React.FC<{ title: string; children: React.ReactNode }> = ({ title, children }) => (
    <div className="bg-gradient-to-br from-[#1b2233] to-[#2e3a57] border border-[#2e3a57]/80 rounded-xl p-6 shadow-lg">
        <h3 className="text-lg font-semibold mb-4">{title}</h3>
        {children}
    </div>
);

const StatItem: React.FC<{ label: string; value: string }> = ({ label, value }) => (
    <div className="flex justify-between items-center text-sm">
        <span className="text-[#f4e9c8]/70">{label}</span>
        <span className="font-semibold">{value}</span>
    </div>
);

export default UserDetail;
