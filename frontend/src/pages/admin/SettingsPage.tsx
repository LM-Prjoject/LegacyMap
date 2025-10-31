import React, { useState } from 'react';

const SettingsPage: React.FC = () => {
    const [activeTab, setActiveTab] = useState<'general' | 'appearance' | 'notifications'>('general');

    return (
        <div className="text-[#f4e9c8]">
            {/* Header */}
            <div className="mb-8 flex justify-between items-center">
                <div>
                    <h1 className="text-4xl font-bold mb-2 flex items-center">
                        <span className="mr-3 text-4xl">⚙️</span> Cài Đặt Hệ Thống
                    </h1>
                    <p className="text-[#f4e9c8]/70">
                        Quản lý cấu hình chung, giao diện và thông báo của khu vực quản trị.
                    </p>
                </div>
                <div className="flex gap-3">
                    <button className="px-5 py-2 bg-gradient-to-br from-[#d1b98a] to-[#f4e9c8] text-[#20283d] rounded-lg font-semibold shadow-md hover:scale-105 transition-all">
                        💾 Lưu Thay Đổi
                    </button>
                </div>
            </div>

            {/* Tabs */}
            <div className="flex gap-3 mb-6">
                {[
                    { key: 'general', label: '⚙️ Chung' },
                    { key: 'appearance', label: '🎨 Giao diện' },
                    { key: 'notifications', label: '🔔 Thông báo' },
                ].map((tab) => (
                    <button
                        key={tab.key}
                        onClick={() => setActiveTab(tab.key as any)}
                        className={`px-5 py-2 rounded-lg font-medium border transition-all ${
                            activeTab === tab.key
                                ? 'bg-gradient-to-br from-[#d1b98a] to-[#f4e9c8] text-[#20283d] border-[#f4e9c8]'
                                : 'bg-[#2e3a57]/50 text-[#f4e9c8] border-[#d1b98a]/30 hover:bg-[#2e3a57]/70'
                        }`}
                    >
                        {tab.label}
                    </button>
                ))}
            </div>

            {/* Content */}
            <div className="bg-gradient-to-br from-[#1b2233] to-[#2e3a57] rounded-xl border border-[#2e3a57]/80 p-6 shadow-lg">
                {activeTab === 'general' && (
                    <GeneralSettings />
                )}
                {activeTab === 'appearance' && (
                    <AppearanceSettings />
                )}
                {activeTab === 'notifications' && (
                    <NotificationSettings />
                )}
            </div>
        </div>
    );
};

/* ====== Sub Components ====== */

const GeneralSettings: React.FC = () => (
    <div className="space-y-5">
        <h2 className="text-2xl font-semibold mb-2">Cài đặt chung</h2>
        <SettingItem label="Tên hệ thống" placeholder="LegacyMap Admin" />
        <SettingItem label="Ngôn ngữ mặc định" placeholder="Tiếng Việt (vi-VN)" />
        <SettingItem label="Múi giờ" placeholder="Asia/Ho_Chi_Minh (GMT+7)" />
    </div>
);

const AppearanceSettings: React.FC = () => (
    <div className="space-y-5">
        <h2 className="text-2xl font-semibold mb-2">Tùy chỉnh giao diện</h2>
        <SettingItem label="Chủ đề" placeholder="Tối / Sáng" />
        <SettingItem label="Màu chủ đạo" placeholder="#d1b98a (vàng kim)" />
        <SettingItem label="Logo hiển thị" placeholder="uploads/logo.png" />
    </div>
);

const NotificationSettings: React.FC = () => (
    <div className="space-y-5">
        <h2 className="text-2xl font-semibold mb-2">Cài đặt thông báo</h2>
        <SettingItem label="Email thông báo hệ thống" placeholder="admin@legacymap.vn" />
        <SettingItem label="Bật thông báo hoạt động người dùng" placeholder="✅ Có" />
        <SettingItem label="Tần suất thông báo" placeholder="Mỗi 24 giờ" />
    </div>
);

const SettingItem: React.FC<{ label: string; placeholder: string }> = ({ label, placeholder }) => (
    <div className="flex flex-col md:flex-row md:items-center justify-between border-b border-[#2e3a57]/60 pb-3">
        <label className="font-medium text-[#f4e9c8]/90 mb-2 md:mb-0">{label}</label>
        <input
            type="text"
            placeholder={placeholder}
            className="bg-[#2e3a57]/70 border border-[#d1b98a]/30 rounded-lg px-4 py-2 text-[#f4e9c8] placeholder-[#f4e9c8]/40 focus:ring-2 focus:ring-[#d1b98a] focus:border-[#d1b98a] w-full md:w-2/3 transition-all"
        />
    </div>
);

export default SettingsPage;
