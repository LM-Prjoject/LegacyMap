import React from 'react';

interface FieldProps {
    label: string;
    icon?: React.ReactNode;
    children: React.ReactNode;
}

export const Field: React.FC<FieldProps> = ({ label, icon, children }) => (
    <div>
        <label className="text-xs font-medium text-slate-500 mb-1 block">{label}</label>
        <div className="relative rounded-xl h-12 px-3 border bg-white border-slate-300
                    focus-within:border-[#1E3A8A] focus-within:ring-2
                    focus-within:ring-[#1E3A8A]/20 flex items-center gap-2">
            {icon}
            {children}
        </div>
    </div>
);
