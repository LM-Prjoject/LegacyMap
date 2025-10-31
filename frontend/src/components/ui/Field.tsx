import React from 'react';

interface FieldProps {
    label: string;
    icon?: React.ReactNode;
    children: React.ReactNode;
}

export const Field: React.FC<FieldProps> = ({ label, icon, children }) => (
    <div>
        <label className="text-xs font-medium text-[#D1B066] mb-1 block">{label}</label>
        <div className="relative rounded-xl h-12 px-3 border bg-[#2e3a57]/40 border-[#D1B066]/30
                    focus-within:border-[#D1B066] focus-within:ring-2
                    focus-within:ring-[#D1B066]/20 flex items-center gap-2 transition-all">
            {icon}
            {children}
        </div>
    </div>
);