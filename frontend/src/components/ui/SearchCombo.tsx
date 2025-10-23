import { useState, useMemo, Fragment } from 'react';
import { Combobox, Transition } from '@headlessui/react';
import { Search, ChevronDown } from 'lucide-react';

export type Option = { value: number | string; label: string };

interface SearchComboProps {
    value: number | string | null | undefined;
    onChange: (v: number | string | null) => void;
    options: Option[];
    placeholder?: string;
    disabled?: boolean;
    emptyText?: string;
    bare?: boolean;
}

export const SearchCombo: React.FC<SearchComboProps> = ({
                                                            value,
                                                            onChange,
                                                            options,
                                                            placeholder = 'Tìm...',
                                                            disabled = false,
                                                            emptyText = 'Không có kết quả',
                                                            bare = false,
                                                        }) => {
    const [query, setQuery] = useState('');
    const filtered = useMemo(() => {
        if (!query.trim()) return options;
        const q = query.toLowerCase();
        return options.filter((o) => o.label.toLowerCase().includes(q));
    }, [options, query]);

    const selected = useMemo(() => options.find((o) => o.value === value) || null, [options, value]);

    return (
        <Combobox value={selected} onChange={(opt: Option | null) => onChange(opt ? opt.value : null)} disabled={disabled}>
            <div className={`relative w-full ${disabled ? 'opacity-60' : ''}`}>
                <div
                    className={
                        bare
                            ? 'flex items-center gap-2 h-12'
                            : 'flex items-center gap-2 rounded-xl h-12 px-3 border bg-white border-slate-300 focus-within:border-[#1E3A8A] focus-within:ring-2 focus-within:ring-[#1E3A8A]/20'
                    }
                >
                    {!bare && <Search className="w-4 h-4 text-slate-400" />}
                    <Combobox.Input
                        displayValue={(opt: Option) => (opt ? opt.label : '')}
                        onChange={(e) => setQuery(e.target.value)}
                        placeholder={placeholder}
                        className="w-full bg-transparent outline-none"
                    />
                    <Combobox.Button className="p-1">
                        <ChevronDown className="w-4 h-4 text-slate-400" />
                    </Combobox.Button>
                </div>

                <Transition as={Fragment} leave="transition ease-in duration-100" leaveFrom="opacity-100" leaveTo="opacity-0" afterLeave={() => setQuery('')}>
                    <Combobox.Options className="absolute z-[200] mt-1 max-h-60 w-full overflow-auto rounded-xl border border-slate-200 bg-white shadow-lg focus:outline-none">
                        {filtered.length === 0 ? (
                            <div className="px-3 py-2 text-sm text-slate-500">{emptyText}</div>
                        ) : (
                            filtered.map((opt) => (
                                <Combobox.Option
                                    key={opt.value}
                                    value={opt}
                                    className={({ active }) =>
                                        `cursor-pointer px-3 py-2 text-sm ${active ? 'bg-indigo-50 text-indigo-700' : 'text-slate-800'}`
                                    }
                                >
                                    {opt.label}
                                </Combobox.Option>
                            ))
                        )}
                    </Combobox.Options>
                </Transition>
            </div>
        </Combobox>
    );
};
