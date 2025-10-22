import React, { createContext, useContext, useEffect, useMemo, useState } from 'react';

type ThemeName = 'imperial' | 'plain';

type ThemeVars = {
    '--paper': string;
    '--paper-2': string;
    '--primary': string;
    '--primary-600': string;
    '--primary-700': string;
    '--accent': string;
    '--gold': string;
    '--success': string;
    '--success-700': string;
};

const THEMES: Record<ThemeName, ThemeVars> = {
    imperial: {
        '--paper':        '#f7f0df',
        '--paper-2':      '#fff8ea',
        '--primary':      '#8b1a1a',   // đỏ tía cung đình
        '--primary-600':  '#6d0f0f',
        '--primary-700':  '#7f1212',
        '--accent':       '#a01515',
        '--gold':         '#d4af37',
        '--success':      '#2f6b3a',
        '--success-700':  '#24532c',
    },
    plain: {
        '--paper':        '#fafafa',
        '--paper-2':      '#ffffff',
        '--primary':      '#334155',
        '--primary-600':  '#1f2937',
        '--primary-700':  '#0f172a',
        '--accent':       '#475569',
        '--gold':         '#a3a3a3',
        '--success':      '#16a34a',
        '--success-700':  '#15803d',
    },
};

function applyTheme(vars: ThemeVars) {
    const root = document.documentElement;
    Object.entries(vars).forEach(([k, v]) => root.style.setProperty(k, v));
}

type ThemeContextValue = {
    theme: ThemeName;
    setTheme: (t: ThemeName) => void;
    vars: ThemeVars;
};

const ThemeContext = createContext<ThemeContextValue | null>(null);

export const ThemeProvider: React.FC<{
    theme?: ThemeName;
    children: React.ReactNode;
}> = ({ theme = 'imperial', children }) => {
    const [current, setCurrent] = useState<ThemeName>(theme);
    const vars = useMemo(() => THEMES[current], [current]);

    useEffect(() => {
        applyTheme(vars);
        document.documentElement.setAttribute('data-theme', current);
    }, [vars, current]);

    return (
        <ThemeContext.Provider value={{ theme: current, setTheme: setCurrent, vars }}>
            {children}
        </ThemeContext.Provider>
    );
};

export function useTheme() {
    const ctx = useContext(ThemeContext);
    if (!ctx) throw new Error('useTheme must be used within ThemeProvider');
    return ctx;
}
