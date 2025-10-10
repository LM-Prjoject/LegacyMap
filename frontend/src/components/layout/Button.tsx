import React from 'react';

interface ButtonProps {
    children: React.ReactNode;
    variant?: 'primary' | 'secondary' | 'outline';
    size?: 'sm' | 'md' | 'lg';
    onClick?: () => void;
    className?: string;
    type?: 'button' | 'submit' | 'reset';
}

const Button: React.FC<ButtonProps> = ({
                                           children,
                                           variant = 'primary',
                                           size = 'md',
                                           onClick,
                                           className = '',
                                           type = 'button'
                                       }) => {
    const baseClasses = 'font-semibold rounded-lg transition-colors duration-200 focus:outline-none focus:ring-2 focus:ring-offset-2';

    const variantClasses = {
        primary: 'bg-blue-600 hover:bg-blue-700 text-white focus:ring-blue-500',
        secondary: 'bg-green-600 hover:bg-green-700 text-white focus:ring-green-500',
        outline: 'border border-blue-600 text-blue-600 hover:bg-blue-50 focus:ring-blue-500'
    };

    const sizeClasses = {
        sm: 'px-3 py-1.5 text-sm',
        md: 'px-6 py-2.5 text-base',
        lg: 'px-8 py-3 text-lg'
    };

    return (
        <button
            type={type}
            className={`${baseClasses} ${variantClasses[variant]} ${sizeClasses[size]} ${className}`}
            onClick={onClick}
        >
            {children}
        </button>
    );
};

export default Button;