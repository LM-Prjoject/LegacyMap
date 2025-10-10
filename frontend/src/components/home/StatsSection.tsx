import React from 'react';

const StatsSection: React.FC = () => {
    const stats = [
        { number: '1,000+', label: 'Gia phả' },
        { number: '10,000+', label: 'Người dùng' },
        { number: '50,000+', label: 'Thành viên' }
    ];

    return (
        <section className="py-16 bg-background">
            <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
                <div className="grid grid-cols-1 md:grid-cols-3 gap-8 text-center">
                    {stats.map((stat, index) => (
                        <div key={index} className="p-6">
                            <div className="text-4xl font-bold text-primary mb-2">
                                {stat.number}
                            </div>
                            <div className="text-xl text-foreground">
                                {stat.label}
                            </div>
                        </div>
                    ))}
                </div>
            </div>
        </section>
    );
};

export default StatsSection;