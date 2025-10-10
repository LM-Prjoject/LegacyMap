// src/components/home/Footer.tsx

export default function Footer() {
    return (
        <footer className="bg-card border-t border-border">
            <div className="max-w-7xl mx-auto px-6 py-8 flex flex-col md:flex-row items-center justify-between gap-4">
                <div className="text-muted-foreground text-sm">
                    © {new Date().getFullYear()} GPGay Gia Phả. All rights reserved.
                </div>
                <nav className="flex items-center gap-6 text-sm">
                    <a href="#about" className="text-muted-foreground hover:text-primary transition-colors">Về chúng tôi</a>
                    <a href="#features" className="text-muted-foreground hover:text-primary transition-colors">Tính năng</a>
                    <a href="#cta" className="text-muted-foreground hover:text-primary transition-colors">Liên hệ</a>
                </nav>
            </div>
        </footer>
    );
}