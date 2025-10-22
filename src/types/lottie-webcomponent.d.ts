import React from 'react'

declare global {
    namespace JSX {
        interface IntrinsicElements {
            'dotlottie-player': React.DetailedHTMLProps<
                React.HTMLAttributes<HTMLElement>,
                HTMLElement
            > & {
                src?: string
                autoplay?: boolean
                loop?: boolean
                speed?: number
                background?: string
                controls?: boolean
                mode?: string
                play?: boolean
                pause?: boolean
                stop?: boolean
                style?: React.CSSProperties
                className?: string
            }
        }
    }
}
export {}
