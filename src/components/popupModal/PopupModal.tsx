import { FC, ReactNode } from "react";
import { Modal, Button, ButtonProps } from "react-bootstrap";

export type PopupModalVariant = "default" | "danger";

export interface PopupModalProps {
    show: boolean;
    onClose: () => void;
    onConfirm?: () => void;
    title?: string | ReactNode;
    body: string | ReactNode;
    confirmText?: string;
    cancelText?: string;
    variant?: PopupModalVariant;
    loading?: boolean;
    disableCloseWhileLoading?: boolean;
    confirmButtonProps?: Omit<ButtonProps, "onClick" | "children" | "variant">;
}

const PopupModal: FC<PopupModalProps> = ({
                                             show,
                                             onClose,
                                             onConfirm,
                                             title,
                                             body,
                                             confirmText = "OK",
                                             cancelText = "Cancel",
                                             variant = "default",
                                             loading = false,
                                             disableCloseWhileLoading = true,
                                             confirmButtonProps,
                                         }) => {
    const confirmVariant: ButtonProps["variant"] =
        variant === "danger" ? "danger" : "primary";

    const canClose = !(disableCloseWhileLoading && loading);

    return (
        <Modal
            show={show}
            onHide={canClose ? onClose : undefined}
            centered
            backdrop={canClose ? true : "static"}
            keyboard={canClose}
        >
            {title && (
                <Modal.Header closeButton={canClose}>
                    <Modal.Title>{title}</Modal.Title>
                </Modal.Header>
            )}

            <Modal.Body>{body}</Modal.Body>

            <Modal.Footer>
                <Button variant="secondary" onClick={onClose} disabled={!canClose}>
                    {cancelText}
                </Button>
                {onConfirm && (
                    <Button
                        variant={confirmVariant}
                        onClick={onConfirm}
                        disabled={loading}
                        {...confirmButtonProps}
                    >
                        {loading ? "Đang xử lý…" : confirmText}
                    </Button>
                )}
            </Modal.Footer>
        </Modal>
    );
};

export default PopupModal;
