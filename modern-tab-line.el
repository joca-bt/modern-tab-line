;;; modern-tab-line.el --- A modern looking tab line -*- lexical-binding: t -*-

;; Copyright (C) 2020 João Guerra

;; Author: João Guerra <joca.bt@gmail.com>
;; URL: https://github.com/joca-bt/modern-tab-line/
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; TODO
;;
;; This packages offers a modern looking tab line. It differs from
;; `tab-line' in the following:
;;   - All windows show the same tabs.
;;   - Scrolling scrolls the tab line instead of the tabs.
;;
;; The default customization sets the tab line height via the
;; separators.
;;
;; Note that enabling `modern-tab-line-mode' will modify face
;; `tab-line' to inherit from `modern-tab-line-tab-line'.
;; -> loading this file?
;;
;;
;; difference between (window-buffer) and (current-buffer) ?
;;

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------

(defgroup modern-tab-line nil
  "A modern looking tab line."
  :group 'convenience
  :version "27.1")

(defcustom modern-tab-line-buffers-function #'modern-tab-line-buffers
  "Function that returns the buffers to show in the tab line."
  :group 'modern-tab-line
  :type 'function
  :set (lambda (symbol value)
         (set-default symbol value)
         (force-mode-line-update)))

(defcustom modern-tab-line-tab-function #'modern-tab-line-tab
  "Function that returns the display representation for a tab."
  :group 'modern-tab-line
  :type 'function
  :set (lambda (symbol value)
         (set-default symbol value)
         (force-mode-line-update)))

(defcustom modern-tab-line-tab-name-function #'buffer-name
  "Function that returns the name for a tab."
  :group 'modern-tab-line
  :type 'function
  :set (lambda (symbol value)
         (set-default symbol value)
         (force-mode-line-update)))

(defcustom modern-tab-line-tab-help-function #'modern-tab-line-tab-help
  "Function that returns the help for a tab."
  :group 'modern-tab-line
  :type 'function
  :set (lambda (symbol value)
         (set-default symbol value)
         (force-mode-line-update)))

(defvar modern-tab-line-tab-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [tab-line mouse-1] #'modern-tab-line-select-tab)
    keymap)
  "Keymap for tabs.")

(defcustom modern-tab-line-separator (modern-tab-line-make-xpm 30 1 "grey55")
  "Separator used between tabs."
  :group 'modern-tab-line
  :type 'string
  :set (lambda (symbol value)
         (set-default symbol value)
         (force-mode-line-update)))

(defcustom modern-tab-line-excluded-modes nil
  "Major modes ignored by `global-modern-tab-line-mode'."
  :group 'modern-tab-line
  :type '(repeat symbol))

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------

(defgroup modern-tab-line-faces nil
  "The faces of `modern-tab-line'."
  :group 'modern-tab-line
  :group 'faces
  :version "27.1")

(defface modern-tab-line-tab-line '((t :inherit variable-pitch
                                       :height 0.9
                                       :foreground "black"
                                       :background "grey85"))
  "Face of the tab line."
  :group 'modern-tab-line-faces)

(defface modern-tab-line-tab-active '((t :inherit modern-tab-line-tab-line))
  "Face of active tabs."
  :group 'modern-tab-line-faces)

(defface modern-tab-line-tab-inactive '((t :inherit modern-tab-line-tab-line
                                           :background "grey75"))
  "Face of inactive tabs."
  :group 'modern-tab-line-faces)

(defface modern-tab-line-tab-highlight '((t :inherit modern-tab-line-tab-line))
  "Face for highlighting tabs."
  :group 'modern-tab-line-faces)

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------

(let ((cache nil))
  (defun modern-tab-line-buffers ()
    "Returns the current buffer and the buffers that are visiting
files.  The order of the buffers is preserved between calls."
    (let ((buffers (seq-filter (lambda (buffer)
                                 (or (eq buffer (current-buffer))
                                     (buffer-file-name buffer)))
                               (buffer-list))))
      (setq cache (append (seq-intersection cache buffers)
                          (seq-difference buffers cache))))))

(let ((space (propertize " " 'display '(space :width 1))))
  (defun modern-tab-line-tab (tab-name)
    "Returns the name of the tab padded with spaces."
    (concat space tab-name space)))

(defun modern-tab-line-tab-help (buffer)
  "Returns the name of the file the buffer is visiting, if any,
otherwise returns the name of the buffer."
  (if-let ((file-name (buffer-file-name buffer)))
      (abbreviate-file-name file-name)
    (buffer-name buffer)))

(defun modern-tab-line-select-tab (event)
  "Switch to the selected tab."
  (interactive "e")
  (let* ((posn (event-start event))
         (window (posn-window posn))
         (buffer (modern-tab-line--get-buffer (car (posn-string posn)))))
    (select-window window)
    (switch-to-buffer buffer)))

(defun modern-tab-line-close-tab (event)
  "Close the selected tab."
  (interactive "e")
  (let* ((posn (event-start event))
         (buffer (modern-tab-line--get-buffer (car (posn-string posn)))))
    (kill-buffer buffer)))

(defun modern-tab-line-switch-to-next-tab ()
  "Switch to the next tab."
  (interactive)
  (let* ((buffers (funcall modern-tab-line-buffers-function))
         (buffer (nth (1+ (seq-position buffers (current-buffer))) buffers)))
    (when buffer
      (switch-to-buffer buffer))))

(defun modern-tab-line-switch-to-previous-tab ()
  "Switch to the previous tab."
  (interactive)
  (let* ((buffers (funcall modern-tab-line-buffers-function))
         (buffer (nth (1- (seq-position buffers (current-buffer))) buffers)))
    (when buffer
      (switch-to-buffer buffer))))

;; modern-tab-line   -> A modern looking tab line.
;; buffers-function  -> Function that returns the buffers to show in the tab line.
;; tab-function      -> Function that returns the display representation for a tab.
;; tab-name-function -> Function that returns the name for a tab.
;; tab-help-function -> Function that returns the help for a tab.
;; tab-keymap        -> Keymap for tabs.
;; separator         -> Separator used between tabs.
;; excluded-modes    -> Major modes ignored by `global-modern-tab-line-mode'.
;;
;; modern-tab-line-faces -> The faces of `modern-tab-line'.
;; tab-line              -> Face of the tab line.
;; tab-active            -> Face of active tabs.
;; tab-inactive          -> Face of inactive tabs.
;; tab-highlight         -> Face for highlighting tabs.
;;
;; buffers                -> Returns the current buffer and the buffers that are visiting files. The order of the buffers is preserved between calls.
;; tab                    -> Returns the name of the tab padded with spaces.
;; tab-help               -> Returns the name of the file the buffer is visiting, if any, otherwise returns the name of the buffer.
;; select-tab             -> Switch to the selected tab.
;; close-tab              -> Close the selected tab.
;; switch-to-next-tab     -> Switch to the next tab.
;; switch-to-previous-tab -> Switch to the previous tab.

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------

(defun modern-tab-line--get-buffer (string)
  (get-text-property 0 'buffer string))

(defun modern-tab-line--get-cache ()
  (window-parameter nil 'modern-tab-line--cache))

(defun modern-tab-line--set-cache (cache)
  (set-window-parameter nil 'modern-tab-line--cache cache))

(defun modern-tab-line--get-scroll ()
  (window-parameter nil 'modern-tab-line--scroll))

(defun modern-tab-line--set-scroll (scroll)
  (set-window-parameter nil 'modern-tab-line--scroll scroll))

(defun modern-tab-line--turn-on ()
  (unless (or (minibufferp)
              (seq-contains-p modern-tab-line-excluded-modes major-mode))
    (modern-tab-line-mode 1)))




























(defun modern-tab-line-make-xpm (height width &optional color)
  "TODO"
  (let ((xpm (format "/* XPM */ static char * xpm[] = { \"%s %s 1 1\", \". c %s\", %s };"
                     width
                     height
                     (or color "none")
                     (apply #'concat (make-list height (format "\"%s\"," (make-string width ?.)))))))
    (propertize " " 'display (create-image xpm 'xpm t :ascent 'center))))







;; (pop-to-buffer buffer t)


;; (global-set-key [tab-line wheel-up] 'modern-tab-line-scroll-left)
;; (global-set-key [tab-line wheel-down] 'modern-tab-line-scroll-right)


;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Sizes.html

;; the temporary buffer has fringes
(let ((buffer (generate-new-buffer " *modern-tab-line-scroll*")))
  (defun modern-tab-line--scroll-tab-line (tab-line)
    (with-current-buffer buffer
      (let ((inhibit-modification-hooks t)
            (truncate-partial-width-windows nil))
        (setq truncate-lines nil
              word-wrap nil)
        (erase-buffer)
        (apply #'insert tab-line)
        (goto-char (point-min))
        (vertical-motion 1)
        ))))






;; (defun modern-tab-line--track-closed-buffer ()
;;   (let ((buffer (if (eq (car ft-buffers)
;;                         (current-buffer))
;;                     (cadr ft-buffers)
;;                   (cl-loop for (previous current next) on ft-buffers
;;                            when (eq current (current-buffer))
;;                            return (or next previous)))))
;;     (when buffer
;;       (switch-to-buffer buffer))))
;;
;; (add-hook 'kill-buffer-hook #'modern-tab-line--track-closed-buffer)































(defun modern-tab-line--format ()
  (cl-destructuring-bind (&optional cached-active-buffer cached-buffers cached-tab-line)
      (modern-tab-line--get-cache)
    (let ((active-buffer (current-buffer))
          (buffers (funcall modern-tab-line-buffers-function)))
      (if (and (eq active-buffer cached-active-buffer)
               (equal buffers cached-buffers))
          cached-tab-line
        (let ((tab-line (modern-tab-line--format-tab-line buffers)))
          (modern-tab-line--set-cache (list active-buffer buffers tab-line))
          tab-line)))))

;; (apply #'concat tab-line)
(defun modern-tab-line--format-tab-line (buffers)
  (let ((tab-line (mapcan (lambda (buffer)
                            (list (modern-tab-line--format-tab buffer)
                                  modern-tab-line-separator))
                          buffers)))
    (if (and (not (window-at-side-p nil 'left))
             (not window-divider-mode))
        (cons modern-tab-line-separator tab-line)
      tab-line)))

;; https://lists.gnu.org/archive/html/emacs-devel/2018-12/msg00430.html
(add-hook 'window-buffer-change-functions (lambda (_) (modern-tab-line--set-cache nil)))

(defun modern-tab-line--format-tab (buffer)
  (let ((active-p (eq buffer (current-buffer))))
    (propertize (funcall modern-tab-line-tab-function (funcall modern-tab-line-tab-name-function buffer))
                'buffer buffer
                'face (if active-p
                          'modern-tab-line-tab-active
                        'modern-tab-line-tab-inactive)
                'mouse-face 'modern-tab-line-tab-highlight
                'pointer 'arrow
                'help-echo (lambda (window object position)
                             (let ((buffer (modern-tab-line--get-buffer object)))
                               (funcall modern-tab-line-tab-help-function buffer)))
                'local-map modern-tab-line-tab-keymap)))

(defun modern-tab-line-scroll-right (scroll)
  (interactive "n")
  (modern-tab-line--set-scroll (+ (or (modern-tab-line--get-scroll)
                                      0)
                                  scroll))
  (force-mode-line-update))
















;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------

;;;###autoload
(define-minor-mode modern-tab-line-mode
  "A modern looking tab line."
  :lighter nil
  (setq tab-line-format (when modern-tab-line-mode
                          '(:eval (modern-tab-line--format)))))

;;;###autoload
(define-globalized-minor-mode global-modern-tab-line-mode
  modern-tab-line-mode
  modern-tab-line--turn-on)

(face-spec-set 'tab-line '((t :inherit modern-tab-line-tab-line)) 'face-defface-spec)

(provide 'modern-tab-line)

;;; modern-tab-line.el ends here
