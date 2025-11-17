;;; gemini-cli-mode.el --- Emacs interface for Gemini CLI -*- lexical-binding: t; -*-

;; Copyright (C) 2025 David Pham
;; SPDX-License-Identifier: Apache-2.0

;; Author: David Pham <davidpham87@gmail.com>
;; Version: 0.1.0
;; Package-Version: 20251116.12345
;; Keywords: gemini-cli tools
;; URL: https://github.com/davidpham87/gemini_cli.el
;; Package-Requires: ((emacs "29.1") (markdown-mode "2.5") (vterm "0.0.2"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;; This file provides functions and a minor mode to interact with the Gemini CLI
;; from within Emacs.  It allows starting the Gemini CLI in a term buffer,
;; switching to that buffer, and sending regions of text to the Gemini process.

;;; Code:

(defvar gemini-cli-buffer nil
  "Buffer for the Gemini CLI process.")

(defun gemini-cli-rebind-cli ()
  "Rebind `gemini-cli-buffer' to the current buffer.
This command is useful if the `gemini-cli-buffer' variable is not
pointing to the correct buffer, for example, if the buffer was
killed and restarted."
  (interactive)
  (setq gemini-cli-buffer (switch-to-buffer "*gemini-cli*")))

(defun gemini-cli-log-conversation ()
  "Log the conversation with Gemini to a file.
This function creates a log file in the `~/.gemini/tmp/gemini_el/'
directory with a timestamped name.  It then uses the `script'
command to record the entire vterm session to this file.  This
is a workaround for a bug that prevents scrolling up in the
`gemini-cli' buffer."
  (let* ((log-dir (expand-file-name "'~/.gemini/tmp/gemini_el/" default-directory))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (log-file (concat log-dir timestamp "_gemini_convo.log")))
    (make-directory log-dir t)
    (vterm-send-string (format "script %s" (shell-quote-argument log-file)))
    (vterm-send-return)))

(defun gemini-cli-start (&optional ignore-logging-p)
  "Start the Gemini CLI in a vterm buffer.
This function opens a new vterm buffer named `*gemini-cli*',
splits the window horizontally, and starts the Gemini CLI.  If a
Gemini process is already running, it displays a message and
does nothing.

When called with a prefix argument IGNORE-LOGGING-P, it will
not log the conversation to a file.  Otherwise, it calls
`gemini-cli-log-conversation' to start logging."
  (interactive "P")
  (if (buffer-live-p gemini-cli-buffer)
      (message "Gemini process already running")
    (progn
      (let* ((new-window (split-window-horizontally)))
        (setq gemini-cli-buffer (vterm "*gemini-cli*"))
        (when (not ignore-logging-p)
          (gemini-cli-log-conversation))
        (set-window-buffer new-window gemini-cli-buffer)
        (vterm-send-string "gemini")
        (vterm-send-return)))))

(defun gemini-cli-switch-buffer ()
  "Switch to the Gemini CLI buffer.
If the `*gemini-cli*' buffer is live, this function switches to
it in another window.  If the buffer is not live, it calls
`gemini-cli-start' to create it."
  (interactive)
  (if (buffer-live-p gemini-cli-buffer)
      (switch-to-buffer-other-window "*gemini-cli*")
    (gemini-cli-start)))

(defun gemini-cli-send-region (start end)
  "Send the current region to the Gemini CLI process.
The text between START and END is sent to the `*gemini-cli*'
buffer without switching the current buffer.

This function is interactive, so it can be called with `M-x
gemini-cli-send-region' or bound to a key.  When called
interactively, START and END are the boundaries of the current
region."
  (interactive "r")
  (let ((current-buffer (current-buffer))
        (region-text (buffer-substring-no-properties start end)))
    (if (buffer-live-p gemini-cli-buffer)
        (progn
          (with-current-buffer gemini-cli-buffer
            (vterm-send-string region-text)
            (sleep-for 0.5)
            (vterm-send-escape)
            (vterm-send-return)
            (vterm-send-return)))
      (message "Gemini process not running. Run M-x gemini-cli-start first."))))

(defun gemini-cli-send-shift-key (key n)
  "Send a KEY with the shift modifier to the Gemini CLI.
This function sends the specified KEY to the `*gemini-cli*'
buffer N times with the shift modifier active.

Argument KEY is the key to send, as a string.
Argument N is the number of times to send the key."
  (if (buffer-live-p gemini-cli-buffer)
      (progn
        (with-current-buffer gemini-cli-buffer
          (dotimes (number n)
            (vterm-send-key key t))))
    (message "Gemini process not running. Run M-x gemini-cli-start first.")))

(defun gemini-cli-send-key (key n)
  "Send a KEY to the Gemini CLI.
This function sends the specified KEY to the `*gemini-cli*'
buffer N times.

Argument KEY is the key to send, as a string.
Argument N is the number of times to send the key."
  (if (buffer-live-p gemini-cli-buffer)
      (progn
        (with-current-buffer gemini-cli-buffer
          (dotimes (number n)
            (vterm-send-key key))))
    (message "Gemini process not running. Run M-x gemini-cli-start first.")))

(defun gemini-cli-page-up ()
  "Send the page up key to the Gemini CLI.
This is equivalent to pressing the Page Up key in the
`*gemini-cli*' buffer."
  (interactive)
  (gemini-cli-send-key "<prior>" 1))

(defun gemini-cli-page-down ()
  "Send the page down key to the Gemini CLI.
This is equivalent to pressing the Page Down key in the
`*gemini-cli*' buffer."
  (interactive)
  (gemini-cli-send-key "<next>" 1))

(defun gemini-cli-send-section ()
  "Send the current markdown section to the Gemini CLI.
This function finds the boundaries of the current markdown
section (using `outline-back-to-heading' and
`outline-end-of-subtree') and sends the text within that
section to the Gemini CLI."
  (interactive)
  (save-excursion
    (outline-back-to-heading t)
    (let ((start (point)))
      (outline-end-of-subtree)
      (gemini-cli-send-region start (point)))))

(defvar gemini-cli-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-p") 'gemini-cli-start)
    (define-key map (kbd "C-c C-z") 'gemini-cli-switch-buffer)
    (define-key map (kbd "C-c C-r") 'gemini-cli-send-region)
    (define-key map (kbd "C-M-x") 'gemini-cli-send-section)
    (define-key map (kbd "C-c M-p") 'gemini-cli-page-up)
    (define-key map (kbd "C-c M-n") 'gemini-cli-page-down)
    map)
  "Keymap for gemini-mode.")

(define-minor-mode gemini-cli-mode
  "A minor mode for interacting with the Gemini CLI.
This mode provides keybindings for starting the Gemini CLI,
switching to the CLI buffer, and sending text to the CLI."
  :init-value nil
  :lighter " Gemini"
  :keymap gemini-cli-mode-map)

;; Activate gemini mode for .gemini files
(define-derived-mode gemini-cli-major-mode prog-mode "Gemini"
  "Major mode for editing .gemini files.
This mode inherits from `prog-mode' and enables `gemini-cli-mode'
to provide Gemini CLI integration."
  (gemini-cli-mode 1))

(add-to-list 'auto-mode-alist '("\\.gemini\\'" . gemini-cli-major-mode))

(provide 'gemini-cli-mode)

;;; gemini-cli-mode.el ends here
