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

(require 'cl-lib) ;; Required for cl-loop and other cl-lib features

(defgroup gemini-cli nil
  "Gemini CLI interface."
  :group 'tools)

(defcustom gemini-cli-agents
  '((:name "gemini" :command "gemini"))
  "List of Gemini CLI agents configuration.
This variable is used to configure multiple agents.
Each element is a property list with keys:
:name - Name of the agent (string)
:command - Command to launch (string, default: `gemini-cli-cmd`)
:home-directory - Working directory (string, optional)
:initial-prompt - Initial command to send (string, optional)"
  :type '(repeat (plist :key-type symbol :value-type string))
  :group 'gemini-cli)

(defvar gemini-cli-active-buffers (make-hash-table :test 'equal)
  "Hash table mapping agent names to their vterm buffers.")

(defvar gemini-cli-last-buffer nil
  "The last visited Gemini CLI buffer.")

(defvar gemini-cli-buffer nil
  "Buffer for the Gemini CLI process.
DEPRECATED: Use `gemini-cli-last-buffer` or look up in `gemini-cli-active-buffers`.")

(defvar gemini-cli-cmd "gemini"
  "Command to use to launch gemini")

(defun gemini-cli-rebind-cli (&optional agent-name)
  "Bind the current buffer to a Gemini agent.
This is useful if the connection between the agent and the buffer
is lost or if you want to treat an existing buffer as an agent's buffer.
Prompts for AGENT-NAME if not provided."
  (interactive (list (completing-read "Bind current buffer to agent: "
                                      (mapcar (lambda (a) (plist-get a :name)) gemini-cli-agents)
                                      nil t)))
  (let ((name (or agent-name "gemini")))
    (puthash name (current-buffer) gemini-cli-active-buffers)
    (setq gemini-cli-last-buffer (current-buffer))
    (when (string= name "gemini")
      (setq gemini-cli-buffer (current-buffer)))
    (message "Bound current buffer to agent '%s'." name)))

(defun gemini-cli--log-conversation ()
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

(defun gemini-cli--get-agent-config (name)
  "Get the configuration for the agent with NAME."
  (cl-find-if (lambda (agent) (string= (plist-get agent :name) name))
              gemini-cli-agents))

(defun gemini-cli--resolve-agent-name (agent-config-or-name)
  "Resolve the agent name from AGENT-CONFIG-OR-NAME."
  (cond ((stringp agent-config-or-name) agent-config-or-name)
        ((and (listp agent-config-or-name) (plist-get agent-config-or-name :name))
         (plist-get agent-config-or-name :name))
        ((> (length gemini-cli-agents) 1)
         (completing-read "Select agent to start: "
                          (mapcar (lambda (a) (plist-get a :name)) gemini-cli-agents)))
        (t "gemini")))

(defun gemini-cli--resolve-config (agent-config-or-name agent-name)
  "Resolve the configuration for AGENT-NAME.
Use AGENT-CONFIG-OR-NAME if it is a configuration list."
  (or (if (listp agent-config-or-name)
          agent-config-or-name
        (gemini-cli--get-agent-config agent-name))
      (list :name "gemini" :command gemini-cli-cmd)))

(defun gemini-cli--initialize-session (buffer config ignore-logging-p)
  "Initialize the Gemini session in BUFFER with CONFIG.
IGNORE-LOGGING-P disables logging."
  (with-current-buffer buffer
    (let ((cmd (or (plist-get config :command) gemini-cli-cmd))
          (home-dir (plist-get config :home-directory))
          (init-prompt (plist-get config :initial-prompt)))
      (when (not ignore-logging-p)
        (gemini-cli--log-conversation))
      (when home-dir
        (vterm-send-string (format "cd %s" home-dir))
        (vterm-send-return))
      (vterm-send-string cmd)
      (vterm-send-return)
      (when init-prompt
        (vterm-send-string init-prompt)
        (gemini-cli-execute-prompt)
        (vterm-send-return)))))

(defun gemini-cli--setup-buffer-state (agent-name buffer)
  "Update global state for AGENT-NAME and BUFFER."
  (puthash agent-name buffer gemini-cli-active-buffers)
  (setq gemini-cli-last-buffer buffer)
  (when (string= agent-name "gemini")
    (setq gemini-cli-buffer buffer)))

(defun gemini-cli-start (&optional agent-config-or-name ignore-logging-p)
  "Start the Gemini CLI in a vterm buffer.
AGENT-CONFIG-OR-NAME can be a configuration plist or an agent name string.
If nil, it defaults to the \"gemini\" agent or prompts if multiple agents are defined.

This function opens a new vterm buffer named `*gemini-{{name}}*',
splits the window horizontally, and starts the Gemini CLI.

When called with a prefix argument IGNORE-LOGGING-P, it will
not log the conversation to a file.  Otherwise, it calls
`gemini-cli-log-conversation' to start logging."
  (interactive (list nil current-prefix-arg))
  (let* ((agent-name (gemini-cli--resolve-agent-name agent-config-or-name))
         (config (gemini-cli--resolve-config agent-config-or-name agent-name))
         (buffer (gethash agent-name gemini-cli-active-buffers)))

    (if (buffer-live-p buffer)
        (message "Agent '%s' is already running in buffer %s" agent-name buffer)
      (let ((new-window (split-window-horizontally))
            (new-buffer (vterm (format "*gemini-%s*" agent-name))))
        (gemini-cli--setup-buffer-state agent-name new-buffer)
        (set-window-buffer new-window new-buffer)
        (gemini-cli--initialize-session new-buffer config ignore-logging-p)))))

(defun gemini-cli--get-active-agent-names ()
  "Return a list of names of active agents."
  (cl-loop for k being the hash-keys of gemini-cli-active-buffers
           using (hash-values v)
           when (buffer-live-p v)
           collect k))

(defun gemini-cli--select-active-agent (prompt)
  "Prompt the user to select an active agent with PROMPT."
  (let ((active-names (gemini-cli--get-active-agent-names)))
    (if active-names
        (completing-read prompt active-names)
      nil)))

(defun gemini-cli-switch-buffer (&optional prefix)
  "Switch to a Gemini CLI buffer.
By default, switches to the last visited Gemini buffer.
With PREFIX argument (C-u), prompts to select an agent to switch to.
If no agent is running, it starts the default one."
  (interactive "P")
  (let ((target-buffer
         (if prefix
             (let ((agent-name (gemini-cli--select-active-agent "Switch to agent: ")))
               (if agent-name (gethash agent-name gemini-cli-active-buffers) nil))
           gemini-cli-last-buffer)))

    (if (buffer-live-p target-buffer)
        (progn
          (switch-to-buffer-other-window target-buffer)
          (setq gemini-cli-last-buffer target-buffer))
      (call-interactively 'gemini-cli-start))))

(defun gemini-cli--get-target-buffer (&optional prefix)
  "Get the target buffer for commands.
If PREFIX is non-nil, prompt the user to select an active agent.
Otherwise, return `gemini-cli-last-buffer`.
If the target buffer is not live, try to find another active one or return nil."
  (let ((buffer (cond (prefix
                       (let ((name (gemini-cli--select-active-agent "Execute in agent: ")))
                         (gethash name gemini-cli-active-buffers)))
                      ((buffer-live-p gemini-cli-last-buffer)
                       gemini-cli-last-buffer)
                      (t
                       (let ((name (car (gemini-cli--get-active-agent-names))))
                         (gethash name gemini-cli-active-buffers))))))
    (when (buffer-live-p buffer)
      (setq gemini-cli-last-buffer buffer)
      buffer)))

(defun gemini-cli-execute-prompt (&optional prefix)
  (interactive "P")
  (let ((target-buffer (gemini-cli--get-target-buffer prefix)))
    (if (buffer-live-p target-buffer)
        (progn
          (setq gemini-cli-last-buffer target-buffer)
          (with-current-buffer target-buffer
            (vterm-send-escape)
            (vterm-send-return)
            (vterm-send-return)))
      (message "Gemini process not running. Run M-x gemini-cli-start first."))))

(defun gemini-cli-send-prompt (prompt &optional sleep-time prefix)
  (interactive "sPrompt: \nP")
  (let ((target-buffer (gemini-cli--get-target-buffer prefix)))
    (if (buffer-live-p target-buffer)
        (progn
          (setq gemini-cli-last-buffer target-buffer)
          (with-current-buffer target-buffer
            (vterm-send-string prompt)
            (sleep-for (or sleep-time 0.5))
            (vterm-send-escape)
            (vterm-send-return)
            (vterm-send-return)))
      (message "Gemini process not running. Run M-x gemini-cli-start first."))))

(defun gemini-cli-send-region (start end &optional prefix)
  "Send the current region to the Gemini CLI process.
The text between START and END is sent to the Gemini CLI
buffer without switching the current buffer.

When called with a prefix argument (C-u), prompt for the target agent.

This function is interactive, so it can be called with `M-x
gemini-cli-send-region' or bound to a key.  When called
interactively, START and END are the boundaries of the current
region."
  (interactive "r\nP")
  (let ((region-text (buffer-substring-no-properties start end)))
    (gemini-cli-send-prompt region-text nil prefix)))

(defun gemini-cli--send-key (n key &optional shift meta ctrl accept-proc-output prefix)
  "Send a KEY with the shift modifier to the Gemini CLI.
This function sends the specified KEY to the Gemini CLI
buffer N times with the shift modifier active.

Argument KEY is the key to send, as a string.
Argument N is the number of times to send the key.
Optional modifiers SHIFT, META and CTRL.
Optional PREFIX to select agent."
  (let ((target-buffer (gemini-cli--get-target-buffer prefix)))
    (if (buffer-live-p target-buffer)
        (progn
          (setq gemini-cli-last-buffer target-buffer)
          (with-current-buffer target-buffer
            (dotimes (number n)
              (vterm-send-key key shift meta ctrl accept-proc-output))))
      (message "Gemini process not running. Run M-x gemini-cli-start first."))))

(defun gemini-cli-page-up (&optional prefix)
  "Send the page up key to the Gemini CLI.
This is equivalent to pressing the Page Up key in the
Gemini buffer.
With PREFIX, prompt for agent."
  (interactive "P")
  (gemini-cli--send-key 1 "<prior>" t nil nil nil prefix))

(defun gemini-cli-page-down (&optional prefix)
  "Send the page down key to the Gemini CLI.
This is equivalent to pressing the Page Down key in the
Gemini buffer.
With PREFIX, prompt for agent."
  (interactive "P")
  (gemini-cli--send-key 1 "<next>" t nil nil nil prefix))

(defun gemini-cli-send-section (&optional prefix)
  "Send the current markdown section to the Gemini CLI.
This function finds the boundaries of the current markdown
section (using `outline-back-to-heading' and
`outline-end-of-subtree') and sends the text within that
section to the Gemini CLI.
With PREFIX, prompt for agent."
  (interactive "P")
  (save-excursion
    (outline-back-to-heading t)
    (let ((start (point)))
      (outline-end-of-subtree)
      (gemini-cli-send-region start (point) prefix))))

(defun gemini-cli-start-line (&optional prefix)
  "Go to the start of the instruction prompts.
With PREFIX, prompt for agent."
  (interactive "P")
  (gemini-cli--send-key 1 "a" nil t nil nil prefix))

(defun gemini-cli-copy-last-result-at-point (&optional prefix)
  "Copy the last result of gemini-cli and copy it in the current buffer.
With PREFIX, prompt for agent."
  (interactive "P")
  (gemini-cli-send-prompt "/copy" 0.1 prefix)
  (sleep-for 0.1)
  (insert (shell-command-to-string "xclip -o -selection clipboard")))

(defun gemini-cli-show-all ()
  "Show all active Gemini CLI buffers in separate windows."
  (interactive)
  (let ((active-names (gemini-cli--get-active-agent-names)))
    (if (not active-names)
        (message "No active Gemini agents.")
      (delete-other-windows)
      (cl-loop for name in active-names
               for first = t then nil
               for buffer = (gethash name gemini-cli-active-buffers)
               when (buffer-live-p buffer)
               do (progn
                    (unless first
                      (split-window-horizontally)
                      (other-window 1))
                    (switch-to-buffer buffer))))))

(defvar gemini-cli-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-p") 'gemini-cli-start)
    (define-key map (kbd "C-c C-z") 'gemini-cli-switch-buffer)
    (define-key map (kbd "C-c C-r") 'gemini-cli-send-region)
    (define-key map (kbd "C-M-x") 'gemini-cli-send-section)
    (define-key map (kbd "C-c M-p") 'gemini-cli-page-up)
    (define-key map (kbd "C-c M-n") 'gemini-cli-page-down)
    (define-key map (kbd "C-c C-a") 'gemini-cli-start-line)
    (define-key map (kbd "C-c C-e") 'gemini-cli-copy-last-result-at-point)
    (define-key map (kbd "C-c C-<return>") 'gemini-cli-execute-prompt)
    map)
  "Keymap for gemini-mode.")

(define-minor-mode gemini-cli-mode
  "A minor mode for interacting with the Gemini CLI.
This mode provides keybindings for starting the Gemini CLI,
switching to the CLI buffer, and sending text to the CLI."
  :init-value nil
  :lighter "Gemini"
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
