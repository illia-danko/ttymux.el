;;; ttymux.el --- Emacs <-> Tmux vanilla switcher -*- lexical-binding: t -*-
;;
;; Copyright (c) 2022 Elijah Danko
;;
;; Author: Elijah Danko <me@elijahdanko.net>
;; URL: https://github.com/elijahdanko/ttymux.el
;; Created: February 19, 2022
;; Keywords: convenience, terminals, tmux, window, pane, navigation, integration
;; Package-Requires: ((emacs "24") (projectile "2.0.0"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tmux workflow goodies.
;;
;; Features:
;; - [C-x o] - switches to a Tmux panel if it's visible, otherwise executes Emacs'
;;   `other-window`. The pane circling feature works intuitively on both Emacs
;;   and other Tmux panes (keeping Emacs `other-window` navigation order).
;; - [C-x 1] - closes other Tmux panes if they're visible, otherwise executes
;;   Emacs' `delete-other-windows`.
;; - [C-x 0] - closes current Tmux panes (no-op on the last pane) or executes
;;   Emacs' `delete-window`.
;; - [tmux-prefix-key c] - creates a new Tmux window preserving Emacs buffer's path
;;   (defined by 'ttymux-pane-directory-method custom variable).
;; - [tmux-prefix-key %] - splits Tmux pane vertically preserving Emacs buffer's
;;   path (defined by 'ttymux-pane-directory-method custom variable).
;; - [tmux-prefix-key "] - splits Tmux pane horizontally preserving Emacs buffer's
;;   path (defined by 'ttymux-pane-directory-method custom variable).
;;
;; Feature was requested  (https://github.com/tmux/tmux/issues/2904).
;; Ready to use strated from 3.3-rc  (https://github.com/tmux/tmux/tree/3.3-rc).
;; Tmux integration (See tmux.conf for details).
;;
;; ############################ Default tmux.conf #############################
;;
;; is_emacs='echo "#{pane_current_command}" | grep -iqE "emacs"'
;; is_other_panes='echo "#{window_panes}" | grep -vqwE "1"'
;; bind-key -T prefix % if "$is_emacs" "send-prefix ; send-keys %" "split-window -h -c \"#{pane_current_path}\""
;; bind-key -T prefix \" if "$is_emacs" 'send-prefix ; send-keys \"' "split-window -v -c \"#{pane_current_path}\""
;; bind-key -T prefix c if "$is_emacs" "send-prefix ; send-keys c" "new-window -c \"#{pane_current_path}\""
;; bind -Temacs-keys o if "$is_emacs" "send C-x; send" "select-pane -t :.+"
;; bind -Temacs-keys Any { send C-x; send }
;; bind -Temacs-keys 1 { kill-pane -a; send C-x; send }
;; bind -Temacs-keys 0 if "$is_emacs" "send C-x; send" 'if $is_other_panes kill-pane'
;; bind -Troot C-x switch-client -Temacs-keys

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'projectile)

(defcustom ttymux-pane-directory-method 'project
  "Advice Tmux to choose a default pane directory."
  :type '(choice (const :tag "Project path" project)
                 (const :tag "Current buffer's path" buffer)
                 (const :tag "Home folder path" home)))

(defvar ttymux-prefix-key "C-q"
  "Tmux prefix key.")

(defvar ttymux-new-window-key "c"
  "Create a new Tmux tab key.")

(defvar ttymux-split-horizontally-key "%"
  "Split Tmux window horizontally key.")

(defvar ttymux-split-vertically-key "\""
  "Split Tmux window vertically key.")

(defvar ttymux-fallback-directory "~"
  "Tmux pane path to be used as a last resort.")

(defvar ttymux-new-window-fn 'ttymux-new-window-default
  "Function to open Tmux window.")

(defvar ttymux-split-horizontally-fn 'ttymux-split-horizontally-default
  "Function to open Tmux pane horizontally.")

(defvar ttymux-split-vertically-fn 'ttymux-split-vertically-default
  "Function to open tmux pane vertically.")

(defun ttymux--current-directory (mode)
  (pcase mode
    ('dired-mode (dired-current-directory))
    (_ default-directory)))

(defun ttymux--pane-directory ()
  (pcase ttymux-pane-directory-method
    ('project (or (projectile-project-root)
                  (ttymux--current-directory major-mode)
                  ttymux-fallback-directory))
    ('buffer (or (ttymux--current-directory major-mode)
                 ttymux-fallback-directory))
    (_ ttymux-fallback-directory)))

(defun ttymux--tmux-cmd (&rest args)
  (ignore-errors (apply 'call-process "tmux" nil nil nil args)))

;;;###autoload
(defun ttymux-new-window-default ()
  "Create a new Tmux window / tab.
Pane path is defined by `ttymux-pane-directory-method'."
  (ttymux--tmux-cmd "new-window" "-c" (ttymux--pane-directory)))

;;;###autoload
(defun ttymux-new-window ()
  "Create a new Tmux window.
Window path is defined by `ttymux-pane-directory-method'."
  (interactive)
  (funcall ttymux-new-window-fn))

;;;###autoload
(defun ttymux-split-horizontally-default ()
  "Create a new horizontal Tmux pane.
Pane path is defined by `ttymux-pane-directory-method'."
  (interactive)
  (ttymux--tmux-cmd "split-window" "-h" "-c" (ttymux--pane-directory)))

;;;###autoload
(defun ttymux-split-horizontally ()
  "Execute a custom ttymux-split-horizontally-fn function.
The function must split a new Tmux pane horizontally."
  (interactive)
  (funcall ttymux-split-horizontally-fn))

;;;###autoload
(defun ttymux-split-vertically-default ()
  "Create a new vertical Tmux pane.
Pane path is defined by `ttymux-pane-directory-method'."
  (interactive)
  (ttymux--tmux-cmd "split-window" "-v" "-c" (ttymux--pane-directory)))

;;;###autoload
(defun ttymux-split-vertically ()
  "Execute a custom ttymux-split-vertically-fn function.
The function must split a new Tmux pane vertically."
  (interactive)
  (funcall ttymux-split-vertically-fn))

(defun ttymux--next-pane ()
  (ttymux--tmux-cmd "select-pane" "-t" ":.+"))

(defun ttymux--tmux-cmd-string-equal (value &rest args)
  (let* ((cmd-args (apply 'concat "tmux " args))
         (cmd-output (string-trim (shell-command-to-string cmd-args))))
    (string-equal value cmd-output)))

(defun ttymux--tmux-pane-at-right ()
  (ttymux--tmux-cmd-string-equal "1" "display-message -p \"#{pane_at_right}\""))

(defun ttymux--tmux-pane-at-left ()
  (ttymux--tmux-cmd-string-equal "1" "display-message -p \"#{pane_at_left}\""))

(defun ttymux--tmux-the-last-pane ()
  (ttymux--tmux-cmd-string-equal "1" "display-message -p \"#{window_panes}\""))

;;;###autoload
(defun ttymux-other-window ()
  "Circle over Emacs windows or Tmux panes.
The function tries to follow a native `other-window' order as
closer as possible."
  (interactive)
  (cond
   ((eq (next-window) (get-buffer-window))
    (ttymux--next-pane))
   ((and (not (window-in-direction 'right))
         (not (window-in-direction 'below))
         (or (ttymux--tmux-pane-at-right)
             (ttymux--tmux-pane-at-left)
             (not (ttymux--tmux-the-last-pane))))
    (other-window 1)
    (ttymux--next-pane))
   (t (other-window 1))))

;;;###autoload
(defun ttymux-delete-window ()
  "Run `delete-window' or kill Tmux pane."
  (interactive)
  (cond
   ((or (not (eq (next-window) (get-buffer-window)))
        (ttymux--tmux-the-last-pane))
    (delete-window))
   (t (ttymux--tmux-cmd "kill-pane"))))

(defvar ttymux-mode-map
  (let ((map (make-sparse-keymap))
        (tmux-new-window-key (format "%s %s" ttymux-prefix-key ttymux-new-window-key))
        (tmux-split-horizontally-key (format "%s %s" ttymux-prefix-key ttymux-split-horizontally-key))
        (tmux-split-vertically-key (format "%s %s" ttymux-prefix-key ttymux-split-vertically-key)))
    (define-key map [remap other-window] #'ttymux-other-window)
    (define-key map [remap delete-window] #'ttymux-delete-window)
    (define-key map (kbd tmux-new-window-key) #'ttymux-new-window)
    (define-key map (kbd tmux-split-horizontally-key) #'ttymux-split-horizontally)
    (define-key map (kbd tmux-split-vertically-key) #'ttymux-split-vertically)
    map)
  "Keymap for `ttymux-mode'.
This keymap remaps standard `other-window' and `delete-window'
command, also creates addition TMUX related keybindings used
alongside with regular TMUX prefix key commands.")

(defvar ttymux-mode-hook nil
  "Run after ttymux-mode turned on.")

(defun ttymux--mode-on ()
  (run-hooks 'ttymux-mode-hook))

(defun ttymux--mode-off ())

;;;###autoload
(define-minor-mode ttymux-mode
  "Manage and navigate Tmux panes from Emacs and vise versa.
The mode allows a user to cycle over Emacs and Tmux panes with
Emacs [C-x o] (other-window) shortcut. It also rebinds [C-x 1]
and [C-x 0] for Tmux integration."
  :init-value nil
  :global t
  :lighter " TTYMUX"
  (if ttymux-mode
      (ttymux--mode-on)
    (ttymux--mode-off)))

(provide 'ttymux)

;;; ttymux.el ends here
