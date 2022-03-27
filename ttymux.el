;;; ttymux.el --- Emacs vanilla Tmux switcher  -*- lexical-binding: t -*-
;;
;; Copyright (c) 2022 Elijah Danko
;;
;; Author: Illia A. Danko <me@eli.net>
;; URL: https://github.com/elijahdanko/ttymux.el
;; Created: February 19, 2022
;; Keywords: convenience, terminals, tmux, window, pane, navigation, integration
;; Package-Requires: ((emacs "24") (projectile "2.0.0"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tmux workflow goodies.

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

;;; Tmux integration (See tmux.conf for details).
;; Feature added (https://github.com/tmux/tmux/issues/2904).
;;
;; Minimal tmux.conf:
;; unbind C-b
;; set -g prefix C-q
;; bind C-q send-prefix
;; is_emacs='echo "#{pane_current_command}" | grep -iqE "emacs"'
;; is_last_pane='echo "#{window_panes}" | grep -qwE "1"'
;; bind-key -T prefix % if-shell "$is_emacs" "send-prefix ; send-keys %" "split-window -h -c \"#{pane_current_path}\""
;; bind-key -T prefix c if-shell "$is_emacs" "send-prefix ; send-keys c" "new-window -c \"#{pane_current_path}\""
;; bind -Temacs-keys o if-shell "$is_emacs" "send C-x; send" "select-pane -t :.+"
;; bind -Temacs-keys 1 { kill-pane -a; send C-x; send }
;; bind -Temacs-keys Any { send C-x; send }
;; bind -Troot C-x switch-client -Temacs-keys

(require 'projectile)

(defcustom ttymux-pane-directory-method 'project
  "Advice Tmux to choose a directory to open a pane."
  :type '(choice (const :tag "Project path" project)
                 (const :tag "Active buffer path" buffer)
                 (const :tag "Home folder path" home)))

(defvar ttymux-prefix-key "C-q"
  "Tmux prefix key.")

(defvar ttymux-new-window-key "c"
  "Create a new Tmux tab key.")

(defvar ttymux-split-horizonatally-key "%"
  "Split Tmux window horizontally.")

(defvar ttymux-fallback-directory "~"
  "Tmux pane path be used as a last resort.")

(defvar ttymux-new-window-fn 'ttymux-new-window-default
  "Specify a function to open tmux window.")

(defvar ttymux-split-horizonatally-fn 'ttymux-split-horizonatally-default
  "Specify a function to open tmux pane horizontally.")

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

(defun ttymux-new-window-default ()
  "Create a new Tmux window/tab. Pane path is defined by
`ttymux-pane-directory-method'."
  (ttymux--tmux-cmd "new-window" "-c" (ttymux--pane-directory)))

(defun ttymux-new-window ()
  (interactive)
  (funcall ttymux-new-window-fn))

(defun ttymux-split-horizonatally-default ()
  "Create a new horizontal Tmux pane. Pane path is defined by
`ttymux-pane-directory-method'."
  (interactive)
  (ttymux--tmux-cmd "split-window" "-h" "-c" (ttymux--pane-directory)))

(defun ttymux-split-horizonatally ()
  (interactive)
  (funcall ttymux-split-horizonatally-fn))

(defun ttymux--next-pane ()
  (ttymux--tmux-cmd "select-pane" "-t" ":.+"))

(defun ttymux--tmux-cmd-string-equal (value &rest args)
  (let* ((cmd-args (apply 'concat "tmux " args))
         (cmd-output (string-trim (shell-command-to-string cmd-args))))
    (string-equal value cmd-output)))

(defun ttymux--tmux-other-window ()
  "Circle over Emacs windows, if the last window switch to the
next tmux pane if any."
  (interactive)
  (cond
   ((eq (next-window) (get-buffer-window))
    (ttymux--next-pane))
   ((and (not (window-in-direction 'right))
         (not (window-in-direction 'below))
         (or (ttymux--tmux-cmd-string-equal "1" "display-message -p \"#{pane_at_right}\"")
             (ttymux--tmux-cmd-string-equal "1" "display-message -p \"#{pane_at_left}\"")
             (not (ttymux--tmux-cmd-string-equal "1" "display-message -p \"#{window_panes}\""))))
    (other-window 1)
    (ttymux--next-pane))
   (t (other-window 1))))

(defvar ttymux-mode-hook nil
  "Run after ttymux-mode turned on.")

(defun ttymux--mode-on ()
  "Override default keys. Run hooks."
  (let ((new-window-key (concat ttymux-prefix-key
                                " "
                                ttymux-new-window-key))
        (split-horizonatally-key (concat ttymux-prefix-key
                                       " "
                                       ttymux-split-horizonatally-key)))
    (global-set-key (kbd ttymux-prefix-key) nil) ; remove other keys if any
    (global-set-key (kbd new-window-key) #'ttymux-new-window)
    (global-set-key (kbd split-horizonatally-key) #'ttymux-split-horizonatally)
    (global-set-key [remap other-window] #'ttymux--tmux-other-window))
  (run-hooks 'ttymux-mode-hook))

(defun ttymux--mode-off ())

;;;###autoload
(define-minor-mode ttymux-mode
  "Switch between Emacs and Tmux using vanilla Emacs shortcuts,
such as C-x o and C-x 1."
  :lighter " TTYMUX"
  (if ttymux-mode
      (ttymux--mode-on)
    (ttymux--mode-off)))

(provide 'ttymux)

;;; ttymux.el ends here
