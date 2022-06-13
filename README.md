Manage and navigate Tmux panes from Emacs and vise versa.

There is several packages out there to address Emacs client <-> Tmux navigation
issue. However, this one rebinds Emacs and Tmux vanilla (default) shortcuts,
preserving a canonical workflow.

Minimal version: [Tmux 3.3-rc](https://github.com/tmux/tmux/tree/3.3-rc).

# Features

* [C-x o] - switches to a Tmux panel if it's visible, otherwise executes Emacs'
  `other-window`. The pane circling feature works intuitively on both Emacs
  and other Tmux panes (keeping Emacs `other-window` navigation order).
* [C-x 1] - closes other Tmux panes if they're visible, otherwise executes
  Emacs' `delete-other-windows`.
* [C-x 0] - closes current Tmux panes (no-op on the last pane) or executes
  Emacs' `delete-window`.
* [tmux-prefix-key c] - creates a new Tmux window preserving Emacs buffer's path
  (defined by 'ttymux-pane-directory-method custom variable).
* [tmux-prefix-key %] - splits Tmux pane vertically preserving Emacs buffer's
  path (defined by 'ttymux-pane-directory-method custom variable).
* [tmux-prefix-key "] - splits Tmux pane horizontally preserving Emacs buffer's
  path (defined by 'ttymux-pane-directory-method custom variable).

# Default tmux.conf

```sh
unbind C-b
set -g prefix C-q
bind C-q send-prefix
is_emacs="ps -o state= -o comm= -t '#{pane_tty}' | grep -iqE emacs"
is_other_panes='echo "#{window_panes}" | grep -vqwE "1"'
bind-key -T prefix % if "$is_emacs" "send-prefix ; send-keys %" "split-window -h -c \"#{pane_current_path}\""
bind-key -T prefix \" if "$is_emacs" 'send-prefix ; send-keys \"' "split-window -v -c \"#{pane_current_path}\""
bind-key -T prefix c if "$is_emacs" "send-prefix ; send-keys c" "new-window -c \"#{pane_current_path}\""
bind -Temacs-keys o if "$is_emacs" "send C-x; send" "select-pane -t :.+"
bind -Temacs-keys Any { send C-x; send }
bind -Temacs-keys 1 { kill-pane -a; send C-x; send }
bind -Temacs-keys 0 if "$is_emacs" "send C-x; send" 'if $is_other_panes kill-pane'
bind -Troot C-x switch-client -Temacs-keys
```

# Known issues

## `new-window` and `split-window` are executed on a different Tmux session

If Emacs client is using and multiple Tmux sessions are running on the same
machine, `split-window` and `new-window` commands always open a pane on a
particular Tmux session. To avoid this run Emacs server outside a Tmux session.

# Licence

MIT
