Emacs vanilla Tmux switcher

# Features

- [C-x o] - switch to a Tmux panel if it's visible, otherwise executes Emacs' `other-window`.
- [C-x 1] - close other Tmux panes if they're visible, otherwise executes Emacs' `delete-other-windows`.

# Minimal tmux.conf

```sh
unbind C-b
set -g prefix C-q
bind C-q send-prefix
is_emacs='echo "#{pane_current_command}" | grep -iqE "emacs"'
is_last_pane='echo "#{window_panes}" | grep -qwE "1"'
bind-key -T prefix % if-shell "$is_emacs" "send-prefix ; send-keys %" "split-window -h -c \"#{pane_current_path}\""
bind-key -T prefix c if-shell "$is_emacs" "send-prefix ; send-keys c" "new-window -c \"#{pane_current_path}\""
bind -Temacs-keys o if-shell "$is_emacs" "send C-x; send" "select-pane -t :.+"
bind -Temacs-keys 1 if-shell "$is_last_pane" "send C-x; send" "resize-pane -Z"
bind -Temacs-keys Any { send C-x; send }
bind -Troot C-x switch-client -Temacs-keys
```

# Licence

MIT
