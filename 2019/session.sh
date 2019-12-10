cd ~/src/advent_of_code/2019/current
# vim session with source files
tmux "new-session" -s "aoc" -d
tmux send-keys -t aoc 'vim *.*' C-m
tmux "split-window" -h
tmux attach -t "aoc"

