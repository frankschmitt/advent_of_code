cd ~/src/advent_of_code/2017/current
# vim session with source files
tmux "new-session" -s "aoc" -d
#tmux send-keys -t aoc 'cd ~/src/advent_of_code/2017/current' C-m
tmux send-keys -t aoc 'vim *.*' C-m
#  grunt watch for auto compilation
#tmux "split-window" -v -p 30
#tmux send-keys -t aoc 'cd ~/src/fs_webapps/fsWebVocabularyTrainer' C-m
#tmux send-keys -t aoc 'grunt watch' C-m
# run HTTP server
tmux "split-window" -h
#tmux send-keys -t aoc 'cd ~/src/fs_webapps/fsWebVocabularyTrainer' C-m
#tmux send-keys -t aoc 'bundle exec thin start --ssl -p 2810' C-m
tmux attach -t "aoc"

