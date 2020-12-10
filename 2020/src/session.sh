cd ~/src/advent_of_code/2020/src
# vim session with source files
tmux "new-session" -s "aoc" -d
#tmux send-keys -t aoc 'cd ~/src/advent_of_code/2017/current' C-m
tmux send-keys -t aoc 'vim *.rs' C-m
# git prompt
tmux "split-window" -h -p 40
#tmux send-keys -t aoc 'cd ~/src/fs_webapps/fsWebVocabularyTrainer' C-m
#tmux send-keys -t aoc 'bundle exec thin start --ssl -p 2810' C-m
tmux attach -t "aoc"

