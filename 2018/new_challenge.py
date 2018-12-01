#!/usr/bin/python

import os
import shutil
import subprocess
import sys

"""Adds the boilerplate code for a new challenge.

   Usage: new_challenge.py <day> <challenge-name>

   This will create a new directory <day>-<challenge-name>, copy the boilerplate over 
   and launch vim with the challenge.adoc file where you can paste the instructions.
   """

if __name__ == "__main__":
  # setup directory
  day = sys.argv[1]
  name = sys.argv[2]
  dirname = day + "-" + name
  shutil.copytree("../templates/haskell/", dirname)
  # ensure "current" links to the new directory
  if os.path.islink("current"):
    os.unlink("current")
  os.symlink(dirname, "current")

  # TODO: launch tmux
  # neither approach works correctly
  #p = subprocess.Popen(['/usr/local/bin/tmux', 'new-session']) 
  #
  #pid=os.fork()
  #if pid==0: # new process
  #  os.system("tmux new-session")
  #  exit()

