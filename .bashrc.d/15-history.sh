# History settings
export HISTCONTROL=ignoreboth
export HISTFILESIZE=999
export HISTSIZE=999
shopt -s cmdhist
shopt -s histappend
shopt -s histreedit

# Smarter history searching
# http://lists.opensuse.org/opensuse-bugs/2009-10/msg10451.html
case "$-" in
    *i*) bind '"\e[A"':history-search-backward
         bind '"\e[B"':history-search-forward
esac
