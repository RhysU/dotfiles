# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
command -v lesspipe >/dev/null 2>&1 && eval "$(SHELL=/bin/sh lesspipe)"

# Avoid some ulimit problems on systems where modifying it is permitted
ulimit -s unlimited 2>/dev/null || true

# enable color support of ls
if command -v dircolors >/dev/null 2>&1; then
    eval "$(dircolors -b)"
fi

# Pager and locale settings
export EDITOR=vim             # Default editor
export FCEDIT="vim -X"        # X-less editor for 'fc'
export LC_ALL=en_US.UTF-8     # Default locale
export PAPERSIZE=letter       # Default paper
export LESS="-R -F"           # Default less options
export PAGER=less             # Default pager

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
