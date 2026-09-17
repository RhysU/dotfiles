# Maintain dotfiles within a bare git repository, adopted from
# https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/
dotfiles() { command git "--git-dir=$HOME/.dotfiles" "--work-tree=$HOME" "$@"; }
dotfiles-install() {
  (
    set -ex
    dotfiles init
    dotfiles config --local status.showUntrackedFiles no
    dotfiles remote add origin "git@github.com:RhysU/dotfiles.git"
    dotfiles fetch
    dotfiles checkout origin/master -ft
    dotfiles submodule init
    dotfiles submodule update
  )
}
# Third:
#   Fire up vim and run :BundleUpdate

# Add color to basic commands when possible, even where GNU is spelled 'gls'
function egrep() { command egrep --color=auto "$@"; }
function fgrep() { command fgrep --color=auto "$@"; }
function grep()  { command grep  --color=auto "$@"; }
if command -v gls >/dev/null 2>&1; then
    ls() { command gls --color=auto "$@"; }
else
    ls() { command ls --color=auto "$@"; }
fi

# "H foo" searches history for "foo" ignoring other "H anything" entries
# Additional magic present to remove duplicate commands when nonconsecutive
H() { history | egrep -v '^ *[[:digit:]]+ +H +' | grep "$@" | sort -rk 2 | uniq -f 1 | sort; }

# I am tired of having view not colorize things
view() { vim -XR "$@"; }

# Abbreviate moving up in the directory hierarchy
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."

# Handy for working with Conda
sa() { test -n "$1" && conda activate "$@" || conda env list; }
sd() { conda deactivate "$@"; }

# A variety of alias-like functions (so they may be invoked as commands)
benicer()  { renice +5 -p $BASHPID; }
h()        { history "$@" ; }
okular()   { command okular >&/dev/null "$@"; }
o()        { octave --silent --persist "$@" ; }
p()        { parallel "$@" ; }
s()        { screen "$@" ; }
t()        { command time --verbose "$@" ; }
v()        { vim "$@" ; }

# Screen-handling aware variations on common commands


# Make-ish alias-like functions (so they may be invoked as commands)
m()       { nice make "$@"; }
mj()      { nice make -j"$(nproc-available)" "$@"; }
mjl()     { local n; n=$(nproc-available); nice make -j"$n" -l"$n" "$@"; }
mjlc()    { local n; n=$(nproc-available); nice make -j"$n" -l"$n" -C "$@"; }
sm()      { V=0 nice make "$@"; }
smj()     { V=0 nice make -j"$(nproc-available)" "$@"; }
smjl()    { local n; n=$(nproc-available); V=0 nice make -j"$n" -l"$n" "$@"; }
smjlc()   { local n; n=$(nproc-available); V=0 nice make -j"$n" -l"$n" -C "$@"; }

# Stop typing "ipython --pylab" all the time
alias pylab="ipython --pylab"

# Remove ANSI escape sequences from a file
stresc () { 'sed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g"''"]"' "$@"; }

# Create and change to a directory
mkcd      () {                 mkdir -vp "$1" && cd    "$1"; }
mkpushd   () {                 mkdir -vp "$1" && pushd "$1"; }
rmmkcd    () { rm -rfv "$1" && mkdir -vp "$1" && cd    "$1"; }
rmmkpushd () { rm -rfv "$1" && mkdir -vp "$1" && pushd "$1"; }

# Idiomatic 'ssh -t hostname screen -xR'
stsxr() { ssh -t $* 'bash -ls -c "screen -xR"'; }

# Permit the OS X-like 'open' to work on Linux for one or more files
open() { for file in "$@"; do xdg-open "$file" & done; }

# Utility to add a prefix to the second and subsequent parameters
strpre() { p=$1; shift; for s in "$@"; do echo -n "$p$s "; done }

# Shortcuts for working with valgrind and/or libtool
# Complication for valgrind invocation comes from limited .valgrindrc functionality
alias libtoolddd="     libtool --mode=execute ddd"
alias libtoolgdb="     libtool --mode=execute gdb"
alias libtoolgdbtui="  libtool --mode=execute gdb -tui"
alias libtoolvalgrind="libtool --mode=execute valgrind --db-command='gdb -tui -w %f %p' $(strpre --suppressions= $HOME/.valgrind/*.supp)"
alias libtoolmpiexec=" libtool --mode=execute mpiexec"

# Open Vim with NERDTree (possibly at some bookmark) and Tagbar
NERD() { vim -c "NERDTree${1+FromBookmark $1}" -c "Tagbar"; }

# Report maximum resident set size of a given command
maxrss() { /usr/bin/time -f "\nMaximum resident set size (Kb): %M" "$@"; }

# Emit on stdout how to quote stdin for bash
# Lifted from https://news.ycombinator.com/item?id=24659282
bashquote() { printf '%q\n' "$(cat)" ; }

# Sometimes one wants to convert an alias into a function for subshells
convert_alias_to_exported_function() {
    if alias -p | grep -q "$1"; then
       local aliaspath="$(alias -p | grep $1 | cut -d= -f2)"
       unalias $1                            # Unalias must be before eval...
       eval "$1 () { $aliaspath \"\$@\"; }"  # ...otherwise name expands
       eval "export -f $1"
    fi
}

# Because I'm tired of typing {cat,vim} `which thing`...
command_which() {
    local cmd=$1
    shift;
    local where=$(command which -- "$@")
    local mime=$(command file --dereference -- "$where")
    if echo "$mime" | grep ' text ' >/dev/null 2>&1; then
        "$cmd" "$where"
    else
        echo "Cowardly refusing to $cmd $where with type:\n" 1>&2
        echo "$mime" 1>&2
        false
    fi
}
catwhich() { command_which cat "$@"; }
vimwhich() { command_which vim "$@"; }
