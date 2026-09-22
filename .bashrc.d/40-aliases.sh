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

# Make-ish alias-like functions (so they may be invoked as commands)
m()       { nice make "$@"; }
mj()      { nice make -j"$(nproc-available)" "$@"; }
mjl()     { local n; n=$(nproc-available); nice make -j"$n" -l"$n" "$@"; }
mjlc()    { local n; n=$(nproc-available); nice make -j"$n" -l"$n" -C "$@"; }
sm()      { V=0 nice make "$@"; }
smj()     { V=0 nice make -j"$(nproc-available)" "$@"; }
smjl()    { local n; n=$(nproc-available); V=0 nice make -j"$n" -l"$n" "$@"; }
smjlc()   { local n; n=$(nproc-available); V=0 nice make -j"$n" -l"$n" -C "$@"; }

# Create and change to a directory
mkcd      () {                 mkdir -vp "$1" && cd    "$1"; }
mkpushd   () {                 mkdir -vp "$1" && pushd "$1"; }
rmmkcd    () { rm -rfv "$1" && mkdir -vp "$1" && cd    "$1"; }
rmmkpushd () { rm -rfv "$1" && mkdir -vp "$1" && pushd "$1"; }

# Permit the OS X-like 'open' to work on Linux for one or more files
open() { for file in "$@"; do xdg-open "$file" & done; }

# Sometimes one wants to convert an alias into a function for subshells
convert_alias_to_exported_function() {
    if alias -p | grep -q "$1"; then
       local aliaspath="$(alias -p | grep $1 | cut -d= -f2)"
       unalias $1                            # Unalias must be before eval...
       eval "$1 () { $aliaspath \"\$@\"; }"  # ...otherwise name expands
       eval "export -f $1"
    fi
}
