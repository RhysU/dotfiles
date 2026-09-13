# Nothing here matters outside an interactive shell
case $- in
    *i*) ;;
      *) return;;
esac

# Aliases ease using https://github.com/RhysU/zc against a particular database
if test -x "${HOME}/bin/zc"; then
    z_raw() { "$HOME/bin/zc" -d "$HOME/.zc"       "$@"; }
    z_add() { "$HOME/bin/zc" -d "$HOME/.zc" -a -- "$@"; }
else
    z_raw() { echo "RhysU/zc is not installed" 1>&2; false; }
    z_add() { :; }
fi

# Run http://github.com/RhysU/zc in some mode against fixed database.
# Whenever it succeeds and only one line is output, attempt to pushd there.
z() {
    local zout newline='
'
    if zout=$(z_raw "$@"); then
        case $zout in
          *"$newline"*) echo "$zout"          ;;  # 2+ matches => display
                    "") builtin pushd "$@"    ;;  # No match => use input
                     *) builtin pushd "$zout" ;;  # One match => use match
        esac
    fi
}
zf() { z -f -- "$@"; }  # Abbreviation
zr() { z -r -- "$@"; }  # Abbreviation
zt() { z -t -- "$@"; }  # Abbreviation

# Z becomes much more useful with tab completion hooks installed.
# Above z() must accommodate trailing slashes because of '-o filenames'.
_z_completion() {
    COMPREPLY=()
    while IFS=$'\n' read -r line; do
        COMPREPLY+=("$line")
    done < <(z_raw -k -- "${COMP_WORDS[@]:1}")
}
complete -o filenames -F _z_completion z

# On every command, record the working directory using z_add.
# Versus https://github.com/rupa/z, updating per-command nicer for GNU Screen.
# Also, the updates are lighter weight on account of backing zc implementation.
precmd_z() {
    # Only register non-HOME directories as HOME is just a 'cd' away.
    if [ "$PWD" != "$HOME" ]; then
        z_add "$PWD"
    fi
}

precmd_functions+=(precmd_z)
