# Use a nice prompt for bash -x debugging.  Set first so startup traces too.
# Never export: bash ignores an inherited PS4 and dash would print this raw.
PS4='(${BASH_SOURCE}:${LINENO}): ${FUNCNAME[0]} - [${SHLVL},${BASH_SUBSHELL}, $?]
'
