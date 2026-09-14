# Colon-separated list handling, wrapped for PATH and its kin.
# All are idempotent, so a nested shell re-sourcing these leaves PATH alone.
# Underscored locals keep indirect expansion clear of the caller's variables.
# Adapted from https://superuser.com/questions/39751/

# list_append_missing NAME ITEM...
# Append each ITEM to the colon-separated list named NAME, skipping those it
# already holds.  Export NAME so children inherit it.  Return 1 given an ITEM
# holding the separator or refusing assignment, and 2 given no NAME at all.
list_append_missing()
{
    local _name=$1 _item _status=0
    shift || return 2
    for _item in "$@"
    do
        case $_item in
            *:*) echo "${FUNCNAME[0]}: colon within '$_item'" 1>&2
                 _status=1
                 continue;;
        esac
        case ":${!_name-}:" in
            *":$_item:"*) continue;;
        esac
        printf -v "$_name" '%s' "${!_name:+${!_name}:}$_item" || _status=1
    done
    export "$_name"
    return $_status
}

# list_prepend_missing NAME ITEM...
# Prepend each ITEM to the colon-separated list named NAME, skipping those it
# already holds.  Items keep their argument order, so the first named lands
# leftmost.  Errors as list_append_missing does.
list_prepend_missing()
{
    local _name=$1 _item _status=0 _i
    test $# -gt 0 || return 2
    for ((_i = $#; _i > 1; _i--))
    do
        _item=${!_i}
        case $_item in
            *:*) echo "${FUNCNAME[0]}: colon within '$_item'" 1>&2
                 _status=1
                 continue;;
        esac
        case ":${!_name-}:" in
            *":$_item:"*) continue;;
        esac
        printf -v "$_name" '%s' "$_item${!_name:+:${!_name}}" || _status=1
    done
    export "$_name"
    return $_status
}

# path_append_missing DIR...
# Append each existing DIR to PATH, skipping those it already holds.
path_append_missing()
{
    local _dir _dirs=()
    for _dir in "$@"
    do
        test -d "$_dir" && _dirs+=("$_dir")
    done
    list_append_missing PATH "${_dirs[@]}"
}

# path_prepend_missing DIR...
# Prepend each existing DIR to PATH, skipping those it already holds.
path_prepend_missing()
{
    local _dir _dirs=()
    for _dir in "$@"
    do
        test -d "$_dir" && _dirs+=("$_dir")
    done
    list_prepend_missing PATH "${_dirs[@]}"
}
