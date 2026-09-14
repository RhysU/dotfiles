# Colon-separated list handling, wrapped for PATH and its kin.
# All are idempotent, so a nested shell re-sourcing these leaves PATH alone.
# Underscored locals keep indirect expansion clear of the caller's variables.
# Adapted from https://superuser.com/questions/39751/

# list_items_valid ITEM...
# Report whether every ITEM is free of the separator, naming those that
# are not.  Complaints carry the caller's name rather than this one.
list_items_valid()
{
    local _item _status=0
    for _item in "$@"
    do
        case $_item in
            *:*) echo "${FUNCNAME[1]}: colon within '$_item'" 1>&2
                 _status=1;;
        esac
    done
    return $_status
}

# list_append_missing NAME ITEM...
# Append each ITEM to the colon-separated list named NAME, skipping those it
# already holds.  Export NAME so children inherit it.  Return 2 given no NAME
# and 1 given any ITEM holding the separator, in which case NAME goes
# untouched rather than half updated.
list_append_missing()
{
    local _name=$1 _item
    shift || return 2
    list_items_valid "$@" || return 1
    for _item in "$@"
    do
        case ":${!_name-}:" in
            *":$_item:"*) continue;;
        esac
        printf -v "$_name" '%s' "${!_name:+${!_name}:}$_item" || return 1
    done
    export "$_name"
}

# list_prepend_missing NAME ITEM...
# Prepend each ITEM to the colon-separated list named NAME, skipping those it
# already holds.  Items keep their argument order, so the first named lands
# leftmost.  Errors as list_append_missing does.
list_prepend_missing()
{
    local _name=$1 _item _i
    shift || return 2
    list_items_valid "$@" || return 1
    for ((_i = $#; _i > 0; _i--))
    do
        _item=${!_i}
        case ":${!_name-}:" in
            *":$_item:"*) continue;;
        esac
        printf -v "$_name" '%s' "$_item${!_name:+:${!_name}}" || return 1
    done
    export "$_name"
}

# path_append_missing DIR...
# Append each existing DIR to PATH, skipping those it already holds.
path_append_missing()
{
    local _dir
    list_items_valid "$@" || return 1
    for _dir in "$@"
    do
        test -d "$_dir" && list_append_missing PATH "$_dir"
    done
}

# path_prepend_missing DIR...
# Prepend each existing DIR to PATH, skipping those it already holds.
path_prepend_missing()
{
    local _dir _i
    list_items_valid "$@" || return 1
    for ((_i = $#; _i > 0; _i--))
    do
        _dir=${!_i}
        test -d "$_dir" && list_prepend_missing PATH "$_dir"
    done
}
