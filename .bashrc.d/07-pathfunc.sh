# Colon-separated list handling, wrapped for PATH and its kin.
# Adapted from https://superuser.com/questions/39751/

# Report whether colon-separated list $1 already holds item $2.
list_contains()
{
    case ":$1:" in
        *":$2:"*) return 0;;
    esac
    return 1
}

# Append each item to the colon-separated list named $1, skipping those held.
list_append_missing()
{
    local name=$1 item
    shift
    for item in "$@"
    do
        list_contains "${!name}" "$item" && continue
        printf -v "$name" '%s' "${!name:+${!name}:}$item"
    done
}

# Prepend each item to the colon-separated list named $1, skipping those held.
# Items keep their argument order, so the first named lands leftmost.
list_prepend_missing()
{
    local name=$1 item i
    for ((i = $#; i > 1; i--))
    do
        item=${!i}
        list_contains "${!name}" "$item" && continue
        printf -v "$name" '%s' "$item${!name:+:${!name}}"
    done
}

# Add existing directories to PATH.  Both are idempotent, so a nested shell
# re-sourcing these settings leaves PATH alone.
path_append_missing()
{
    local dir dirs=()
    for dir in "$@"
    do
        test -d "$dir" && dirs+=("$dir")
    done
    list_append_missing PATH "${dirs[@]}"
}

path_prepend_missing()
{
    local dir dirs=()
    for dir in "$@"
    do
        test -d "$dir" && dirs+=("$dir")
    done
    list_prepend_missing PATH "${dirs[@]}"
}
