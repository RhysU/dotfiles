# Colon-separated list handling, wrapped for PATH and its kin.
# All are idempotent, so a nested shell re-sourcing these leaves PATH alone.
# Adapted from https://superuser.com/questions/39751/

# list_contains LIST ITEM
# Report whether colon-separated LIST already holds ITEM.
list_contains()
{
    case ":$1:" in
        *":$2:"*) return 0;;
    esac
    return 1
}

# list_append_missing NAME ITEM...
# Append each ITEM to the colon-separated list named NAME, skipping those held.
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

# list_prepend_missing NAME ITEM...
# Prepend each ITEM to the colon-separated list named NAME, skipping those held.
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

# path_append_missing DIR...
# Append each existing DIR to PATH, skipping those held.
path_append_missing()
{
    local dir dirs=()
    for dir in "$@"
    do
        test -d "$dir" && dirs+=("$dir")
    done
    list_append_missing PATH "${dirs[@]}"
}

# path_prepend_missing DIR...
# Prepend each existing DIR to PATH, skipping those held.
path_prepend_missing()
{
    local dir dirs=()
    for dir in "$@"
    do
        test -d "$dir" && dirs+=("$dir")
    done
    list_prepend_missing PATH "${dirs[@]}"
}
