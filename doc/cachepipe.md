# cachepipe

A memoizing replacement for the shell pipe.  Each stage's output is cached and
keyed by the pipeline prefix that produced it, so editing the tail of a pipeline
replays the head from cache instead of recomputing it.

Status: contract only.  No implementation yet.

The central trade, stated before anything else: **a memoizing pipe is not a
pipe.**  Caching a stage means running it to completion, so a downstream stage
that exits early no longer kills the stages upstream of it.  Everything below
follows from choosing memoization over pipe semantics rather than pretending to
offer both.

## 1  Model

A pipeline is a linear chain of stages.  Each stage is an argv vector executed
directly, with no shell.  Stage *i* reads stage *i-1*; stage 0 reads the
pipeline's stdin; the last stage writes the pipeline's stdout.

`cachepipe`'s argv is the cached region, and the shell's `|` bounds it.  Two
markers carry two concepts and nothing carries either twice: `::` delimits
stages inside the region, `|` puts a stage outside it.  A stage outside is never
read from the cache and never written to it, which is why the tool needs no way
to say "keep this out of the cache" of its own.

The cache memoizes prefixes.  For each prefix of length *i* the tool may hold an
entry recording the exact bytes that prefix wrote to stdout and the exit status
of every stage in it.  A run replays the longest usable prefix and executes the
rest.

Caching is best-effort and semantically invisible.  For a terminating pipeline
of deterministic, side-effect-free stages, the bytes on stdout and the exit
status are identical with a cold cache, a warm cache, a full disk, or no cache
directory at all.  The rest of this document exists to keep that sentence true.

## 2  Grammar

    cachepipe [--OPTION...] [+ATTR...] CMD [ARG...] [ :: [+ATTR...] CMD [ARG...] ]...
    cachepipe --ACTION

The separator is `::`.  No convention exists to borrow: a repeatable stage
delimiter living inside argv is a rare shape, and every near-convention means
something else.  `--` says options are over, GNU parallel taught `:::` to mean
fan out across an argument list, and `find -exec ... \;` needs an escape.  `::`
is safe unquoted, legal as a filename, and connotes a section boundary rather
than an argument list.

Legality as a filename is what lets a separator also name the program, so an
installed `:c` gives a uniform `:c curl -s URL :: jq .items` with no wrapper.
The cost is a livelier collision than a rarer glyph would carry: `::` is a
plausible argument in its own right, as in `grep ::` over C++ sources, and `sep`
is the escape.

Leading `--` tokens belong to the tool.  The first bare or `+` token starts
stage 1.  An unrecognized `--x` before the first command is an error, never a
command name, because no command is named `--freshh`.  The same closed-set rule
applies to `+`.

Attributes describe one stage and precede its command.  `+` cannot collide with
a command name, so `date +%s` and `sort +2` parse untouched: the scan stops at
the first bare token.

| Attribute | Effect |
|---|---|
| `+fresh`   | Executes this stage rather than replaying it, making every prefix ending at or after it unreadable |
| `+N`       | Widens what counts as success for this stage |

There is no attribute for "do not cache this stage".  Fresh is the shell's own
`|`, outside the tool: `cachepipe curl -s URL :: jq .items | head -n5` runs
`head` afresh every time.  `cachepipe`'s argv is exactly the cached region, and
anything you do not want cached belongs outside it.  An attribute that suppressed
caching mid-region could only mean one of two things, and both are already
spelled better: publish nothing downstream, which is a plain pipe, or publish
entries that no pipeline can ever read, which is a leak.

No attribute takes a value.  Multiple accepted statuses are repetition, `+1 +2
grep ...`, so the grammar needs no value delimiter.  Parsing decides on the
first character after `+`: digits are a status, letters are a name, anything
else is an error.

An empty stage, a doubled separator, or a trailing separator is a usage error
with a message naming the position.

## 3  Options

Every option has two spellings, `--name=value` and `CACHEPIPE_NAME`.  Precedence
is flag, then environment, then default.  There is no curated subset and nothing
to memorize.

| Option | Meaning |
|---|---|
| `session`    | Explicit token sharing one namespace across terminals |
| `root`       | Cache root.  Point at `$XDG_RUNTIME_DIR/cachepipe` for RAM-backed storage that dies at logout |
| `sep`        | Separator override, for a pipeline needing a literal `::` argument |
| `max-bytes`  | Per-entry cap, overriding the measured default |
| `reserve`    | Free-space floor never consumed |
| `budget`     | Total bytes before LRU eviction |
| `fresh`      | Equivalent to `+fresh` on stage 1 |
| `quiet`      | Suppress the plan on stderr |
| `explain`    | Print the plan and exit without executing |

Actions take no pipeline: `--help`, `--status`, `--prune`, `--clear`,
`--shell-init`.  Naming one forbids stages.

## 4  Identity

A key is a Merkle chain over the prefix, not a hash of a flattened list:

    k[0] = H(version || uid || cwd || stdin-identity)
    k[i] = H(k[i-1] || encode(stage[i]))

`encode` is injective: each argument length-prefixed.  Chaining makes the key
stable under replay by construction, so reading a prefix back cannot change that
prefix's own key.

No attribute enters the encoding.  `+fresh` is a transient instruction, and
keying on it would hide the entry it just published from every later run.  `+N`
changes no byte of output: `+1 grep pat` and `grep pat` write the same bytes,
and an entry published by the first is sound for the second to replay, since the
recorded status is reported either way.  So a key is a function of argv alone.

`version` covers the on-disk format and the key construction, so a change to
either invalidates everything old without touching a file.  `cwd` is in the salt
because `cat data.json` means different things in different directories.  The
environment is not, and the escape hatch is to name variables explicitly.

## 5  Stdin

Stdin content is not otherwise in the key, so it is resolved at `k[0]`.  Every
prefix contains stage 0, so a rule that refuses caching whenever stdin is
unidentifiable would refuse it always: an interactive shell has a terminal on
stdin even when nothing reads it.  The key therefore records what stage 0
actually consumed, which is knowable after the fact.

- Stdin is a seekable regular file.  Its content hashes into `k[0]` for both
  lookup and publication.  `jq . < a.json` hits its own cache and never collides
  with `< b.json`.
- Anything else.  Lookup uses a "consumed nothing" identity.  If stage 0 then
  reads zero bytes, publication proceeds under that identity, which is the
  common case of a first stage that ignores stdin.  If stage 0 reads any bytes
  from an unidentifiable source, nothing is published for any prefix.

So `curl -s URL :: jq .` caches at a terminal, `producer | cachepipe jq .`
caches nothing, and `cat :: wc -l` typed by hand stores nothing rather than
replaying yesterday's typing.  `< /dev/null` needs no special case: it reads
zero bytes and lands in the second branch.

Piped stdin therefore stays unmemoizable, and the fix is to name the producer
rather than pipe it: `cachepipe producer :: jq .items` memoizes the whole chain.
Content-keying the piped bytes was considered and rejected as too complicated
for its payoff.  It would identify anonymous bytes by their digest, but the
digest is unknown until the stream is complete, so the stage after the boundary
could not start until the stage before it finished.  That trades concurrency for
reuse, pays off only when the input is unpredictable yet frequently unchanged,
and fails invisibly when it does not: the pipeline merely gets slower, with
nothing to diagnose.

A cache hit that replaces stage 0 does not consume stdin.

## 6  Validity and publication

Recording the exit status is correctness.  Publishing on it is policy.  The
original tool's sin was not caching a failure but discarding the status while
keeping the bytes.

Every entry records the per-stage exit statuses, and replay reproduces them.
This is not configurable.

Publication additionally requires an accepted status, defaulting to `{0}`.
`+N` adds to that set; `0` is always in it, since "0 means failure" is not a
thing.  `N` runs 0 through 255 and names an exit status, so `+137` means a stage
that called `exit(137)` and never `SIGKILL`.  Signals are never acceptable and
no attribute overrides that: a stage killed by a signal did not choose its
output, so its bytes are not a result.  An unaccepted status poisons its own
prefix and every longer one.

A `+KILL` spelling admitting signals was considered and rejected.  A signal
means the byte stream is short, so accepting one restores the truncated-entry
defect this design exists to remove.  `+PIPE` would be inert besides: draining
means a cacheable stage never takes a SIGPIPE, and the stages that can take one
never publish.  The plausible motive is a deadline, which is spelled with a
wrapper: `timeout` exits 124 and `+124` already covers it.  Adding `+KILL`
alongside `+137` would also invite reading the two as synonyms, which they are
not.

There is no built-in table of per-command exit conventions.  `1` means opposite
things in `grep` and in `cat`, and a table would have to parse argv to tell
`grep -q` from `grep`, would be wrong for busybox, and would make semantics
depend invisibly on a program's name.  See the appendix for the survey.

Refusing publication on status is otherwise silent, so it is reported once with
the fix:

    grep pat: exit 1, not cached -- pass `+1` if that status is a result here

Publication is a `link`, never a rename.  `O_TMPFILE` is the ideal source
because the file has no name until it is published, so an interrupted run leaves
nothing to clean up.  It is not portable, so it is probed once and falls back to
a named temp inside the session dir.  Both paths publish by `link`, which makes
`EEXIST` mean "someone published this first, discard mine".  First-writer-wins
costs nothing under the determinism assumption and needs no lock.

Named temps carry the session token so the pruner sweeps a killed run's
leftovers as ordinary dead-session debris.

Each entry carries metadata: format version, the prefix's argv, byte count,
exit statuses, creation time.  On read the argv and byte count are
verified.  A mismatch is corruption, and corruption earns a loud diagnostic, an
unlink, and a recompute.  Recomputing is the normal path for disposable state,
not recovery inside error handling.

Interruption publishes nothing partial.  Prefixes that had already completed at
the moment of interruption are still published; killing `jq` does not throw away
the `curl` already paid for.

## 7  Replay and completion

Every cacheable stage is drained to completion whether or not anything
downstream is still reading.  Consequences, stated plainly:

- `curl big.json :: jq .items :: head -n5` downloads the whole file.  That is
  the price of the entry that makes the next run cheap.  Move the stage outside
  `cachepipe` with a shell `|` to buy pipe semantics back.
- Draining stops at a per-entry byte cap.  Past the cap the entry is abandoned,
  the stage reverts to ordinary pipe semantics, and a diagnostic says so.  This
  keeps `yes :: head -n1` from hanging and keeps the cap from ever changing the
  answer.

Replay reproduces stdout and exit status only.  Stderr is never captured and
never replayed, so a warm run is quieter than a cold one.  Side effects are not
replayed either: a stage that writes files writes them on the first run and not
on the second.  Both are hazards of the idea itself and belong in the first
paragraph of the manual, not in a footnote.

## 8  Exit status and diagnostics

Exit status is the first nonzero stage status, else zero.  This is `pipefail`,
not bash's default, because a memoizer that reports the tail's success over the
head's failure will cache garbage and then swear it is fine.  Replayed prefixes
report the statuses recorded at publication.

Reserved codes: 2 for usage errors, and a distinct code for cache-integrity
failure.  Everything else is the pipeline's own status.

The plan prints to stderr before execution.  A replayed prefix collapses to
`<r>`, and a stage whose entry will be published is followed by `<w>`:

    curl -s URL <w> :: jq .items <w> :: head -n5 <w>
    <r> :: head -n10 <w>

## 9  Storage

The root is `$XDG_CACHE_HOME/cachepipe`, falling back to `~/.cache/cachepipe`.
`$XDG_RUNTIME_DIR` is the opt-in alternative and not the default: it is a tmpfs
sized at ten percent of RAM by default, shared with dbus, gnupg, ssh-agent and
the compositor, so filling it breaks the whole desktop session rather than just
this tool.  Drain-to-completion makes filling it plausible.  Liveness pruning
already reclaims dead sessions, so the runtime dir adds only promptness.

Root and session dirs are 0700 and uid-owned, opened `O_NOFOLLOW`.  Wrong
ownership or mode is fatal and loud, never silently repaired.  A symlink where
an entry belongs is an integrity failure, not a redirect.

The session token is `sid` plus the session leader's start time, read from
`/proc/<sid>/stat` field 22.  The start time is what defeats pid recycling;
`sid` alone lets a new shell inherit a dead one's cache.  Where the platform
will not yield a start time the token is unverifiable and the dir is treated as
dead.  Liveness checks always fail toward discarding: a wrong "dead" costs a
recompute, a wrong "alive" serves someone else's bytes.

`session` names a token explicitly and every shell naming it shares one
namespace.  A named token has no leader, so it is pruned by age and budget
rather than by liveness.  Sharing remains uid-scoped and 0700; a token is a
convenience for one person across terminals, not a multi-user cache.

## 10  Lifetime and reclamation

Every invocation is the collector.  On startup, scan sibling session dirs and
remove any whose leader is gone or whose start time no longer matches.  Cost is
one `stat` per session dir.  No hook, no daemon, nothing to install.

`cachepipe --shell-init` prints a trap for an rc file to `eval`, which reclaims
the session dir at shell exit.  It fires on normal exit and on `SIGHUP`, so it
covers logout and a closed terminal, and it misses `SIGKILL` and a lost machine.
The trap is an optimization and never a guarantee.  Correctness must not depend
on the user having edited an rc file.

Entries do not expire; they are evicted.  The root has a byte budget enforced
LRU.  `--fresh` forces re-execution and republication, and it exists solely for
stale successes, since failures are never published in the first place.

A full store is a cache event, never a pipeline event.  On `ENOSPC` while
draining, the entry is abandoned, the stage reverts to pipe semantics, a
diagnostic names the store and the shortfall, and the bytes keep flowing
downstream unharmed.  Three rules support this:

- The byte cap is measured, not constant.  It derives from free space at
  startup, so a 20 MB `/tmp` yields a cap of a few megabytes rather than a fixed
  default that guarantees failure.
- The reserve floor is never consumed.  Caching stops while free space is below
  the floor, so `cachepipe` is never the process that breaks the machine.
- Eviction runs before the cap bites: LRU within the session dir first, then
  dead sessions, then stop caching.  Evicting your own older entries is right;
  evicting a live sibling session's entries is not.

On a store too small for the reserve floor, `cachepipe` runs every pipeline
uncached, says so once, and is merely a slower `|`.  That is correct behaviour
and needs no special path.

## 11  Non-goals

No shell semantics: no globs, redirection, builtins, or quoting rules.  No
dependency tracking: the key covers the command, not the files, network, or
clock behind it, so `cat f :: jq .` keeps serving the old contents after `f`
changes.  No fan-out, no DAG, no build system.  No sandboxing.  Bytes are bytes
and nothing is decoded as text.

## 12  Invariants

1. A published entry is the complete stdout of its prefix from a run in which
   every stage terminated with an accepted status and no signal.
2. Presence, absence, exhaustion, or corruption of the cache never changes
   stdout or exit status for a terminating, deterministic, side-effect-free
   pipeline.
3. Key equality implies prefix equality up to SHA-256, and key computation is
   stable under replay substitution.
4. No entry is ever observed partially written.
5. No read or write follows a symlink or escapes the cache root, and no entry is
   read that is not owned by the uid at 0600.
6. Interruption never publishes a partial entry and never leaves a temp file
   that the pruner will not collect.
7. A prefix containing a `+fresh` stage is never replayed.
8. A session dir is reused only when its leader is alive and its start time
   matches.  Unverifiable is dead.
9. Publication is a link into the session dir; a key is written at most once per
   session.
10. Every stored byte is reachable from a session dir whose liveness any later
    invocation can decide, so nothing becomes unreclaimable.

## Appendix: worked invocations

Output below is illustrative, unlike the evidence that follows it.

Cold run, then the same pipeline reused four ways.  This is the whole point of
the tool in five commands:

    $ cachepipe curl -s "$URL" :: jq .items
    curl -s https://example.com/big.json <w> :: jq .items <w>

    $ cachepipe curl -s "$URL" :: jq .items
    <r>

    $ cachepipe curl -s "$URL" :: jq .items :: head -n5
    <r> :: head -n5 <w>

    $ cachepipe curl -s "$URL" :: jq .items :: head -n20
    <r> :: head -n20 <w>

    $ cachepipe curl -s "$URL" :: jq .count :: head -n5
    <r> :: jq .count <w> :: head -n5 <w>

A regular file on stdin hashes into the key.  A pipe cannot, so nothing caches:

    $ cachepipe jq .items < big.json
    jq .items <w>

    $ producer | cachepipe jq .items :: head -n5
    jq .items :: head -n5
    cachepipe: stage 1 read from a pipe; nothing cached this run

A nonzero status that means "no" rather than "failed" is refused once, with the
fix named:

    $ cachepipe grep -c pat access.log :: awk '{s+=$1} END {print s}'
    grep -c pat access.log :: awk ...
    cachepipe: grep exit 1, not cached -- pass `+1` if that status is a result here

    $ cachepipe +1 grep -c pat access.log :: awk '{s+=$1} END {print s}'
    grep -c pat access.log <w> :: awk ... <w>

Several accepted statuses are repetition, needing no delimiter:

    $ cachepipe +1 +2 check-drift ./config :: tee drift.log
    check-drift ./config <w> :: tee drift.log <w>

A shell `|` ends the cached region, so the tail runs fresh every time and the
head still caches.  `+fresh` re-executes a stale head and republishes it:

    $ cachepipe curl -s "$URL" :: jq .items | head -n5
    curl -s https://example.com/big.json <w> :: jq .items <w>

    $ cachepipe +fresh curl -s "$URL" :: jq .items
    curl -s https://example.com/big.json <w> :: jq .items <w>

A failing stage propagates its status and publishes nothing, so the next run
genuinely retries:

    $ cachepipe curl -sf "$URL" :: jq .items ; echo "rc=$?"
    curl -sf https://example.com/big.json :: jq .items
    rc=22

Early exit downstream no longer kills upstream, and the cap is what keeps that
affordable:

    $ cachepipe curl -s "$URL" :: head -n5
    curl -s https://example.com/big.json <w> :: head -n5 <w>

    $ cachepipe curl -s "$BIG" :: head -n5
    curl -s https://example.com/huge.tar <w> :: head -n5 <w>
    cachepipe: stage 1 exceeded 96M cap, entry abandoned, pipe semantics resumed

Inspection elides prefix chains to the left:

    $ cachepipe --status
    /home/rhys/.cache/cachepipe/2906.10681   3 entries   14.2M   budget 512M
       9.9M   2m ago   curl -s https://example.com/big.json
       4.3M   2m ago   ... :: jq .items
        12K   1m ago   ... :: jq .items :: head -n20

Naming a token shares one namespace across terminals for a project:

    $ export CACHEPIPE_SESSION=acme-etl
    $ cachepipe curl -s "$URL" :: jq .items
    <r>

## Appendix: evidence

`#|` is the obvious separator and the shell eats it silently, which is the
failure class this design exists to eliminate:

    $ bash -O interactive_comments -c "python3 argv.py a #| b"
      argv: ['a']
    $ bash -c "python3 argv.py a |# b"
      bash: -c: line 2: syntax error: unexpected end of file

Survivors of the same test were `::`, `:::`, `_/`, `//`, `%%`, `,,`, `@_` and
`--`.  Any glyph containing `|` is dead, and `;;` is a shell syntax error.

Only some survivors can also name a program, which is what allows the separator
and the command to be the same token:

    ::    legal      :::   legal      %%    legal      --    legal
    _/    ILLEGAL    //    ILLEGAL

`/` is the one byte forbidden in a filename, so every glyph that draws a pipe
bowl is disqualified from naming the tool.

Exit status `1` is load-bearing in both directions, which is why the accepted
set defaults to `{0}` and why no per-command table can be correct:

    grep nomatch < nums.txt            -> 1
    diff nums.txt <(echo x)            -> 1
    cmp nums.txt /dev/null             -> 1
    sort -c <(printf '2\n1\n')         -> 1
    jq -e .missing <<< '{}'            -> 1
    cat /nonexistent                   -> 1
    wc -l /nonexistent                 -> 1
    sed -e                             -> 1
    python3 -c 'raise SystemExit(1)'   -> 1

An unlinked directory cannot serve as a session-scoped cache, which is why
reclamation is by process liveness rather than by fd lifetime:

    dir fd still valid after rmdir: True
    created an entry in the removed dir: no -> ENOENT
    listing removed dir: []
