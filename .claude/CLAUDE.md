# Working with me

I've developed software for decades across many languages and problem domains.
Speak to me as a peer.  My deepest frustration is one idea presented several
conflicting ways.  Present a single, unified view.

Prime directive: match the repository.  Follow its existing prose, docstring,
comment, formatting, and layout conventions.  The defaults below fill gaps only
where the repo has no established convention.  When a default would fight the
codebase, the codebase wins.

The tone and prose rules are firm.  The engineering defaults are
context-dependent; adopt them as warranted and ask when ignoring one seems
right.

## Tone and prose

Speak plainly.  No dramatic tone, no marketing, no exclamatory flourish.  Be
candid and self-deprecating: admit mistakes, dead-ends, and uncertainty rather
than polishing them away.  Call a hack a hack.

Write terse but richly informative.  Make Hemingway proud.  Comments and
docstrings stay short and punchy; comment imperatively.  Prefer the imperative
"Configure XYZ" over "you need to configure XYZ".  No parentheticals.  Two
spaces after every period.

Never call code "modernized".  Code is more or less complex, never more or less
fashionable.

## Code structure

Write tightly scoped modules, preferring header-only libraries where the
language allows, often the whole implementation in one file.  Keep functions
short: one idea each, minimal branching.  Guard with early returns and minimize
cold paths run only on error.

Express immutability in whatever the language offers: const, INTENT(IN/OUT),
frozen dataclasses with replace(), explicit constructors.  Return new state.
Prefer const correctness where it does not increase complexity.

Prefer idiomatic expressions specific to the language in use.  Write generic,
precision-agnostic numerics; prefer iterators and templates.

## Imports

Group and alphabetize imports and includes.  Pull in specific names, never
wholesale.  Never lazily import anything in Python without asking me first.

## Formatting defaults

When the repo sets no convention: put the return type on its own line, Allman
braces for definitions, K&R braces for control flow, comments and assignment
runs aligned into columns.

Type Python fully and ship py.typed.  Format with black/ruff at 79 columns,
check with mypy, package under src/.

## Errors and invariants

Guard invariants with an always-on ENSURE-style macro.  Route errors through one
settable handler with integer codes.  Fail loudly.  Never recover inside error
handling: it hides bugs and breeds emergent behavior.

## Standards and portability

Pick one well-defined language standard and enforce it.  Probe support with
autoconf.  Stay pedantic and portable.  Keep optional dependencies optional;
build and test both with and without them.

## Commits

Make commits atomic and single-purpose.  Separate formatting from logic.  Keep
version bumps and NEWS/changelog entries as their own commits.  Mark incomplete
work honestly: "(in-progress)", "draft", "Scaffold out".

Write the subject as a terse imperative, capitalized, no period, under ~50
chars, prefixed by the component in multi-part repos: "suzerain:", "esio:".  Add
a body only when the why is non-obvious; keep it short prose, never bullet
lists, explaining rationale or root cause and referencing the project's tracker.

Attribute meticulously and respect each project's license.  Credit reporters and
sources of inspiration in the commit where the fix arose.

## Documentation

Lead the README with a feature bullet list.  Paste real, verbatim program output
into it.  Derive the math in the header in LaTeX.  Cite the literature with
bracketed keys.  Generate API docs with Doxygen and Graphviz, in every language
including Fortran.

## Testing, CI, and versioning

Test with the community-standard framework.  Run CI across a version matrix and
deploy docs from CI.  Stamp the version from version control: git describe,
setuptools_scm.

Adopt each ecosystem's conventional tooling and layout rather than inventing
your own.
