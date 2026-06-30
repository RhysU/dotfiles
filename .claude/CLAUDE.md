WHEN INTERACTING/DISCUSSING WITH THE USER, CLAUDE SHOULD KEEP IN MIND:

I've been a software developer for several decades in a variety of languages and
applications spaces.  I find it frustrating when the same idea is presented
several different, conflicting ways instead of taking the time to present
a single, unified view of the information. When coding, please make commit
messages and documentation richly informative but terse.  Make Hemingway proud.
For example, instead of "you need to configure XYZ" use the imperative form
"Configure XYZ".  Do not use parentheticals (the respite of the lazy).  Instead,
realize parentheticals are the respite of the lazy!  Follow the existing style
of English prose found in the repository, both for docstrings and comments. Do
not tell me that you've "modernized" code-- code can be more or less complex but
its "modernity" is purely software fashion and not worth commenting upon.
Prefer idiomatic expressions specific to the programming language in use. Short
functions with minimal control flow are ideal. Never lazily import anything in
Python without asking me first!  In languages supporting it, prefer const
correctness where doing so does not increase complexity. Avoid recovering from
errors inside error handling logic because it makes debugging hard and causes
unintentional, emergent behavior.  Lastly, follow the coding conventions found
within the codebase as you start working with it.  Do not invent new formatting
or commenting practices found nowhere else in the code.  Lastly, in your
language eschew a needlessly dramatic tone.  Speak plainly.  I hate reading
comments and docstrings so they should be as short and as punchy as possible.


WHEN DEVELOPING SOFTWARE, CLAUDE MIGHT ADOPT THESE PRINCIPLES AS WARRANTED...
...AND CLAUDE IS ENCOURAGED TO ASK QUESTIONS ABOUT THEIR APPLICABILITY,
IF IT SEEMS APPROPRIATE TO IGNORE THEM, etc.

 1. Write tightly scoped modules, e.g. preferring header-only libraries where
    possible.  Tightly scoped modules often allow putting the whole
    implementation in one file.
 2. Keep functions short. One idea each, minimal branching.
 3. Guard with early returns.  Minimize "cold-paths" executed only on errors.
 4. Express immutability in whatever the language offers: const, INTENT(IN/OUT),
    frozen dataclasses with replace(), explicit constructors. Return new state.
 5. Format definitions uniformly: return type on its own line, Allman braces for
    definitions and K&R for control flow, comments and assignment runs aligned
    into columns.
 6. Comment imperatively and briefly.
 7. Write generic, precision-agnostic numerics. Prefer iterators and templates.
 8. Group and alphabetize imports and includes. Pull in specific names, never
    wholesale.
 9. Pick one well-defined language standard and enforce it. Probe support with
    autoconf. Stay pedantic and portable.
10. Guard invariants with an always-on ENSURE-style macro. Route errors through
    one settable handler with integer codes. Fail loudly. Never recover inside
    error handling.
11. Type Python fully and ship py.typed. Format with black/ruff at 79 columns,
    check with mypy, package under src/.
12. Lead the README with a feature bullet list.
13. Derive the math in the header. Use LaTeX.
14. Cite the literature with bracketed keys.
15. Attribute meticulously. Respect each project's existing license. Credit
    reporters and sources of inspiration, in the commit when that is where the
    fix arose.
16. Write the commit subject as a terse imperative, capitalized, no period,
    under ~50 chars; prefix it with the component in multi-part repos
    ("suzerain:", "esio:"). Add a body only when the why is non-obvious, as
    short prose, never bullet lists, explaining rationale or root cause, and
    reference the project's own tracker.
17. Generate API docs with Doxygen and Graphviz, in every language including
    Fortran.
18. Paste real, verbatim program output into the README.
19. Keep prose and commit bodies plain, candid, and self-deprecating. Two spaces
    after every period. Admit mistakes, dead-ends, and uncertainty rather than
    polishing them away; call a hack a hack. No marketing.
20. Make commits atomic and single-purpose. Separate formatting from logic. Keep
    version bumps and NEWS/changelog entries as their own commits. Mark
    incomplete work honestly: "(in-progress)", "draft", "Scaffold out".
21. Stamp the version from version control: git describe, setuptools_scm.
22. Make optional dependencies optional. Build and test with and without them.
23. Test with the community-standard framework.
24. Run CI across a version matrix. Deploy docs from CI.
25. Adopt each ecosystem's conventional tooling and layout rather than inventing
    your own.
