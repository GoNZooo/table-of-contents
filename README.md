# table-of-contents

Install with:

```bash
stack install
```

Run `table-of-contents --help` for help:

```bash
$ table-of-contents --help
table-of-contents - Generate a table of contents for a markdown file

Usage: table-of-contents COMMAND
  Generate a table of contents for a markdown file

Available options:
  -h,--help                Show this help text

Available commands:
  watch                    Watch a path for changes and execute a command on
                           each file change
  print                    Print table of contents for a Markdown file
  inject                   Inject table of contents into a Markdown file
```

## Commands

### print

Prints a table of contents for a given markdown file when run; does nothing to the file.

### inject

Injects a table of contents into a given markdown file when run; idempotent.

### watch

Takes "print" or "inject" as arguments, plus a directory path, then watches said directory for
changes to Markdown files. On update, they will have the given action executed on them
automatically. This is good for when you forget to inject new ToCs into files after having changed
them.

Example ToC output looks like this:

```markdown
- [Composite datatypes](#composite-datatypes)
  - [Bool](#bool)
  - [Newtypes](#newtypes)
  - [Record types](#record-types)
  - [Union types](#union-types)
  - [Combining records and unions](#combining-records-and-unions)
  - [Generic datatypes](#generic-datatypes)
  - [Commonly used composite datatypes](#commonly-used-composite-datatypes)
    - [Maybe](#maybe)
    - [Either](#either)
    - [List / []](#list--)
    - [Tuples](#tuples)
  - [Strictness annotations](#strictness-annotations)
    - [Lists and lazyness](#lists-and-lazyness)
    - [More tools for strictness](#more-tools-for-strictness)
    - [More extensive material on lazyness](#more-extensive-material-on-lazyness)
```
