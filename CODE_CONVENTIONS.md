# R Coding Conventions

Even though there is no official style guide, R is mature and steady enough to have an “unofficial” convention. This document will outline some of those conventions to help developer keep code consitent throughout this application.

This [R Coding Style Guide](https://www.r-bloggers.com/%F0%9F%96%8A-r-coding-style-guide/) may be referenced for general conventions. It has been borrowed from HEAVILY for this document :)

## Why use conventions

As code is passed from one developer to another, it is very useful to have the code written in a unified and consitent manner. This allows the developer just picking up a piece of code to more quickly read and understand the code. The more readily code can be read and understood, the faster actual useful development may begin and development that is inline with the original code structure (it makes better sense).

Consistency in code also reduces the amount of trivial merge conflict when committing code and thus saves more time.

## R file names

R file names should be meaningful and use the .R file extension. File names should be written in [snake case](https://en.wikipedia.org/wiki/Snake_case)

Good:

```R
my_fetch_data.R
01_read_data.R
02_validate_data.R
03_mutate_data.R
```

Bad:

```R
FileTwo.r
FileThree
```

If the file contains only one function, name it by the function name.

## Qualifying namespaces

Explicitly qualify namespaces for all external functions. While there is a small performance penalty for using ::, it makes it easier to understand dependencies in the code.

```R
purrr::map()
```

## Explicit globals

When using global variables, show that explicitly by referencing them from the .globalEnv.

Good:

```R
.GlobalEnv$DB_HOST
```

Bad:

```R
DB_HOST
```
