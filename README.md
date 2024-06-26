# klister-lang
A work in progress language for your work in progress scripts

## Motivation

A lot of new bash code is being written despite it being an aged language with many serious issues. Clearly it fills a need for people - quickly whipping up a script that runs other programs - with a little bit of logic sprinkled in. This language is an attempt to fill that need while avoiding most of the footguns.

## Design philosophy

* It's better to have a 90% correct script today, than a 100% correct script in a week.
* It's better to crash on edge cases than risk doing the wrong thing.
* If you decide to handle edge cases it should be as smooth and terse as possible.

## Notable design choices

* If a program is invoked and exits with an error code an exception is raised. Like bash set -e, except that you could catch it if you want.
* Pipelines are by default not run concurrently. They only move on to the next stage if/when the first one completes successfully.
* To run programs the command must be enclosed in backticks. This is to create a clean separation between commands and builtins.
* Catching exceptions is done by converting them to result objects. This is done with `result_object = ?( expression )` or ``result_object = ?` command ` `` or `result_object = ?{ statementblock }`
* Globs will by-default raise an exception if they match a dash-prefixed file as that could be misinterpreted as a flag.
* Fallible implicit casts raise an exception if they fail. Casting bytes to a c-string will raise on embedded nulls. If an integer downcast would overflow it will raise.

## Status

Quite incomplete

* Types. Some progress on the type system. Might be completely overhauled but the result will still likely be fairly basic.
* C-imports. Very experimental. Might be extended or removed.
* Running programs, pipelines etc. Works but is pretty basic.
* Interpreter error handling: In progress. The interpreter attempts to give a useful error message on syntax error (but the line/col is a little off). Some runtime errors might be made catchable or not catchable.