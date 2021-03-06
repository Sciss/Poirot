# Poirot

[![Build Status](https://travis-ci.org/Sciss/Poirot.svg?branch=master)](https://travis-ci.org/Sciss/Poirot)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/de.sciss/poirot_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/de.sciss/poirot_2.11)

## statement

Poirot is a Scala front-end for the [JaCoP](https://github.com/Sciss/jacop) constraints solver library. It was
originally based on the Scala DSL included with JaCoP, written and (C)opyright by Krzysztof Kuchcinski and
Radoslaw Szymanek. All modifications and new interface (C)opyright 2013&ndash;2020 by Hanns Holger Rutz. All rights reserved.

Poirot is released under the [GNU Affero General Public License](http://github.com/Sciss/Poirot/blob/master/LICENSE)
and comes with absolutely no warranties. Please see the [JaCoP](http://jacop.osolpro.com/index.php?option=com_content&view=article&id=5&Itemid=2)
website for supplementary AGPL terms. To contact the author, send an e-mail to `contact at sciss.de`

## linking

To use this project as a library, use the following artifact:

    "de.sciss" %% "poirot" % v

The current version `v` is `0.3.1`

## building

This project currently builds with sbt against Scala 2.13 and 2.12. It requires the JaCoP 3.5.0 artifact published under
the `de.sciss` group ID. It should be automatically retrieved from Maven Central.

To compile, use `sbt compile`, for the API docs `sbt doc`, to jump into a REPL `sbt console`.

## contributing

Please see the file [CONTRIBUTING.md](CONTRIBUTING.md)

## differences to the original Scala DSL

- You must use an implicit instance of `Model`
- Operators follow standard Scala naming, e.g. `|`, `&`, `^` instead of `\/`, `/\`, `xor` for boolean combinators
- Standard Scala style, e.g. argument names, camel case etc.; more consistent use of empty parens
- Removing unnecessary things such as `ClassTag`s
- Removing global state when possible
- `BoolVar` is called `BooleanVar`, `FloatVar` is called `DoubleVar`.

## getting started

You need evidence of a `Model` instance for most operations. Constraints are automatically posted to that model.
Examples are provided in the test directory, you can selectively run them via `sbt test:run`. If you write small
self containing problems, you may mix in the `Problem` trait which provides exactly one model (store).

For example, have a look at the standard n-Queens problem, `Queen.scala`.

## limitations

I am currently figuring out how to best handle the syntax. No all methods from the original Scala DSL are implemented.

I have only worked with int and boolean variables, so pretty much everything regarding `SetVar`, `IntSet`, `FSM`,
`Network` is not tested or cleaned up.
