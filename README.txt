
  IntSeq is a Scheme library for defining and computing integer sequences,
  i.e. functions N -> Z.
  It consists of a set of Scheme modules, currently written for MIT/GNU Scheme
  but later intended to be ported to a reasonable basic set (e.g. R5RS) of
  common Scheme functionality.

  Unless otherwise noted, all code is
  Copyright (C) 2002-2013 Antti Karttunen, and is subject to GPL v2.
  (See the file COPYING in src directory).

  However, the sequence definitions themselves will be placed in Public
  Domain, as they are often based on public mathematical information
  already published in OEIS. (See https://oeis.org )

  Modules:

  src/definech.scm
    Code for memoizing definec and other macros.
    The actual memoization-macros (implement-cached-function)
    will be later separated into the module of their own.

  src/transforms.scm
    A set of higher-order functions which each transform almost any (*)
    integer sequence function to another integer sequence function.
    (* However, some of them expect genuinely monotone functions).
    In most cases the function defined (the result of transformation) will
    be defined with an internal memoization-cache, so the previous module
    is also needed.
    You can think this as a DSL ("Domain Specific Language") for defining
    integer sequences via elementary mathematical transformations.

  src/utils
    Miscellaneous utility-function modules.

