## Nimbol: SPITBOL-like pattern construction and matching in Nim
##
## This module is a translation of the GNAT.Spitbol Ada library to Nim
##
## Copyright (C) 1997-2013, AdaCore
## Copyright (C) 2015, H. G. Weller
##
## Nimbol is free software;  you can  redistribute it  and/or modify it under
## terms of the  GNU General Public License as published  by the Free Soft-
## ware  Foundation;  either version 3,  or (at your option) any later ver-
## sion.  GNAT is distributed in the hope that it will be useful, but WITH-
## OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY
## or FITNESS FOR A PARTICULAR PURPOSE.
##
## As a special exception under Section 7 of GPL version 3, you are granted
## additional permissions described in the GCC Runtime Library Exception,
## version 3.1, as published by the Free Software Foundation.
##
## You should have received a copy of the GNU General Public License and
## a copy of the GCC Runtime Library Exception along with this program;
## see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
## <http://www.gnu.org/licenses >.
##
## Summary
## =======
##
## This package provides a complete implementation of the SPITBOL-like pattern
## construction and matching operations based on Macro-SPITBOL created by Robert
## Dewar.  SPITBOL is a completely general patterm matching package based on the
## pattern language of SNOBOL4 modeled on context free grammars, with context
## sensitive extensions that provide full (type 0) computational capabilities.
##
## Original GNAT.Spitbol source files
## `g-spipat.ads
## <https://www2.adacore.com/gap-static/GNAT_Book/html/rts/g-spipat__ads.htm>`_
## `g-spipat.adb
## <https://www2.adacore.com/gap-static/GNAT_Book/html/rts/g-spipat__adb.htm>`_
##
## Pattern Matching Tutorial
## =========================
##
## A pattern matching operation (a call to one of the match subprograms) takes a
## subject string and a pattern, and optionally a replacement string. The
## replacement string option is only allowed if the subject is a variable.
##
## The pattern is matched against the subject string, and either the match
## fails, or it succeeds matching a contiguous sub-string. If a replacement
## string is specified, then the subject string is modified by replacing the
## matched sub-string with the given replacement.
##
## Concatenation and Alternation
## -----------------------------
##
## A pattern consists of a series of pattern elements. The pattern is built up
## using either the concatenation operator::
##
## .. code-block:: nim
##   a & b
##
## which means match ``a`` followed immediately by matching ``b``, or the
## alternation operator::
##
## .. code-block:: nim
##   a or b
##
## which means first attempt to match ``a``, and then if that does not succeed,
## match ``b``.
##
## There is full backtracking, which means that if a given pattern element fails
## to match, then previous alternatives are matched.  For example if we have the
## pattern::
##
## .. code-block:: nim
##   (a or b) & (c or d) & (e or f)
##
## First we attempt to match ``a``, if that succeeds, then we go on to try to
## match ``c``, and if that succeeds, we go on to try to match ``e``. If ``e``
## fails, then we try ``f``. If ``f`` fails, then we go back and try matching
## ``d`` instead of ``c``. Let's make this explicit using a specific example,
## and introducing the simplest kind of pattern element, which is a literal
## string. The meaning of this pattern element is simply to match the characters
## that correspond to the string characters. Now let's rewrite the above pattern
## form with specific string literals as the pattern elements::
##
## .. code-block:: nim
##   ("abc" or "ab") & ("def" or "cde") & ("gh" or "ij")
##
## The following strings will be attempted in sequence::
##
##   abc . def . gh
##   abc . def . ij
##   abc . cde . gh
##   abc . cde . ij
##   ab . def . gh
##   ab . def . ij
##   ab . cde . gh
##   ab . cde . ij
##
## Here we use the dot simply to separate the pieces of the string matched by
## the three separate elements.
##
## Moving the start Point
## ----------------------
##
## A pattern is not required to match starting at the first character of the
## string, and is not required to match to the end of the string.  The first
## attempt does indeed attempt to match starting at the first character of the
## string, trying all the possible alternatives. But if all alternatives fail,
## then the starting point of the match is moved one character, and all possible
## alternatives are attempted at the new anchor point.
##
## The entire match fails only when every possible starting point has been
## attempted. As an example, suppose that we had the subject string::
##
## .. code-block:: nim
##   "ababcdeijkl"
##
## matched using the pattern in the previous example::
##
## .. code-block:: nim
##  ("abc" or "ab") & ("def" or "cde") & ("gh" or "ij")
##
## would succeed, after two anchor point moves::
##
## .. code-block:: nim
##   "ababcdeijkl"
##      ^^^^^^^
##      matched
##      section
##
## This mode of pattern matching is called the unanchored mode. It is also
## possible to put the pattern matcher into anchored mode by setting the global
## variable ``anchoredMode`` to ``true``. This will cause all subsequent matches
## to be performed in anchored mode, where the match is required to start at the
## first character.
##
## We will also see later how the effect of an anchored match can be obtained
## for a single specified anchor point if this is desired.
##
## Other Pattern Elements
## ----------------------
##
## In addition to strings (or single characters), there are many special pattern
## elements that correspond to special predefined alternations:
##   ============= =============================================================
##   Pattern Func     Description
##   ============= =============================================================
##   ``Arb``       Matches any string. First it matches the ``nil`` string, and
##                 then on a subsequent failure, matches one character, and
##                 then two characters, and so on. It only fails if the
##                 entire remaining string is matched.
##
##   ``Bal``       Matches a non-empty string that is parentheses balanced with
##                 respect to ordinary ``()`` characters. Examples of balanced
##                 strings are ``"abc"``, ``"a((b)c)"``, and ``"a(b)c(d)e"``.
##                 Bal matches the shortest possible balanced string on the
##                 first attempt, and if there is a subsequent failure, attempts
##                 to extend the string.
##
##   ``Abort``     Immediately aborts the entire pattern match, signaling
##                 failure. This is a specialized pattern element, which is
##                 useful in conjunction with some of the special pattern
##                 elements that have side effects.
##
##   ``Fail``      The ``nil`` alternation. Matches no possible strings, so it
##                 always signals failure. This is a specialized pattern
##                 element, which is useful in conjunction with some of the
##                 special pattern elements that have side effects.
##
##   ``Fence``     Matches the ``nil`` string at first, and then if a failure
##                 causes alternatives to be sought, aborts the match (like an
##                 ``Abort``). Note that using ``Fence`` at the start of a
##                 pattern has the same effect as matching in anchored mode.
##
##   ``Rem``       Matches from the current point to the last character in the
##                 string. This is a specialized pattern element, which is
##                 useful in conjunction with some of the special pattern
##                 elements that have side effects.
##
##   ``Succeed``   Repeatedly matches the ``nil`` string (it is equivalent to
##                 the alternation ``("" or "" or "" ....)``. This is a special
##                 pattern element, which is useful in conjunction with some of
##                 the special pattern elements that have side effects.
##   ============= =============================================================
##
## Pattern Construction Functions
## ------------------------------
##
## The following functions construct additional pattern elements:
##   ============= =============================================================
##   Pattern Func     Description
##   ============= =============================================================
##   ``Any(s)``    Where ``s`` is a string, matches a single character that is
##                 any one of the characters in ``s``. Fails if the current
##                 character is not one of the given set of characters.
##
##   ``Arbno(p)``  Where ``p`` is any pattern, matches any number of instances
##                 of the pattern, starting with zero occurrences. It is thus
##                 equivalent to ``("" or (p & ("" or (p & ("" ....)))))``.  The
##                 pattern ``p`` may contain any number of pattern elements
##                 including the use of alternation and concatenation.
##
##   ``Break(s)``  Where ``s`` is a string, matches a string of zero or more
##                 characters up to but not including a break character that is
##                 one of the characters given in the string ``s``.  Can match
##                 the ``nil`` string, but cannot match the last character in
##                 the string, since a break character is required to be
##                 present.
##
##   ``BreakX(s)`` Where ``s`` is a string, behaves exactly like ``Break(s)``
##                 when it first matches, but if a string is successfully
##                 matched, then a subsequent failure causes an attempt to
##                 extend the matched string.
##
##   ``Fence(p)``  Where ``p`` is a pattern, attempts to match the pattern ``p``
##                 including trying all possible alternatives of ``p``. If none
##                 of these alternatives succeeds, then the Fence pattern
##                 fails. If one alternative succeeds, then the pattern match
##                 proceeds, but on a subsequent failure, no attempt is made to
##                 search for alternative matches of ``p``. The pattern ``p``
##                 may contain any number of pattern elements including the use
##                 of alternation and concatenation.
##
##   ``Len(n)``    Where ``n`` is a natural number, matches the given number of
##                 characters. For example, ``Len(10)`` matches any string that
##                 is exactly ten characters long.
##
##   ``NotAny(s)`` Where ``s`` is a string, matches a single character that is
##                 not one of the characters of ``s``. Fails if the current
##                 character is one of the given set of characters.
##
##   ``NSpan(s)``  Where ``s`` is a string, matches a string of zero or more
##                 characters that is among the characters given in the
##                 string. Always matches the longest possible such string.
##                 Always succeeds, since it can match the ``nil`` string.
##
##   ``Pos(n)``    Where ``n`` is a natural number, matches the ``nil`` string
##                 if exactly ``n`` characters have been matched so far, and
##                 otherwise fails.
##
##   ``Rpos(n)``   Where ``n`` is a natural number, matches the ``nil`` string
##                 if exactly ``n`` characters remain to be matched, and
##                 otherwise fails.
##
##   ``Rtab(n)``   Where ``n`` is a natural number, matches characters from the
##                 current position until exactly ``n`` characters remain to be
##                 matched in the string. Fails if fewer than ``n`` unmatched
##                 characters remain in the string.
##
##   ``Tab(n)``    Where ``n`` is a natural number, matches characters from the
##                 current position until exactly ``n`` characters have been
##                 matched in all. Fails if more than ``n`` characters have
##                 already been matched.
##
##   ``Span(s)``   Where ``s`` is a string, matches a string of one or more
##                 characters that is among the characters given in the
##                 string. Always matches the longest possible such string.
##                 Fails if the current character is not one of the given set of
##                 characters.
##   ============= =============================================================
##
## Recursive Pattern Matching
## --------------------------
##
## The plus operator ``(+p)`` where ``p`` is a pattern variable, creates a
## recursive pattern that will, at pattern matching time, follow the pointer to
## obtain the referenced pattern, and then match this pattern. This may be used
## to construct recursive patterns. Consider for example::
##
## .. code-block:: nim
##   var p = ("a" or ("b" & (+p)))
##
## On the first attempt, this pattern attempts to match the string ``"a"``.  If
## this fails, then the alternative matches a ``"b"``, followed by an attempt to
## match ``p`` again. This second attempt first attempts to match ``"a"``, and
## so on. The result is a pattern that will match a string of b's followed by a
## single ``'a'``.
##
## This particular example could simply be written as ``NSpan('b') & 'a'``, but
## the use of recursive patterns in the general case can construct complex
## patterns which could not otherwise be built.
##
## Pattern Assignment Operations
## -----------------------------
##
## In addition to the overall result of a pattern match, which indicates success
## or failure, it is often useful to be able to keep track of the pieces of the
## subject string that are matched by individual pattern elements, or
## subsections of the pattern.
##
## The pattern assignment operators allow this capability. The first form is the
## immediate assignment::
##
## .. code-block:: nim
##    p * s
##
## Here ``p`` is an arbitrary pattern, and ``s`` is a variable of type string
## that will be set to the substring matched by p. This assignment happens
## during pattern matching, so if ``p`` matches more than once, then the
## assignment happens more than once.
##
## The deferred assignment operation::
##
## .. code-block:: nim
##   p ** s
##
## avoids these multiple assignments by deferring the assignment to the end of
## the match. If the entire match is successful, and if the pattern ``p`` was
## part of the successful match, then at the end of the matching operation the
## assignment to ``s`` of the string matching ``p`` is performed.
##
## The cursor assignment operation::
##
## .. code-block:: nim
##   Setcur(addr(n))
##
## assigns the current cursor position to the natural variable ``n``. The cursor
## position is defined as the count of characters that have been matched so far
## (including any start point moves).
##
## Finally the operations ``*`` and ``**`` may be used with values of type ``ptr
## File``. The effect is to do a putLine operation of the matched
## substring. These are particularly useful in debugging pattern matches.
##
## Deferred Matching
## -----------------
##
## The pattern construction functions (such as ``Len`` and ``Any``) all permit
## the use of pointers to natural or string values, or functions that return
## natural or string values. These forms cause the actual value to be obtained
## at pattern matching time. This allows interesting possibilities for
## constructing dynamic patterns as illustrated in the examples section.
##
## In addition the ``(+s)`` operator may be used where ``s`` is a pointer to
## string or function returning string, with a similar deferred effect.
##
## A special use of deferred matching is the construction of predicate
## functions. The element ``(+p)`` where ``p`` is an access to a function that
## returns a bool value, causes the function to be called at the time the
## element is matched. If the function returns true, then the ``nil`` string is
## matched, if the function returns false, then failure is signalled and
## previous alternatives are sought.
##
## Deferred Replacement
## --------------------
##
## The simple model given for pattern replacement (where the matched sub-string
## is replaced by the string given as the third argument to match) works fine in
## simple cases, but this approach does not work in the case where the
## expression used as the replacement string is dependent on values set by the
## match.
##
## For example, suppose we want to find an instance of a parenthesized
## character, and replace the parentheses with square brackets. At first glance
## it would seem that::
##
## .. code-block:: nim
##   match(subject, '(' & Len(1) * c & ')', '[' & c & ']')
##
## would do the trick, but that does not work, because the third argument to
## match gets evaluated too early, before the call to match, and before the
## pattern match has had a chance to set ``c``.
##
## To solve this problem we provide the deferred replacement capability.  With
## this approach, which of course is only needed if the pattern involved has
## side effects, is to do the match in two stages. The call to match sets a
## pattern result in a variable of the private type MatchResult, and then a
## subsequent ``Replace`` operation uses this ``MatchResult`` object to perform
## the required replacement.
##
## Using this approach, we can now write the above operation properly in a
## manner that will work::
##
## .. code-block:: nim
##   var m : MatchResult
##   ...
##   match(subject, '(' & Len(1) * c & ')', m)
##   Replace(m, '[' & c & ']')
##
## As with other match cases, there is a function and function form of this
## match call. A call to ``Replace`` after a failed match has no effect. Note
## that subject should not be modified between the calls.
##
## Examples of Pattern Matching
## ----------------------------
##
## First a simple example of the use of pattern replacement to remove a line
## number from the start of a string. We assume that the line number has the
## form of a string of decimal digits followed by a period, followed by one or
## more spaces::
##
## .. code-block:: nim
##    let digs = Span("0123456789")
##
##    let lNum = Pos(0) & digs & '.' & Span(' ')
##
## Now to use this pattern we simply do a match with a replacement::
##
## .. code-block:: nim
##    match(Line, Lnum, "")
##
## which replaces the line number by the ``nil`` string. Note that it is also
## possible to use a ``CharacterSet`` value as an argument to ``Span`` and
## similar functions, and in particular all the useful constants in are
## available. This means that we could define ``digs`` as::
##
## .. code-block:: nim
##    let digs = Span(digit)
##
## The style we use here, of defining constant patterns and then using them is
## typical. It is possible to build up patterns dynamically, but it is usually
## more efficient to build them in pieces in advance using constant
## declarations. Note in particular that although it is possible to construct a
## pattern directly as an argument for the match routine, it is much more
## efficient to preconstruct the pattern as we did in this example.
##
## Now let's look at the use of pattern assignment to break a string into
## sections. Suppose that the input string has two unsigned decimal integers,
## separated by spaces or a comma, with spaces allowed anywhere. Then we can
## isolate the two numbers with the following pattern::
##
## .. code-block:: nim
##    num1, num2 : string
##
##    let b = NSpan(' ')
##
##    let n = Span("0123456789")
##
##    let t = NSpan(' ') & n * num1 & Span(" ,") & n * num2
##
## The match operation ``match(" 124, 257 ", t)`` would assign the string
## ``124`` to ``num1`` and the string ``257`` to ``num2``.
##
## Now let's see how more complex elements can be built from the set of
## primitive elements. The following pattern matches strings that have the
## syntax of Ada 95 based literals::
##
## .. code-block:: nim
##    let digs  = Span(digit)
##    let uDigs = digs & Arbno('_' & digs)
##
##    let eDig  = Span(hexaDigit)
##    let uEdig = edig & Arbno('_' & edig)
##
##    let bNum = uDigs & '#' & uEdig & '#'
##
## A match against bNum will now match the desired strings, e.g.  it will match
## ``16#123_abc#``, but not ``a#b#``. However, this pattern is not quite
## complete, since it does not allow colons to replace the pound signs. The
## following is more complete::
##
## .. code-block:: nim
##    let bChar = Any("#:")
##    let bNum  = uDigs & bChar & uEdig & bChar
##
## but that is still not quite right, since it allows ``#`` and ``:`` to be
## mixed, and they are supposed to be used consistently. We solve this by using
## a deferred match::
##
## .. code-block:: nim
##    temp  : string
##
##    bNum  = Udigs & Bchar * temp & UEdig & (+Temp)
##
## Here the first instance of the base character is stored in ``temp``, and then
## later in the pattern we rematch the value that was assigned.
##
## For an example of a recursive pattern, let's define a pattern that is like
## the built in ``Bal``, but the string matched is balanced with respect to
## square brackets or curly brackets.
##
## The language for such strings might be defined in extended BNF as::
##
## .. code-block:: nim
##   ELEMENT ::= <any character other than [] or {}>
##               | '[' BALANCED_STRING ']'
##               | '{' BALANCED_STRING '}'
##
##   BALANCED_STRING ::= ELEMENT {ELEMENT}
##
## Here we use ``{}`` to indicate zero or more occurrences of a term, as is
## common practice in extended BNF. Now we can translate the above BNF into
## recursive patterns as follows::
##
## .. code-block:: nim
##   Element, Balanced_String : Pattern
##   .
##   .
##   .
##   Element = NotAny ("[]{}")
##               or
##             ('[' & (+Balanced_String) & ']')
##               or
##             ('{' & (+Balanced_String) & '}')
##
##   Balanced_String = Element & Arbno(Element)
##
## Note the important use of ``+`` here to refer to a pattern not yet
## defined. Note also that we use assignments precisely because we cannot refer
## to as yet undeclared variables in initializations.
##
## Now that this pattern is constructed, we can use it as though it were a new
## primitive pattern element, and for example, the match::
##
## .. code-block:: nim
##   match("xy[ab{cd}]", Balanced_String * Current_Output & Fail)
##
## will generate the output:
##
## .. code-block:: nim
##    x
##    xy
##    xy[ab{cd}]
##    y
##    y[ab{cd}]
##    [ab{cd}]
##    a
##    ab
##    ab{cd}
##    b
##    b{cd}
##    {cd}
##    c
##    cd
##    d
##
## Note that the function of the fail here is simply to force the pattern
## Balanced_String to match all possible alternatives. Studying the operation of
## this pattern in detail is highly instructive.
##
## Finally we give a rather elaborate example of the use of deferred
## matching. The following declarations build up a pattern which will find the
## longest string of decimal digits in the subject string
##
## .. code-block:: nim
##    var max, Cur: string
##    var loc : Natural
##
##    proc gts: bool =
##      return cur.len > max.len
##
##    let digs = Span(digit)
##
##    let find =
##      "" * max & Fence            & # initialize max to nil
##      BreakX(digit)               & # scan looking for digits
##      ((Span(digit)*cur           & # assign next string to cur
##       (+gts)                     & # check size(cur) > Size(max)
##       Setcur(addr(loc)))           # if so, save location
##                * max)            & # and assign to max
##      Fail()                        # seek all alternatives
##
## As we see from the comments here, complex patterns like this take on aspects
## of sequential programs. In fact they are sequential programs with general
## backtracking. In this pattern, we first use a pattern assignment that matches
## ``nil`` and assigns it to ``max``, so that it is initialized for the new
## match. Now ``BreakX`` scans to the next digit. Arb would do here, but
## ``BreakX`` will be more efficient.  Once we have found a digit, we scan out
## the longest string of digits with Span, and assign it to ``cur``. The
## deferred call to ``gts`` tests if the string we assigned to ``cur`` is the
## longest so far. If not, then failure is signalled, and we seek alternatives
## (this means that ``BreakX`` will extend and look for the next digit string).
## If the call to ``gts`` succeeds then the matched string is assigned as the
## largest string so far into ``max`` and its location is saved in
## ``loc``. Finally ``Fail`` forces the match to fail and seek alternatives, so
## that the entire string is searched.
##
## If the pattern ``find`` is matched against a string, the variable ``max`` at
## the end of the pattern will have the longest string of digits, and ``loc``
## will be the starting character location of the string. For example,
## ``match("ab123cd4657ef23", find)`` will assign ``"4657"`` to ``max`` and
## ``11`` to ``loc`` (indicating that the string ends with the eleventh
## character of the string).
##
## Note: the use of ``ptr`` to ``gts`` will not be needed if ``gts`` is defined
## at the outer level, but definitely will be necessary if ``gts`` is a nested
## function (in which case of course the scope of the pattern ``find`` will be
## restricted to this nested scope, and this cannot be checked, i.e. use of the
## pattern outside this scope is erroneous). Generally it is a good idea to
## define patterns and the functions they call at the outer level where
## possible, to avoid such problems.
##
## Correspondence with Pattern Matching in SPITBOL
## -----------------------------------------------
##
## Generally the Nim syntax and names correspond closely to SPITBOL syntax for
## pattern matching construction.
##
## The basic pattern construction operators are renamed as follows:
## =======  =====
## SPITBOL   Nim
## =======  =====
## (space)    &
##    |       or
##    $       *
##    .       **
## =======  =====
##
## The Nim operators were chosen so that the relative precedences of these
## operators corresponds to that of the SPITBOL operators, but as always, the
## use of parentheses is advisable to clarify.
##
## The actual pattern matching syntax is modified in Nim as follows:
## =======  ==============
## SPITBOL  Nim
## =======  ==============
## x y      match(x, y)
## x y = z  match(x, y, z)
## =======  ==============
##
## and pattern failure is indicated by returning a bool result from
## the match function (true for success, false for failure).
##
##
## Description of Algorithm and Data Structures
## ============================================
##
## A pattern structure is represented as a linked graph of nodes
## with the following structure::
##
##     +------------------------------------+
##     I                pCode               I
##     +------------------------------------+
##     I                index               I
##     +------------------------------------+
##     I                pThen               I
##     +------------------------------------+
##     I             parameter(s)           I
##     +------------------------------------+
##
##     ``pCode`` is a code value indicating the type of the pattern node. This
##     code is used both as the discriminant value for the record, and as the
##     case index in the main match routine that branches to the proper match
##     code for the given element.
##
##     ``index`` is a serial index number. The use of these serial index numbers
##     is described in a separate section.
##
##     ``pThen`` is a pointer to the successor node, i.e the node to be matched
##     if the attempt to match the node succeeds. If this is the last node of
##     the pattern to be matched, then Pthen points to a dummy node of kind
##     pcEOP (end of pattern), which initializes pattern exit.
##
##     The parameter or parameters are present for certain node types, and the
##     type varies with the pattern code.

import odarrays

{.push warning[SmallLshouldNotBeUsed]: off.}

type
  Character = char ## Element type
  String = string  ## Container of Element type
  CharArray[n: static[int]] = array[0..n-1, Character]
  CharacterSet = set[Character]

type
  PString* = String ## \
    ## This subtype is used in the remainder of the package to indicate a formal
    ## parameter that is converted to its corresponding pattern, i.e. a pattern
    ## that matches the characters of the string.

  PChar* = Character ## \
    ## Similarly, this subtype is used in the remainder of the package to
    ## indicate a formal parameter that is converted to its corresponding
    ## pattern, i.e. a pattern that matches this one character.

  BoolFunc* = proc(): bool ## General bool function type.
    ## When this type is used as a formal parameter type in this package, it
    ## indicates a deferred predicate pattern. The function will be called when
    ## the pattern element is matched and failure signalled if false is
    ## returned.

  NaturalFunc* = proc(): Natural ## General Natural function type.
    ## When this type is used as a formal parameter type in this package, it
    ## indicates a deferred pattern.  The function will be called when the
    ## pattern element is matched to obtain the currently referenced Natural
    ## value.

  StringFunc* = proc(): String ## General string function type.
    ## When this type is used as a formal parameter type in this package, it
    ## indicates a deferred pattern.  The function will be called when the
    ## pattern element is matched to obtain the currently referenced string
    ## value.

  PatternCode = enum
    pcArbY,
    pcAssign,
    pcBal,
    pcBreakXX,
    pcAbort,
    pcEOP,
    pcFail,
    pcFence,
    pcFenceX,
    pcFenceY,
    pcREnter,
    pcRRemove,
    pcRRemore,
    pcRem,
    pcSucceed,
    pcUnanchored,

    pcAlt,
    pcArbX,
    pcArbnoS,
    pcArbnoX,

    pcRpat,
    pcRpatP,

    pcPredFunc,

    pcAssignImm,
    pcAssignOnM,
    pcAnySR,
    pcBreakSR,
    pcBreakXSR,
    pcNotAnySR,
    pcNSpanSR,
    pcSpanSR,
    pcStringSR,

    pcAssignImmP,
    pcAssignOnMP,
    pcAnySP,
    pcBreakSP,
    pcBreakXSP,
    pcNotAnySP,
    pcNSpanSP,
    pcSpanSP,
    pcStringSP,

    pcWriteImm,
    pcWriteOnM,

    pcWriteImmP,
    pcWriteOnMP,

    pcNil,
    pcString,
    pcString2,
    pcString3,
    pcString4,
    pcString5,
    pcString6,

    pcSetcur,
    pcSetcurP,

    pcAnyCH,
    pcBreakCH,
    pcBreakXCH,
    pcChar,
    pcNotAnyCH,
    pcNSpanCH,
    pcSpanCH,

    pcAnyCS,
    pcBreakCS,
    pcBreakXCS,
    pcNotAnyCS,
    pcNSpanCS,
    pcSpanCS,

    pcArbnoY,
    pcLenNat,
    pcPosNat,
    pcRPosNat,
    pcRTabNat,
    pcTabNat,

    pcPosNF,
    pcLenNF,
    pcRPosNF,
    pcRTabNF,
    pcTabNF,

    pcPosNR,
    pcLenNR,
    pcRPosNR,
    pcRTabNR,
    pcTabNR,

    pcPosNP,
    pcLenNP,
    pcRPosNP,
    pcRTabNP,
    pcTabNP,

    pcAnySF,
    pcBreakSF,
    pcBreakXSF,
    pcNotAnySF,
    pcNSpanSF,
    pcSpanSF,
    pcStringSF

  PE = object
    case pCode: PatternCode
    of pcArbY..pcUnanchored: nil
    of pcAlt..pcArbnoX: alt: ref PE
    of pcRpat: patRef: ref Pattern
    of pcRpatP: patPtr: ptr Pattern
    of pcPredFunc: boolFunc: BoolFunc
    of pcAssignImm..pcStringSR: vr: ref String
    of pcAssignImmP..pcStringSP: vp: ptr String
    of pcWriteImm, pcWriteOnM: fileRef: ref File
    of pcWriteImmP, pcWriteOnMP: filePtr: ptr File
    of pcNil: nil
    of pcString: str: String
    of pcString2: str2: CharArray[2]
    of pcString3: str3: CharArray[3]
    of pcString4: str4: CharArray[4]
    of pcString5: str5: CharArray[5]
    of pcString6: str6: CharArray[6]
    of pcSetcur: val: ref Natural
    of pcSetcurP: valPtr: ptr Natural
    of pcAnyCH..pcSpanCH: elem: Character
    of pcAnyCS..pcSpanCS: es: CharacterSet
    of pcArbnoY..pcTabNat: nat: Natural
    of pcPosNF..pcTabNF: nf: NaturalFunc
    of pcPosNR..pcTabNR: nr: ref Natural
    of pcPosNP..pcTabNP: np: ptr Natural
    of pcAnySF..pcStringSF: vf: StringFunc
    index: Natural ## Serial index number of pattern element within pattern
    pThen: ref PE ## Successor element, to be matched after this one

  Pattern* = object
    ## Type representing a pattern.
    ## This package provides a complete set of operations for constructing
    ## patterns that can be used in the pattern matching operations provided.
    stk : Natural ## \
      ## Maximum number of stack entries required
      ## for matching this pattern. See description of pattern history stack
      ## in body.
    p : ref PE ## Pointer to initial pattern element for pattern


## Note: the data structures and general approach used in this implementation
## are derived from the original MINIMAL sources for SPITBOL. The code is not
## a direct translation, but the approach is followed closely. In particular,
## we use the one stack approach developed in the SPITBOL implementation.

# Nim IO functions
# ----------------

proc put(str: char) {.inline.} =
  write(stdout, str)

proc put(str: string) {.inline.} =
  write(stdout, str)

proc putLine(str: string) {.inline.} =
  writeLine(stdout, str)

proc newLine {.inline.} =
  write(stdout, "\n")


# Internal Debugging
# ------------------

const internalDebug = false ## \
  ## Set this flag to true to activate some built-in debugging traceback
  ## These are all lines output with debugPut and putLineD.

proc debugNewLine() {.inline.} =
  ## Output new blank line with newLine if ``internalDebug`` is true
  when internalDebug:
    newLine()

proc debugPut(str: string) {.inline.} =
  ## Output string with ``Put`` if ``internalDebug`` is true
  when internalDebug:
    put(str)

proc debugPutLine(str: string) {.inline.} =
  ## Output string with putLine if ``internalDebug`` is true
  when internalDebug:
    putLine(str)


## Pattern Matching Operations
## ---------------------------
##
## The match function performs an actual pattern matching operation.  The
## versions with three parameters perform a match without modifying the subject
## string and return a bool result indicating if the match is successful or
## not. The Anchor parameter is set to true to obtain an anchored match in which
## the pattern is required to match the first character of the string. In an
## unanchored match, which is
##
## the default, successive attempts are made to match the given pattern at each
## character of the subject string until a match succeeds, or until all
## possibilities have failed.
##
## Note that pattern assignment functions in the pattern may generate
## side effects, so these functions are not necessarily pure.

var anchoredMode : bool = false ## \
  ## This global variable can be set true to cause all subsequent pattern
  ## matches to operate in anchored mode. In anchored mode, no attempt is
  ## made to move the anchor point, so that if the match succeeds it must
  ## succeed starting at the first character. Note that the effect of
  ## anchored mode may be achieved in individual pattern matches by using
  ## Fence or Pos(0) at the start of the pattern.

const stackSize : Positive = 10 ## \
  ## Size used for internal pattern matching stack.

type MatchResult = object
  ## Type used to record result of pattern match
  res : ptr String ## \
    ## Pointer to subject string. Set to ``nil`` if match failed

  start : Natural ## \
    ## Starting index position of matched section of \
    ## subject string. Only valid if ``res`` is ``non-nil``.

  stop : Natural ## \
    ## Ending index position (1's origin) of matched section of \
    ## subject string. Only valid if ``res`` is ``non-nil``.



# Local Type Declarations
# -----------------------

const pcHasAlt = pcAlt..pcArbnoX ## \
  ## Range of pattern codes that has an Alt field. This is used in the
  ## recursive traversals, since these links must be followed.

proc newPE(pCode: PatternCode; index: Natural; pThen: ref PE): ref PE =
  new(result)
  result[] = PE(pCode: pCode, index: index, pThen: pThen)

let EOP = newPE(pcEOP, 0, nil) ## \
  ## This is the end of pattern element, and is thus the representation of
  ## a ``nil`` pattern. It has a zero index element since it is never placed
  ## inside a pattern. Furthermore it does not need a successor, since it
  ## marks the end of the pattern, so that no more successors are needed.

const okForSimpleArbno: array[PatternCode, bool] =
  [
    pcArbY: false,
    pcAssign: false,
    pcBal: false,
    pcBreakXX: false,
    pcAbort: false,
    pcEOP: false,
    pcFail: false,
    pcFence: false,
    pcFenceX: false,
    pcFenceY: false,
    pcREnter: false,
    pcRRemove: false,
    pcRRemore: false,
    pcRem: false,
    pcSucceed: false,
    pcUnanchored: false,

    pcAlt: false,
    pcArbX: false,
    pcArbnoS: false,
    pcArbnoX: false,

    pcRpat: false,
    pcRpatP: false,

    pcPredFunc: false,

    pcAssignImm: false,
    pcAssignOnM: false,
    pcAnySR: true,
    pcBreakSR: false,
    pcBreakXSR: false,
    pcNotAnySR: true,
    pcNSpanSR: false,
    pcSpanSR: true,
    pcStringSR: false,

    pcAssignImmP: false,
    pcAssignOnMP: false,
    pcAnySP: true,
    pcBreakSP: false,
    pcBreakXSP: false,
    pcNotAnySP: true,
    pcNSpanSP: false,
    pcSpanSP: true,
    pcStringSP: false,

    pcWriteImm: false,
    pcWriteOnM: false,

    pcWriteImmP: false,
    pcWriteOnMP: false,

    pcNil: false,
    pcString: true,
    pcString2: true,
    pcString3: true,
    pcString4: true,
    pcString5: true,
    pcString6: true,

    pcSetcur: false,
    pcSetcurP: false,

    pcAnyCH: true,
    pcBreakCH: false,
    pcBreakXCH: false,
    pcChar: true,
    pcNotAnyCH: true,
    pcNSpanCH: false,
    pcSpanCH: true,

    pcAnyCS: true,
    pcBreakCS: false,
    pcBreakXCS: false,
    pcNotAnyCS: true,
    pcNSpanCS: false,
    pcSpanCS: true,

    pcArbnoY: false,
    pcLenNat: true,
    pcPosNat: false,
    pcRPosNat: false,
    pcRTabNat: false,
    pcTabNat: false,

    pcPosNF: false,
    pcLenNF: false,
    pcRPosNF: false,
    pcRTabNF: false,
    pcTabNF: false,

    pcPosNR: false,
    pcLenNR: false,
    pcRPosNR: false,
    pcRTabNR: false,
    pcTabNR: false,

    pcPosNP: false,
    pcLenNP: false,
    pcRPosNP: false,
    pcRTabNP: false,
    pcTabNP: false,

    pcAnySF: true,
    pcBreakSF: false,
    pcBreakXSF: false,
    pcNotAnySF: true,
    pcNSpanSF: false,
    pcSpanSF: true,
    pcStringSF: false
  ] ## \
  ## This array is used to determine if a pattern used as an
  ## argument for Arbno is eligible for treatment using the simple Arbno
  ## structure (i.e. it is a pattern that is guaranteed to match at least
  ## one character on success, and not to make any entries on the stack.

type PtrArray = seq[ptr PE] ## \
  ## This type is used to build an array whose ``n``'th entry references the
  ## element in a pattern whose index value is ``n``. See buildPtrArray.


## The Pattern History Stack
## -------------------------
##
## The pattern history stack is used for controlling backtracking when a match
## fails. The idea is to stack entries that give a cursor value to be restored,
## and a node to be reestablished as the current node to attempt an appropriate
## rematch operation. The processing for a pattern element that has rematch
## alternatives pushes an appropriate entry or entry on to the stack, and the
## proceeds. If a match fails at any point, the top element of the stack is
## popped off, resetting the cursor and the match continues by accessing the
## node stored with this entry.

type StackEntry = object

  cursor: int ## \
    ## Saved cursor value that is restored when this entry is popped
    ## from the stack if a match attempt fails. Occasionally, this
    ## field is used to store a history stack pointer instead of a
    ## cursor.

  node: ptr PE ## \
    ## This pattern element reference is reestablished as the current
    ## node to be matched (which will attempt an appropriate rematch).

## Note: the pattern matching stack is used only to handle backtracking.  If no
## backtracking occurs, its entries are never accessed, and never popped off,
## and in particular it is normal for a successful match to terminate with
## entries on the stack that are simply discarded.
##
## Note: in subsequent diagrams of the stack, we always place element zero (the
## deepest element) at the top of the page, then build the stack down on the
## page with the most recent (top of stack) element being the bottom-most entry
## on the page.
##
## Stack checking is handled by labeling every pattern with the maximum number
## of stack entries that are required, so a single check at the start of
## matching the pattern suffices. There are two exceptions.
##
## First, the count does not include entries for recursive pattern
## references. Such recursions must therefore perform a specific stack check
## with respect to the number of stack entries required by the recursive pattern
## that is accessed and the amount of stack that remains unused.
##
## Second, the count includes only one iteration of an Arbno pattern, so a
## specific check must be made on subsequent iterations that there is still
## enough stack space left. The Arbno node has a field that records the number
## of stack entries required by its argument for this purpose.
##
## Use of Serial Index Field in Pattern Elements
## ---------------------------------------------
##
## The serial index numbers for the pattern elements are assigned as a pattern
## is constructed from its constituent elements. Note that there is never any
## sharing of pattern elements between patterns (copies are always made), so the
## serial index numbers are unique to a particular pattern as referenced from
## the ``p`` field of a value of type Pattern.
##
## The index numbers meet three separate invariants, which are used for various
## purposes as described in this section.
##
## First, the numbers uniquely identify the pattern elements within a
## pattern. If num is the number of elements in a given pattern, then the serial
## index numbers for the elements of this pattern will range from ``1 .. num``,
## so that each element has a separate value.
##
## The purpose of this assignment is to provide a convenient auxiliary data
## structure mechanism during operations which must traverse a pattern
## (e.g. copy and finalization processing). Once constructed patterns are
## strictly read only. This is necessary to allow sharing of patterns between
## tasks. This means that we cannot go marking the pattern (e.g. with a visited
## bit). Instead we construct a separate vector that contains the necessary
## information indexed by the index values in the pattern elements. For this
## purpose the only requirement is that they be uniquely assigned.
##
## Second, the pattern element referenced directly, i.e. the leading pattern
## element, is always the maximum numbered element and therefore indicates the
## total number of elements in the pattern. More precisely, the element
## referenced by the ``p`` field of a pattern value, or the element returned by
## any of the internal pattern construction routines in the body (that return a
## value of type ref PE) always is this maximum element,
##
## The purpose of this requirement is to allow an immediate determination of the
## number of pattern elements within a pattern. This is used to properly size
## the vectors used to contain auxiliary information for traversal as described
## above.
##
## Third, as compound pattern structures are constructed, the way in which
## constituent parts of the pattern are constructed is stylized. This is an
## automatic consequence of the way that these compound structures are
## constructed, and basically what we are doing is simply documenting and
## specifying the natural result of the pattern construction. The section
## describing compound pattern structures gives details of the numbering of each
## compound pattern structure.
##
## The purpose of specifying the stylized numbering structures for the compound
## patterns is to help simplify the processing in the Image function, since it
## eases the task of retrieving the original recursive structure of the pattern
## from the flat graph structure of elements.  This use in the Image function is
## the only point at which the code makes use of the stylized structures.
##
## Recursive Pattern Matches
## -------------------------
##
## The pattern primitive ``(+p)`` where ``p`` is a ``ptr Pattern`` or
## ``PatternFunc`` causes a recursive pattern match. This cannot be handled by
## an actual recursive call to the outer level match routine, since this would
## not allow for possible backtracking into the region matched by the inner
## pattern. Indeed this is the classical clash between recursion and
## backtracking, and a simple recursive stack structure does not suffice.
##
## This section describes how this recursion and the possible associated
## backtracking is handled. We still use a single stack, but we establish the
## concept of nested regions on this stack, each of which has a stack base value
## pointing to the deepest stack entry of the region. The base value for the
## outer level is zero.
##
## When a recursive match is established, two special stack entries are
## made. The first entry is used to save the original node that starts the
## recursive match. This is saved so that the successor field of this node is
## accessible at the end of the match, but it is never popped and executed.
##
## The second entry corresponds to a standard new region action. A pcRRemove
## node is stacked, whose cursor field is used to store the outer stack base,
## and the stack base is reset to point to this pcRRemove node. Then the
## recursive pattern is matched and it can make history stack entries in the
## normal matter, so now the stack looks like::
##
##    (stack entries made by outer level)
##
##    (Special entry, node is (+p) successor
##     cursor entry is not used)
##
##    (pcRRemove entry, "cursor" value is <-- Stack base saved base value for
##     the enclosing region)
##
##    (stack entries made by inner level)
##
## If a subsequent failure occurs and pops the pcRRemove node, it removes
## itself and the special entry immediately underneath it, restores the stack
## base value for the enclosing region, and then again signals failure to look
## for alternatives that were stacked before the recursion was initiated.
##
## Now we need to consider what happens if the inner pattern succeeds, as
## signalled by accessing the special pcEOP pattern primitive. First we
## recognize the nested case by looking at the Base value. If this Base value is
## Stack.first, then the entire match has succeeded, but if the base value is
## greater than ``stack.first``, then we have successfully matched an inner
## pattern, and processing continues at the outer level.
##
## There are two cases. The simple case is when the inner pattern has made no
## stack entries, as recognized by the fact that the current stack pointer is
## equal to the current base value. In this case it is fine to remove all trace
## of the recursion by restoring the outer base value and using the special
## entry to find the appropriate successor node.
##
## The more complex case arises when the inner match does make stack entries. In
## this case, the pcEOP processing stacks a special entry whose cursor value
## saves the saved inner base value (the one that references the corresponding
## pcRRemove value), and whose node pointer references a pcRRemore node, so
## the stack looks like::
##
##    (stack entries made by outer level)
##
##    (Special entry, node is (+p) successor,
##     cursor entry is not used)
##
##    (pcRRemove entry, "cursor" value is saved base value for the enclosing
##     region)
##
##    (stack entries made by inner level)
##
##    (pcRegionReplace entry, "cursor" value is stack pointer value referencing
##     the pcRRemove entry).
##
## If the entire match succeeds, then these stack entries are, as usual, ignored
## and abandoned. If on the other hand a subsequent failure causes the
## pcRegionReplace entry to be popped, it restores the inner base value from
## its saved "cursor" value and then fails again.  Note that it is OK that the
## cursor is temporarily clobbered by this pop, since the second failure will
## reestablish a proper cursor value.
##
## Compound Pattern Structures
## ---------------------------
##
## This section discusses the compound structures used to represent constructed
## patterns. It shows the graph structures of pattern elements that are
## constructed, and in the case of patterns that provide backtracking
## possibilities, describes how the history stack is used to control the
## backtracking. Finally, it notes the way in which the index numbers are
## assigned to the structure.
##
## In all diagrams, solid lines (built with minus signs or vertical bars,
## represent successor pointers (pThen fields) with > or V used to indicate the
## direction of the pointer. The initial node of the structure is in the upper
## left of the diagram. ``a`` dotted line is an alternative pointer from the
## element above it to the element below it. See individual sections for details
## on how alternatives are used.
##
##
## In the pattern structures listed in this section, a line that looks like
## ``---->`` with nothing to the right indicates an end of pattern (``EOP``)
## pointer that represents the end of the match.
##
## Concatenation
## -------------
##
## When a pattern concatenation ``(l & r)`` occurs, the resulting structure is
## obtained by finding all such ``EOP`` pointers in ``l``, and replacing them to
## point to ``r``. This is the most important flattening that occurs in
## constructing a pattern, and it means that the pattern matching circuitry does
## not have to keep track of the structure of a pattern with respect to
## concatenation, since the appropriate successor is always at hand.
##
## Concatenation itself generates no additional possibilities for backtracking,
## but the constituent patterns of the concatenated structure will make stack
## entries as usual. The maximum amount of stack required by the structure is
## thus simply the sum of the maximums required by ``l`` and ``r``.
##
## The index numbering of a concatenation structure works by leaving the
## numbering of the right hand pattern, ``r``, unchanged and adjusting the
## numbers in the left hand pattern, ``l`` up by the count of elements in
## r. This ensures that the maximum numbered element is the leading element as
## required (given that it was the leading element in ``l``).
##
## Concat
## ------
##
## Creates a pattern element that represents a concatenation of the two given
## pattern elements (i.e. the pattern ``l`` followed by ``r``).  The result
## returned is always the same as ``l``, but the pattern referenced by ``l`` is
## modified to have ``r`` as a successor. This proc does not copy ``l`` or
## ``r``, so if a copy is required, it is the responsibility of the caller. The
## ``Incr`` parameter is an amount to be added to the ``Nat`` field of any
## PArbnoY node that is in the left operand, it represents the additional
## stack space required by the right operand.
##
## Concat needs to traverse the left operand performing the following
## set of fixups:
##
##   a) Any successor pointers(``pThen`` fields) that are set to ``EOP`` are
##      reset to point to the second operand.
##
##   b) Any pcArbnoY node has its stack count field incremented
##      by the parameter Incr provided for this purpose.
##
##   d) ``num`` fields of all pattern elements in the left operand are
##      adjusted to include the elements of the right operand.
##
## Note: we do not use SetSuccessor in the processing for Concat, since
## there is no point in doing two traversals, we may as well do everything
## at the same time.
##
## Alternation
## -----------
##
## ``a`` pattern (l or r) constructs the structure::
##
##    +---+     +---+
##    | a |---->| l |---->
##    +---+     +---+
##      .
##      .
##    +---+
##    | r |---->
##    +---+
##
## The ``a`` element here is a pcAlt node, and the dotted line represents the
## contents of the alt field. When the pcAlt element is matched, it stacks a
## pointer to the leading element of ``r`` on the history stack so that on
## subsequent failure, a match of ``r`` is attempted.
##
## The ``a`` node is the highest numbered element in the pattern. The original
## index numbers of ``r`` are unchanged, but the index numbers of the ``l``
## pattern are adjusted up by the count of elements in ``r``.
##
## Note that the difference between the index of the ``l`` leading element the
## index of the ``r`` leading element (after building the alt structure)
## indicates the number of nodes in ``l``, and this is true even after the
## structure is incorporated into some larger structure.  For example, if the
## ``a`` node has index 16, and ``l`` has index 15 and ``r`` has index 5, then
## we know that ``l`` has 10 (15-5) elements in it.
##
## Suppose that we now concatenate this structure to another pattern
## with 9 elements in it. We will now have the ``a`` node with an index
## of 25, ``l`` with an index of 24 and ``r`` with an index of 14. We still
## know that ``l`` has 10 (24-14) elements in it, numbered 15-24, and
## consequently the successor of the alternation structure has an
## index with a value less than 15. This is used in Image to figure
## out the original recursive structure of a pattern.
##
## To clarify the interaction of the alternation and concatenation
## structures, here is a more complex example of the structure built
## for the pattern:
##
## .. code-block:: nim
##     (v or w or x) (y or z)
##
## where ``a,b,c,d,e`` are all single element patterns::
##
##   +---+     +---+       +---+     +---+
##   I a I---->I v I---+-->I a I---->I y I---->
##   +---+     +---+   I   +---+     +---+
##     .               I     .
##     .               I     .
##   +---+     +---+   I   +---+
##   I a I---->I w I-->I   I z I---->
##   +---+     +---+   I   +---+
##     .               I
##     .               I
##   +---+             I
##   I x I------------>+
##   +---+
##
## The numbering of the nodes would be as follows::
##
##   +---+     +---+       +---+     +---+
##   I 8 I---->I 7 I---+-->I 3 I---->I 2 I---->
##   +---+     +---+   I   +---+     +---+
##     .               I     .
##     .               I     .
##   +---+     +---+   I   +---+
##   I 6 I---->I 5 I-->I   I 1 I---->
##   +---+     +---+   I   +---+
##     .               I
##     .               I
##   +---+             I
##   I 4 I------------>+
##   +---+
##
## Note: The above structure actually corresponds to
##
## .. code-block:: nim
##   (a or (b or c)) (d or e)
##
## rather than
##
## .. code-block:: nim
##   ((a or b) or c) (d or e)
##
## which is the more natural interpretation, but in fact alternation
## is associative, and the construction of an alternative changes the
## left grouped pattern to the right grouped pattern in any case, so
## that the Image function produces a more natural looking output.
##
## Arb
## ---
## An Arb pattern builds the structure::
##
##   +---+
##   | x |---->
##   +---+
##     .
##     .
##   +---+
##   | y |---->
##   +---+
##
## The x node is a pcArbX node, which matches nil, and stacks a
## pointer to y node, which is the pcArbY node that matches one
## extra character and restacks itself.
##
## The pcArbX node is numbered 2, and the pcArbY node is 1
##
## Arbno (simple case)
## -------------------
## The simple form of Arbno can be used where the pattern always
## matches at least one character if it succeeds, and it is known
## not to make any history stack entries. In this case, Arbno(p)
## can construct the following structure::
##
##   |    +-------------+
##   |    |             ^
##   |    V             |
##   |  +---+           |
##   |  | s |---->      |
##   |  +---+           |
##   |    .             |
##   |    .             |
##   |  +---+           |
##   |  | p |---------->+
##   |  +---+
##
## The S (pcArbnoS) node matches ``nil`` stacking a pointer to the pattern
## ``p``. If a subsequent failure causes ``p`` to be matched and this match
## succeeds, then node ``A`` gets restacked to try another instance if needed by
## a subsequent failure.
##
## Note that we know that ``p`` cannot be ``EOP``, because a ``nil`` pattern
## does not meet the requirements for simple Arbno.
##
## Arbno (complex case)
## --------------------
## A call to Arbno (p), where ``p`` can match ``nil`` (or at least is not
## known to require a non-nil string) and/or ``p`` requires pattern stack
## entries, constructs the following structure::
##
##   |    +--------------------------+
##   |    |                          ^
##   |    V                          |
##   |  +---+                        |
##   |  | x |---->                   |
##   |  +---+                        |
##   |    .                          |
##   |    .                          |
##   |  +---+     +---+     +---+    |
##   |  | e |---->| p |---->| y |--->+
##   |  +---+     +---+     +---+
##
## The node ``x`` (pcArbnoX) matches ``nil``, stacking a pointer to the
## ``e-p-x`` structure used to match one Arbno instance.
##
## Here ``e`` is the pcREnter node which matches ``nil`` and creates two
## stack entries. The first is a special entry whose node field is
## not used at all, and whose cursor field has the initial cursor.
##
## The second entry corresponds to a standard new region action. A
## pcRRemove node is stacked, whose cursor field is used to store
## the outer stack base, and the stack base is reset to point to
## this pcRRemove node. Then the pattern ``p`` is matched, and it can
## make history stack entries in the normal manner, so now the stack
## looks like::
##
##    (stack entries made before assign pattern)
##
##    (Special entry, node field not used,
##     used only to save initial cursor)
##
##    (pcRRemove entry, "cursor" value is <-- Stack Base saved base value for
##     the enclosing region)
##
##    (stack entries made by matching p)
##
## If the match of ``p`` fails, then the pcRRemove entry is popped and
## it removes both itself and the special entry underneath it,
## restores the outer stack base, and signals failure.
##
## If the match of ``p`` succeeds, then node ``y``, the pcArbnoY node, pops
## the inner region. There are two possibilities. If matching ``p`` left
## no stack entries, then all traces of the inner region can be removed.
## If there are stack entries, then we push an pcRegionReplace stack
## entry whose "cursor" value is the inner stack base value, and then
## restore the outer stack base value, so the stack looks like::
##
##    (stack entries made before assign pattern)
##
##    (Special entry, node field not used,
##     used only to save initial cursor)
##
##    (pcRRemove entry, "cursor" value is saved base value for the enclosing
##     region)
##
##    (stack entries made by matching p)
##
##    (pcRegionReplace entry, "cursor" value is stack pointer value referencing
##     the pcRRemove entry).
##
## Now that we have matched another instance of the Arbno pattern,
## we need to move to the successor. There are two cases. If the
## Arbno pattern matched nil, then there is no point in seeking
## alternatives, since we would just match a whole bunch of nils.
## In this case we look through the alternative node, and move
## directly to its successor (i.e. the successor of the Arbno
## pattern). If on the other hand a non-nil string was matched,
## we simply follow the successor to the alternative node, which
## sets up for another possible match of the Arbno pattern.
##
## As noted in the section on stack checking, the stack count (and
## hence the stack check) for a pattern includes only one iteration
## of the Arbno pattern. To make sure that multiple iterations do not
## overflow the stack, the Arbno node saves the stack count required
## by a single iteration, and the Concat function increments this to
## include stack entries required by any successor. The pcArbnoY
## node uses this count to ensure that sufficient stack remains
## before proceeding after matching each new instance.
##
## The node numbering of the constituent pattern ``p`` is not affected.  Where
## ``n`` is the number of nodes in ``p``, the ``y`` node is numbered ``n + 1``,
## the ``e`` node is ``n + 2``, and the ``x`` node is ``n + 3``.
##
## Assign Immediate
## ----------------
##
## Immediate assignment ``(p * V)`` constructs the following structure::
##
##   +---+     +---+     +---+
##   | e |---->| p |---->| a |---->
##   +---+     +---+     +---+
##
## Here ``e`` is the pcREnter node which matches ``nil`` and creates two
## stack entries. The first is a special entry whose node field is
## not used at all, and whose cursor field has the initial cursor.
##
## The second entry corresponds to a standard new region action. A
## pcRRemove node is stacked, whose cursor field is used to store
## the outer stack base, and the stack base is reset to point to
## this pcRRemove node. Then the pattern ``p`` is matched, and it can
## make history stack entries in the normal manner, so now the stack
## looks like::
##
##    (stack entries made before assign pattern)
##
##    (Special entry, node field not used,
##     used only to save initial cursor)
##
##    (pcRRemove entry, "cursor" value is <-- Stack Base saved base value for
##     the enclosing region)
##
##    (stack entries made by matching p)
##
## If the match of ``p`` fails, then the pcRRemove entry is popped
## and it removes both itself and the special entry underneath it,
## restores the outer stack base, and signals failure.
##
## If the match of ``p`` succeeds, then node ``a``, which is the actual
## pcAssignImmP node, executes the assignment (using the stack
## base to locate the entry with the saved starting cursor value),
## and the pops the inner region. There are two possibilities, if
## matching ``p`` left no stack entries, then all traces of the inner
## region can be removed. If there are stack entries, then we push
## an pcRegionReplace stack entry whose "cursor" value is the
## inner stack base value, and then restore the outer stack base
## value, so the stack looks like::
##
##    (stack entries made before assign pattern)
##
##    (Special entry, node field not used,
##     used only to save initial cursor)
##
##    (pcRRemove entry, "cursor" value is saved base value for the enclosing
##     region)
##
##    (stack entries made by matching p)
##
##    (pcRegionReplace entry, "cursor" value is the stack pointer value
##     referencing the pcRRemove entry).
##
## If a subsequent failure occurs, the pcRegionReplace node restores
## the inner stack base value and signals failure to explore rematches
## of the pattern ``p``.
##
## The node numbering of the constituent pattern ``p`` is not affected.  Where
## ``n`` is the number of nodes in p, the ``a`` node is numbered ``n + 1``, and
## the e node is ``n + 2``.
##
## Assign On Match
## ---------------
## The assign on match (**) pattern is quite similar to the assign
## immediate pattern, except that the actual assignment has to be
## delayed. The following structure is constructed::
##
##   +---+     +---+     +---+
##   | e |---->| p |---->| a |---->
##   +---+     +---+     +---+
##
## The operation of this pattern is identical to that described above
## for deferred assignment, up to the point where ``p`` has been matched.
##
## The ``a`` node, which is the pcAssignOnMP node first pushes a
## pcAssign node onto the history stack. This node saves the ending
## cursor and acts as a flag for the final assignment, as further
## described below.
##
## It then stores a pointer to itself in the special entry node field.
## This was otherwise unused, and is now used to retrieve the address
## of the variable to be assigned at the end of the pattern.
##
## After that the inner region is terminated in the usual manner,
## by stacking a pcRRemore entry as described for the assign
## immediate case. Note that the optimization of completely
## removing the inner region does not happen in this case, since
## we have at least one stack entry (the pcAssign one we just made).
## The stack now looks like::
##
##    (stack entries made before assign pattern)
##
##    (Special entry, node points to copy of
##     the pcAssignOnMP node, and the
##     cursor field saves the initial cursor).
##
##    (pcRRemove entry, "cursor" value is saved base value for the enclosing
##     region)
##
##    (stack entries made by matching p)
##
##    (pcAssign entry, saves final cursor)
##
##    (pcRegionReplace entry, "cursor" value is stack pointer value referencing
##     the pcRRemove entry).
##
## If a subsequent failure causes the pcAssign node to execute it
## simply removes itself and propagates the failure.
##
## If the match succeeds, then the history stack is scanned for
## pcAssign nodes, and the assignments are executed (examination
## of the above diagram will show that all the necessary data is
## at hand for the assignment).
##
## To optimize the common case where no assign-on-match operations
## are present, a global flag AssignOnM is maintained which is
## initialize to False, and gets set true as part of the execution
## of the pcAssignOnMP node. The scan of the history stack for
## pcAssign entries is done only if this flag is set.
##
## The node numbering of the constituent pattern ``p`` is not affected.  Where
## ``n`` is the number of nodes in p, the ``a`` node is numbered ``n + 1``, and
## the e node is ``n + 2``.
##
## Bal
## ---
##
## Bal builds a single node::
##
##   +---+
##   | b |---->
##   +---+
##
## The node ``b`` is the pcBal node which matches a parentheses balanced
## string, starting at the current cursor position. It then updates
## the cursor past this matched string, and stacks a pointer to itself
## with this updated cursor value on the history stack, to extend the
## matched string on a subsequent failure.
##
## Since this is a single node it is numbered 1 (the reason we include
## it in the compound patterns section is that it backtracks).
##
## BreakX
## ------
##
## BreakX builds the structure::
##
##   +---+     +---+
##   | b |---->| a |---->
##   +---+     +---+
##     ^         .
##     |         .
##     |       +---+
##     +<------| x |
##             +---+
##
## Here the ``b`` node is the BreakXxx node that performs a normal Break
## function. The ``a`` node is an alternative (pcAlt) node that matches
## nil, but stacks a pointer to node x (the pcBreakXX node) which
## extends the match one character (to eat up the previously detected
## break character), and then rematches the break.
##
## The ``b`` node is numbered 3, the alternative node is 1, and the ``x``
## node is 2.
##
## Fence
## -----
##
## Fence builds a single node::
##
##   +---+
##   | f |---->
##   +---+
##
## The element ``f``, pcFence, matches ``nil``, and stacks a pointer to a
## pcAbort element which will abort the match on a subsequent failure.
##
## Since this is a single element it is numbered 1 (the reason we
## include it in the compound patterns section is that it backtracks).
##
## Fence Function
## --------------
##
## A call to the Fence function builds the structure::
##
##   +---+     +---+     +---+
##   | e |---->| p |---->| x |---->
##   +---+     +---+     +---+
##
## Here ``e`` is the pcREnter node which matches ``nil`` and creates two
## stack entries. The first is a special entry which is not used at
## all in the fence case (it is present merely for uniformity with
## other cases of region enter operations).
##
## The second entry corresponds to a standard new region action. A
## pcRRemove node is stacked, whose cursor field is used to store
## the outer stack base, and the stack base is reset to point to
## this pcRRemove node. Then the pattern ``p`` is matched, and it can
## make history stack entries in the normal manner, so now the stack
## looks like::
##
##    (stack entries made before fence pattern)
##
##    (Special entry, not used at all)
##
##    (pcRRemove entry, "cursor" value is <-- Stack Base saved base value for
##     the enclosing region)
##
##    (stack entries made by matching p)
##
## If the match of ``p`` fails, then the pcRRemove entry is popped
## and it removes both itself and the special entry underneath it,
## restores the outer stack base, and signals failure.
##
## If the match of ``p`` succeeds, then node ``x``, the pcFenceX node, gets
## control. One might be tempted to think that at this point, the
## history stack entries made by matching ``p`` can just be removed since
## they certainly are not going to be used for rematching (that is
## whole point of Fence after all). However, this is wrong, because
## it would result in the loss of possible assign-on-match entries
## for deferred pattern assignments.
##
## Instead what we do is to make a special entry whose node references
## pcFenceY, and whose cursor saves the inner stack base value, i.e.
## the pointer to the pcRRemove entry. Then the outer stack base
## pointer is restored, so the stack looks like::
##
##    (stack entries made before assign pattern)
##
##    (Special entry, not used at all)
##
##    (pcRRemove entry, "cursor" value is saved base value for the enclosing
##     region)
##
##    (stack entries made by matching p)
##
##    (pcFenceY entry, "cursor" value is stack pointer value referencing the
##     pcRRemove entry).
##
## If a subsequent failure occurs, then the pcFenceY entry removes
## the entire inner region, including all entries made by matching p,
## and alternatives prior to the Fence pattern are sought.
##
## The node numbering of the constituent pattern ``p`` is not affected.
## Where ``n`` is the number of nodes in p, the x node is numbered ``n + 1``,
## and the e node is ``n + 2``.
##
## Succeed
## -------
##
## Succeed builds a single node::
##
##   +---+
##   | s |---->
##   +---+
##
## The node S is the pcSucceed node which matches nil, and stacks
## a pointer to itself on the history stack, so that a subsequent
## failure repeats the same match.
##
## Since this is a single node it is numbered 1 (the reason we include
## it in the compound patterns section is that it backtracks).
##
## Write Immediate
## ---------------
##
## The structure built for a write immediate operation (``p * F``, where
## ``F`` is a file access value) is::
##
##   +---+     +---+     +---+
##   | e |---->| p |---->| w |---->
##   +---+     +---+     +---+
##
## Here ``e`` is the pcREnter node and ``w`` is the pcWriteImmP node. The
## handling is identical to that described above for Assign Immediate,
## except that at the point where a successful match occurs, the matched
## substring is written to the referenced file.
##
## The node numbering of the constituent pattern ``p`` is not affected.  Where
## ``n`` is the number of nodes in p, the ``w`` node is numbered ``n + 1``, and
## the ``e`` node is ``n + 2``.
##
## Write On Match
## --------------
##
## The structure built for a write on match operation (``p ** F``, where
## ``F`` is a file access value) is::
##
##   +---+     +---+     +---+
##   | e |---->| p |---->| w |---->
##   +---+     +---+     +---+
##
## Here ``e`` is the pcREnter node and ``W`` is the pcWriteOnMP node. The
## handling is identical to that described above for Assign On Match,
## except that at the point where a successful match has completed,
## the matched substring is written to the referenced file.
##
## The node numbering of the constituent pattern ``p`` is not affected.  Where
## ``n`` is the number of nodes in p, the ``w`` node is numbered ``n + 1``, and
## the ``e`` node is ``n + 2``.

# Constant Patterns
# -----------------
# The following pattern elements are referenced only from the pattern
# history stack. In each case the processing for the pattern element
# results in pattern match abort, or further failure, so there is no
# need for a successor and no need for a node number

var cpAssign   = PE(pCode: pcAssign,  index: 0, pThen: nil)
var cpAbort    = PE(pCode: pcAbort,   index: 0, pThen: nil)
var cpFenceY   = PE(pCode: pcFenceY,  index: 0, pThen: nil)
var cpRRemove  = PE(pCode: pcRRemove, index: 0, pThen: nil)
var cpRRemore  = PE(pCode: pcRRemore, index: 0, pThen: nil)

# Local Subprograms
# ---------------------

proc uninitializedPattern =
  ## Called to raise ProgramError with an appropriate error message if
  ## an uninitialized pattern is used in any pattern construction or
  ## pattern matching operation.
  raise newException(ValueError, "uninitialized value of type Nimbol.Pattern")

proc logicError =
  ## Called to raise ProgramError with an appropriate message if an
  ## internal logic error is detected.
  raise newException(ValueError, "Internal logic error in Nimbol")

proc peAddr(p: ref PE): ptr PE {.inline.} =
  addr(p[])

proc buildPtrArray(e: ref PE): PtrArray =
  ## Given a pattern element which is the leading element of a pattern
  ## structure, and a ``PtrArray`` with bounds ``1..e.index``, fills in the
  ## ``PtrArray`` so that its ``n``'th entry references the element of the
  ## referenced pattern whose index value is ``n``.
  proc RecordPE(e: ref PE; refs: var PtrArray) =
    ## Record given pattern element if not already recorded in ``ra``,
    ## and also record any referenced pattern elements recursively.
    debugPut("  RecordPE called with ref PE = " & $(cast[int](e)))
    if e == EOP or refs[e.index - 1] != nil:
      debugPutLine(", nothing to do")
      return
    else:
      debugPutLine(", recording index " & $e.index)
      refs[e.index - 1] = peAddr(e)
      RecordPE(e.pThen, refs)
      if e.pCode in pcHasAlt:
        RecordPE(e.alt, refs)
  # Start of processing for buildPtrArray
  result = newSeq[ptr PE](e.index)
  debugNewLine()
  debugPutLine("Entering buildPtrArray")
  RecordPE(e, result)
  debugNewLine()

proc newPE(
    pCode: PatternCode;
    index: Natural;
    pThen: ref PE;
    vr: ref String): ref PE =
  result = newPE(pCode, index, pThen)
  result.vr = vr

proc newPE(
    pCode: PatternCode;
    index: Natural;
    pThen: ref PE;
    vp: ptr String): ref PE =
  result = newPE(pCode, index, pThen)
  result.vp = vp

proc newPE(
    pCode: PatternCode;
    index: Natural;
    pThen: ref PE;
    fileRef: ref File): ref PE =
  result = newPE(pCode, index, pThen)
  result.fileRef = fileRef

proc newPE(
    pCode: PatternCode;
    index: Natural;
    pThen: ref PE;
    filePtr: ptr File): ref PE =
  result = newPE(pCode, index, pThen)
  result.filePtr = filePtr

proc newPE(
    pCode: PatternCode;
    index: Natural;
    pThen: ref PE;
    patPtr: ptr Pattern): ref PE =
  result = newPE(pCode, index, pThen)
  result.patPtr = patPtr

proc newPE(
    pCode: PatternCode;
    index: Natural;
    pThen: ref PE;
    patRef: ref Pattern): ref PE =
  result = newPE(pCode, index, pThen)
  result.patRef = patRef

proc newPE(
    pCode: PatternCode;
    index: Natural;
    pThen: ref PE;
    boolFunc: BoolFunc): ref PE =
  result = newPE(pCode, index, pThen)
  result.boolFunc = boolFunc

proc newPE(
    pCode: PatternCode;
    index: Natural;
    pThen: ref PE;
    vf: StringFunc): ref PE =
  result = newPE(pCode, index, pThen)
  result.vf = vf

proc newPE(
    pCode: PatternCode;
    index: Natural;
    pThen: ref PE;
    es: CharacterSet): ref PE =
  result = newPE(pCode, index, pThen)
  result.es = es

proc newPE(
    pCode: PatternCode;
    index: Natural;
    pThen: ref PE;
    elem: Character): ref PE =
  result = newPE(pCode, index, pThen)
  result.elem = elem

proc newPE(
    pCode: PatternCode;
    index: Natural;
    pThen: ref PE;
    alt: ref PE): ref PE =
  result = newPE(pCode, index, pThen)
  result.alt = alt

proc newPE(
    pCode: PatternCode;
    index: Natural;
    pThen: ref PE;
    nat: Natural): ref PE =
  result = newPE(pCode, index, pThen)
  result.nat = nat

proc newPE(
    pCode: PatternCode;
    index: Natural;
    pThen: ref PE;
    nf: NaturalFunc): ref PE =
  result = newPE(pCode, index, pThen)
  result.nf = nf

proc newPE(
    pCode: PatternCode;
    index: Natural;
    pThen: ref PE;
    str: PString): ref PE =
  result = newPE(pCode, index, pThen)
  case pCode
  of pcString2:
    for i, c in str:
      result.str2[i] = c
  of pcString3:
    for i, c in str:
      result.str3[i] = c
  of pcString4:
    for i, c in str:
      result.str4[i] = c
  of pcString5:
    for i, c in str:
      result.str5[i] = c
  of pcString6:
    for i, c in str:
      result.str6[i] = c
  of pcString:
    result.str = str
  else: discard

proc newPE(
    pCode: PatternCode;
    index: Natural;
    pThen: ref PE;
    nr: ref Natural): ref PE {.inline.} =
    result = newPE(pCode, index, pThen)
    result.nr = nr

proc newPE(
    pCode: PatternCode;
    index: Natural;
    pThen: ref PE;
    np: ptr Natural): ref PE {.inline.} =
    result = newPE(pCode, index, pThen)
    result.np = np

proc newPEVal(
    pCode: PatternCode;
    index: Natural;
    pThen: ref PE;
    val: ref Natural): ref PE {.inline.} =
    result = newPE(pCode, index, pThen)
    result.val = val

proc newPEValPtr(
    pCode: PatternCode;
    index: Natural;
    pThen: ref PE;
    valPtr: ptr Natural): ref PE {.inline.} =
    result = newPE(pCode, index, pThen)
    result.valPtr = valPtr

proc toPE(c: PChar): ref PE {.inline.} =
  ## Given a character, constructs a pattern element that matches
  ## the single character.
  newPE(pcChar, 1, EOP, c)

proc toPE(s: PString): ref PE =
  ## Given a string, constructs a pattern element that matches the string
  let len = s.len
  case len
  of 0:
    return newPE(pcNil, 1, EOP)
  of 1:
    return newPE(pcChar, 1, EOP, s[s.low()])
  of 2:
    return newPE(pcString2, 1, EOP, s)
  of 3:
    return newPE(pcString3, 1, EOP, s)
  of 4:
    return newPE(pcString4, 1, EOP, s)
  of 5:
    return newPE(pcString5, 1, EOP, s)
  of 6:
    return newPE(pcString6, 1, EOP, s)
  else:
    return newPE(pcString, 1, EOP, s)


proc copy(p: ref PE): ref PE =
  ## Creates a copy of the pattern element referenced by the given
  ## pattern element reference. This is a deep copy, which means that
  ## it follows the Next and Alt pointers.
  if p == nil:
     uninitializedPattern()
     return nil
  else:
    # References to elements in p, indexed by index field
    var refs = buildPtrArray(p)
    # Create copies of elements of p, indexed by index field
    var copies = newSeq[ref PE](p.index)
    for i, r in refs:
      new(copies[i])
      copies[i][] = r[]
    # Adjust all internal references
    for e in copies:
      # Adjust successor pointer to point to copy
      if e.pThen != EOP:
        e.pThen = copies[e.pThen.index - 1]
      # Adjust Alt pointer if there is one to point to copy
      if e.pCode in pcHasAlt and e.alt != EOP:
        e.alt = copies[e.alt.index - 1]
    return copies[p.index - 1]


proc pattern*(p: Pattern): Pattern {.inline.} =
  Pattern(stk: 0, p: copy(p.p))

proc pattern*(p: PString): Pattern {.inline.} =
  Pattern(stk: 0, p: toPE(p))

proc pattern*(c: PChar): Pattern {.inline.} =
  Pattern(stk: 0, p: toPE(c))

proc toSet(s: String): CharacterSet =
  for c in s:
    incl(result, c)


# These are debugging routines, which return a representation of the
# given access value (they are called only by Image and Dump)
# ------------------------------------------------------------------

proc `$`(s: openarray[Character]): string =
  result = newString(s.len)
  for i, c in s:
    result[i] = c

proc `$`(bf: BoolFunc): string =
  return "boolFunc(" & "BoolFunc" & ')'

proc `$`(fr: ref File): string =
  return "fileRef(" & $cast[int](fr) & ')'

proc `$`(fp: ptr File): string =
  return "filePtr(" & $cast[int](fp) & ')'

proc `$`(nf: NaturalFunc): string =
  return "NF(" & "NaturalFunc" & ')'

proc `$`(nr: ref Natural): string =
  return "NR(" & $cast[int](nr) & ')'

proc `$`(np: ptr Natural): string =
  return "NP(" & $cast[int](np) & ')'

proc `$`(pr: ref Pattern): string =
  return "patPtr(" & $cast[int](pr) & ')'

proc `$`(pp: ptr Pattern): string =
  return "patPtr(" & $cast[int](pp) & ')'

proc `$`(vf: StringFunc): string =
  return "SF(" & "StringFunc" & ')'

proc `$`(vr: ref String): string =
  return "SR(" & $cast[int](vr) & ')'

proc `$`(vp: ptr String): string =
  return "SP(" & $cast[int](vp) & ')'

proc setSucc(pat: ref PE; succ: ref PE) =
  ## Adjusts all ``EOP`` pointers in ``pat`` to point to ``succ``. No other
  ## changes are made. In particular, ``succ`` is unchanged, and no index
  ## numbers are modified. Note that ``pat`` may not be equal to ``EOP`` on
  ## entry.
  ##
  ## Note: this proc is not used by the normal concatenation circuit,
  ## since other fixups are required on the left operand in this case, and
  ## they might as well be done all together.
  if pat == nil:
    uninitializedPattern()
  elif pat == EOP:
    logicError()
  else:
    # We build a reference array for l whose n'th element points to
    # the pattern element of l whose original index value is N.
    var refs = buildPtrArray(pat)
    for p in refs:
      if p.pThen == EOP:
        p.pThen = succ
      if p.pCode in pcHasAlt and p.alt == EOP:
        p.alt = succ

proc Alternate(l, r: ref PE): ref PE =
  ## If the left pattern is nil, then we just add the alternation
  ## node with an index one greater than the right hand pattern.
  if l == EOP:
     return newPE(pcAlt, r.index + 1, EOP, r)
  # If the left pattern is non-nil, then build a reference vector
  # for its elements, and adjust their index values to accommodate
  # the right hand elements. Then add the alternation node.
  else:
    var refs = buildPtrArray(l)
    for p in refs:
      p.index.inc(r.index)
    return newPE(pcAlt, l.index + 1, l, r)

proc `or`(l, r: ref PE): ref PE {.inline.} =
  ## Build pattern structure corresponding to the alternation of ``l``, ``r``.
  ## (i.e. try to match ``l``, and if that fails, try to match ``r``).
  Alternate(l, r)

proc ArbnoSimple(p: ref PE): ref PE =
  ## Build simple Arbno pattern, ``p`` is a pattern that is guaranteed to match
  ## at least one character if it succeeds and to require no stack entries under
  ## all circumstances. The result returned is a simple Arbno structure as
  ## previously described.
  ##
  ##     +-------------+
  ##     |             ^
  ##     V             |
  ##   +---+           |
  ##   | s |---->      |
  ##   +---+           |
  ##     .             |
  ##     .             |
  ##   +---+           |
  ##   | p |---------->+
  ##   +---+
  ##
  ## The node numbering of the constituent pattern ``p`` is not affected.  The
  ## ``s`` node has a node number of ``p.index + 1``.
  ##
  ## Note that we know that ``p`` cannot be ``EOP``, because a ``nil`` pattern
  ## does not meet the requirements for simple Arbno.
  result = newPE(pcArbnoS, p.index + 1, EOP, p)
  setSucc(p, result)

proc Bracket(e, p, a: ref PE): ref PE =
  ## Given two single node pattern elements ``e`` and ``a``, and a (possible
  ## complex) pattern ``p``, construct the concatenation ``e-->p-->a`` and
  ## return a pointer to ``e``. The concatenation does not affect the node
  ## numbering in ``p``. ``a`` has a number one higher than the maximum number
  ## in ``p``, and ``e`` has a number two higher than the maximum number in
  ## ``p`` (see for example the AssignImmediate structure to understand a
  ## typical use of this function).
  if p == EOP:
     e.pThen = a
     e.index = 2
     a.index = 1
  else:
     e.pThen = p
     setSucc(p, a)
     e.index = p.index + 2
     a.index = p.index + 1
  return e

proc BreakXMake(b: ref PE): Pattern =
  ## Given a pattern element for a Break pattern, returns the
  ## corresponding BreakX compound pattern structure.
  ##
  ##   +---+     +---+
  ##   | b |---->| a |---->
  ##   +---+     +---+
  ##     ^         .
  ##     |         .
  ##     |       +---+
  ##     +<------| x |
  ##             +---+
  ##
  ## The ``b`` node is numbered 3, the alternative node is 1, and the ``x``
  ## node is 2.
  let x = newPE(pcBreakXX, 2, b)
  let a = newPE(pcAlt, 1, EOP, x)
  b.pThen = a
  Pattern(stk: 2, p: b)

proc Concat(l, r: ref PE; Incr: Natural): ref PE =
  ## Creates a pattern element that represents a concatenation of the two given
  ## pattern elements (i.e. the pattern ``l`` followed by ``r``).  The result
  ## returned is always the same as ``l``, but the pattern referenced by ``l``
  ## is modified to have ``r`` as a successor. This proc does not copy ``l`` or
  ## ``r``, so if a copy is required, it is the responsibility of the
  ## caller. The ``Incr`` parameter is an amount to be added to the ``Nat``
  ## field of any PArbnoY node that is in the left operand, it represents the
  ## additional stack space required by the right operand.
  ##
  ## Concat needs to traverse the left operand performing the following
  ## set of fixups:
  ##
  ##   a) Any successor pointers(``pThen`` fields) that are set to ``EOP`` are
  ##      reset to point to the second operand.
  ##
  ##   b) Any pcArbnoY node has its stack count field incremented by the
  ##      parameter Incr provided for this purpose.
  ##
  ##   d) ``num`` fields of all pattern elements in the left operand are
  ##      adjusted to include the elements of the right operand.
  ##
  ## Note: we do not use setSucc in the processing for Concat, since
  ## there is no point in doing two traversals, we may as well do everything
  ## at the same time.
  if l == EOP:
    return r
  elif r == EOP:
    return l
  else:
    # We build a reference array for l whose n'th element points to
    # the pattern element of l whose original index value is N.
    var refs = buildPtrArray(l)
    for p in refs:
       p.index.inc(r.index)
       if p.pCode == pcArbnoY:
          p.nat = p.nat + Incr
       if p.pThen == EOP:
          p.pThen = r
       if p.pCode in pcHasAlt and p.alt == EOP:
          p.alt = r
    return l


# Pattern concatenation. Matches l followed by r
# ----------------------------------------------
proc `&`*(l: PString; r: Pattern): Pattern {.inline.} =
  Pattern(stk: r.stk, p: Concat(toPE(l), copy(r.p), r.stk))

proc `&`*(l: Pattern; r: PString): Pattern {.inline.} =
  Pattern(stk: l.stk, p: Concat(copy(l.p), toPE(r), 0))

proc `&`*(l: PChar; r: Pattern): Pattern {.inline.} =
  Pattern(stk: r.stk, p: Concat(toPE(l), copy(r.p), r.stk))

proc `&`*(l: Pattern; r: PChar): Pattern {.inline.} =
  Pattern(stk: l.stk, p: Concat(copy(l.p), toPE(r), 0))

proc `&`*(l: Pattern; r: Pattern): Pattern {.inline.} =
  Pattern(stk: l.stk + r.stk, p: Concat(copy(l.p), copy(r.p), r.stk))

proc `&=`*(l: var Pattern; r: Pattern) {.inline.} =
  l.stk += r.stk
  l.p = Concat(l.p, copy(r.p), r.stk)


# Pattern Assignment Functions
# ----------------------------

proc `*`*(p: Pattern; val: ref String): Pattern =
  ## Matches ``p``, and if the match succeeds, assigns the matched substring to
  ## the given string variable ``val``. This assignment happens as soon as the
  ## substring is matched, and if the pattern ``p1`` is matched more than once
  ## during the course of the match, then the assignment will occur more than
  ## once.
  ##
  ## Assign immediate::
  ##
  ##   +---+     +---+     +---+
  ##   | e |---->| p |---->| a |---->
  ##   +---+     +---+     +---+
  ##
  ## The node numbering of the constituent pattern ``p`` is not affected.  Where
  ## ``n`` is the number of nodes in ``p``, the ``a`` node is numbered ``n +
  ## 1``, and the e node is ``n + 2``.
  let pat = copy(p.p)
  let e = newPE(pcREnter,    0, EOP)
  let a = newPE(pcAssignImm, 0, EOP, val)
  Pattern(stk: p.stk + 3, p: Bracket(e, pat, a))

proc `*`*(ps: PString; val: ref String): Pattern =
  let pat = toPE(ps)
  let e = newPE(pcREnter,    0, EOP)
  let a = newPE(pcAssignImm, 0, EOP, val)
  Pattern(stk: 3, p: Bracket(e, pat, a))

proc `*`*(pc: PChar; val: ref String): Pattern =
  let pat = toPE(pc)
  let e = newPE(pcREnter,    0, EOP)
  let a = newPE(pcAssignImm, 0, EOP, val)
  Pattern(stk: 3, p: Bracket(e, pat, a))

proc `*`*(p: Pattern; val: var String): Pattern =
  let pat = copy(p.p)
  let e = newPE(pcREnter,    0, EOP)
  let a = newPE(pcAssignImmP, 0, EOP, addr(val))
  Pattern(stk: p.stk + 3, p: Bracket(e, pat, a))

proc `*`*(ps: PString; val: var String): Pattern =
  let pat = toPE(ps)
  let e = newPE(pcREnter,    0, EOP)
  let a = newPE(pcAssignImmP, 0, EOP, addr(val))
  Pattern(stk: 3, p: Bracket(e, pat, a))

proc `*`*(pc: PChar; val: var String): Pattern =
  let pat = toPE(pc)
  let e = newPE(pcREnter,    0, EOP)
  let a = newPE(pcAssignImmP, 0, EOP, addr(val))
  Pattern(stk: 3, p: Bracket(e, pat, a))

proc `*`*(p: Pattern; file: ref File): Pattern =
  ## These are similar to the corresponding pattern assignment operations except
  ## that instead of setting the value of a variable, the matched substring is
  ## written to the appropriate file. This can be useful in following the
  ## progress of a match without generating the full amount of information
  ## obtained by setting debugMode to true.
  ##
  ## Write immediate::
  ##
  ##   +---+     +---+     +---+
  ##   | e |---->| p |---->| w |---->
  ##   +---+     +---+     +---+
  ##
  ## The node numbering of the constituent pattern ``p`` is not affected.  Where
  ## ``n`` is the number of nodes in ``p``, the ``w`` node is numbered ``n +
  ## 1``, and the ``e`` node is ``n + 2``.
  let pat = copy(p.p)
  let e = newPE(pcREnter,   0, EOP)
  let w = newPE(pcWriteImmP, 0, EOP, file)
  Pattern(stk: 3, p: Bracket(e, pat, w))

proc `*`*(ps: PString; file: ref File): Pattern =
  let pat = toPE(ps)
  let e = newPE(pcREnter,   0, EOP)
  let w = newPE(pcWriteImmP, 0, EOP, file)
  Pattern(stk: 3, p: Bracket(e, pat, w))

proc `*`*(pc: PChar; file: ref File): Pattern =
  let pat = toPE(pc)
  let e = newPE(pcREnter,   0, EOP)
  let w = newPE(pcWriteImmP, 0, EOP, file)
  Pattern(stk: 3, p: Bracket(e, pat, w))

proc `*`*(p: Pattern; file: var File): Pattern =
  let pat = copy(p.p)
  let e = newPE(pcREnter,   0, EOP)
  let w = newPE(pcWriteImmP, 0, EOP, addr(file))
  Pattern(stk: 3, p: Bracket(e, pat, w))

proc `*`*(ps: PString; file: var File): Pattern =
  let pat = toPE(ps)
  let e = newPE(pcREnter,   0, EOP)
  let w = newPE(pcWriteImmP, 0, EOP, addr(file))
  Pattern(stk: 3, p: Bracket(e, pat, w))

proc `*`*(pc: PChar; file: var File): Pattern =
  let pat = toPE(pc)
  let e = newPE(pcREnter,   0, EOP)
  let w = newPE(pcWriteImmP, 0, EOP, addr(file))
  Pattern(stk: 3, p: Bracket(e, pat, w))


# Pattern Assignment Functions
# ----------------------------

proc `**`*(p: Pattern; val: ref String): Pattern =
  ## Like "*" above, except that the assignment happens at most once after the
  ## entire match is completed successfully. If the match fails, then no
  ## assignment takes place.
  ##
  ## Assign on match::
  ##
  ##   +---+     +---+     +---+
  ##   | e |---->| p |---->| a |---->
  ##   +---+     +---+     +---+
  ##
  ## The node numbering of the constituent pattern ``p`` is not affected.  Where
  ## ``n`` is the number of nodes in ``p``, the ``a`` node is numbered ``n +
  ## 1``, and the ``e`` node is ``n + 2``.
  let pat = copy(p.p)
  let e = newPE(pcREnter,    0, EOP)
  let a = newPE(pcAssignOnM, 0, EOP, val)
  Pattern(stk: p.stk + 3, p: Bracket(e, pat, a))

proc `**`*(ps: PString; val: ref String): Pattern =
  let pat = toPE(ps)
  let e = newPE(pcREnter,    0, EOP)
  let a = newPE(pcAssignOnM, 0, EOP, val)
  Pattern(stk: 3, p: Bracket(e, pat, a))

proc `**`*(pc: PChar; val: ref String): Pattern =
  let pat = toPE(pc)
  let e = newPE(pcREnter,    0, EOP)
  let a = newPE(pcAssignOnM, 0, EOP, val)
  Pattern(stk: 3, p: Bracket(e, pat, a))

proc `**`*(p: Pattern; val: var String): Pattern =
  let pat = copy(p.p)
  let e = newPE(pcREnter,    0, EOP)
  let a = newPE(pcAssignOnMP, 0, EOP, addr(val))
  Pattern(stk: p.stk + 3, p: Bracket(e, pat, a))

proc `**`*(ps: PString; val: var String): Pattern =
  let pat = toPE(ps)
  let e = newPE(pcREnter,    0, EOP)
  let a = newPE(pcAssignOnMP, 0, EOP, addr(val))
  Pattern(stk: 3, p: Bracket(e, pat, a))

proc `**`*(pc: PChar; val: var String): Pattern =
  let pat = toPE(pc)
  let e = newPE(pcREnter,    0, EOP)
  let a = newPE(pcAssignOnMP, 0, EOP, addr(val))
  Pattern(stk: 3, p: Bracket(e, pat, a))


proc `**`*(p: Pattern; file: ref File): Pattern =
  ## Write on match::
  ##
  ##   +---+     +---+     +---+
  ##   | e |---->| p |---->| w |---->
  ##   +---+     +---+     +---+
  ##
  ## The node numbering of the constituent pattern ``p`` is not affected.  Where
  ## ``n`` is the number of nodes in ``p``, the ``w`` node is numbered ``n +
  ## 1``, and the ``e`` node is ``n + 2``.
  let pat = copy(p.p)
  let e = newPE(pcREnter,   0, EOP)
  let w = newPE(pcWriteOnM, 0, EOP, file)
  Pattern(stk: p.stk + 3, p: Bracket(e, pat, w))

proc `**`*(ps: PString; file: ref File): Pattern =
  let pat = toPE(ps)
  let e = newPE(pcREnter,   0, EOP)
  let w = newPE(pcWriteOnM, 0, EOP, file)
  Pattern(stk: 3, p: Bracket(e, pat, w))

proc `**`*(pc: PChar; file: ref File): Pattern =
  let pat = toPE(pc)
  let e = newPE(pcREnter,   0, EOP)
  let w = newPE(pcWriteOnM, 0, EOP, file)
  Pattern(stk: 3, p: Bracket(e, pat, w))

proc `**`*(p: Pattern; file: var File): Pattern =
  let pat = copy(p.p)
  let e = newPE(pcREnter,   0, EOP)
  let w = newPE(pcWriteOnMP, 0, EOP, addr(file))
  Pattern(stk: p.stk + 3, p: Bracket(e, pat, w))

proc `**`*(ps: PString; file: var File): Pattern =
  let pat = toPE(ps)
  let e = newPE(pcREnter,   0, EOP)
  let w = newPE(pcWriteOnMP, 0, EOP, addr(file))
  Pattern(stk: 3, p: Bracket(e, pat, w))

proc `**`*(pc: PChar; file: var File): Pattern =
  let pat = toPE(pc)
  let e = newPE(pcREnter,   0, EOP)
  let w = newPE(pcWriteOnMP, 0, EOP, addr(file))
  Pattern(stk: 3, p: Bracket(e, pat, w))


# Deferred Matching Operations
# ----------------------------

proc `+`*(str: ref String): Pattern {.inline.} =
  ## Here ``Str`` must be a string variable. This function constructs a pattern
  ## which at pattern matching time will access the current value of this
  ## variable, and match against these characters.
  Pattern(stk: 0, p: newPE(pcStringSR, 1, EOP, str))

proc `+`*(str: var String): Pattern {.inline.} =
  ## Here ``Str`` must be a string variable. This function constructs a pattern
  ## which at pattern matching time will access the current value of this
  ## variable, and match against these characters.
  Pattern(stk: 0, p: newPE(pcStringSP, 1, EOP, addr(str)))

proc `+`*(str: StringFunc): Pattern {.inline.} =
  ## Constructs a pattern which at pattern matching time calls the given
  ## function, and then matches against the string or character value
  ## that is returned by the call.
  Pattern(stk: 0, p: newPE(pcStringSF, 1, EOP, str))

proc `+`*(P: ref Pattern): Pattern {.inline.} =
  ## Here ``P`` must be a Pattern variable. This function constructs a pattern
  ## which at pattern matching time will access the current value of this
  ## variable, and match against the pattern value.
  Pattern(stk: 3, p: newPE(pcRpat, 1, EOP, P))

proc `+`*(P: var Pattern): Pattern {.inline.} =
  ## Here ``P`` must be a Pattern variable. This function constructs a pattern
  ## which at pattern matching time will access the current value of this
  ## variable, and match against the pattern value.
  Pattern(stk: 3, p: newPE(pcRpatP, 1, EOP, addr(P)))

proc `+`*(P: BoolFunc): Pattern {.inline.} =
  ## Constructs a predicate pattern function that at pattern matching time calls
  ## the given function. If true is returned, then the pattern matches.  If
  ## false is returned, then failure is signalled.
  Pattern(stk: 3, p: newPE(pcPredFunc, 1, EOP, P))


# Pattern alternation. Creates a pattern that will first try to match
# l and then on a subsequent failure, attempts to match r instead.
# -------------------------------------------------------------------
proc `or`*(l: PString; r: Pattern): Pattern {.inline.} =
  Pattern(stk: r.stk + 1, p: toPE(l) or copy(r.p))

proc `or`*(l: Pattern; r: PString): Pattern {.inline.} =
  Pattern(stk: l.stk + 1, p: copy(l.p) or toPE(r))

proc `or`*(l: PString; r: PString): Pattern {.inline.} =
  Pattern(stk: 1, p: toPE(l) or toPE(r))

proc `or`*(l: Pattern; r: Pattern): Pattern {.inline.} =
  Pattern(stk: max(l.stk, r.stk) + 1, p: copy(l.p) or copy(r.p))

proc `or`*(l: PChar;   r: Pattern): Pattern {.inline.} =
  Pattern(stk: 1, p: toPE(l) or copy(r.p))

proc `or`*(l: Pattern; r: PChar): Pattern {.inline.} =
  Pattern(stk: 1, p: copy(l.p) or toPE(r))

proc `or`*(l: PChar;   r: PChar): Pattern {.inline.} =
  Pattern(stk: 1, p: toPE(l) or toPE(r))

proc `or`*(l: PString; r: PChar): Pattern {.inline.} =
  Pattern(stk: 1, p: toPE(l) or toPE(r))

proc `or`*(l: PChar;   r: PString): Pattern {.inline.} =
  Pattern(stk: 1, p: toPE(l) or toPE(r))


proc `or=`*(l: var Pattern; r: Pattern): Pattern {.inline.} =
  l.stk = max(l.stk, r.stk) + 1
  l.p = copy(l.p) or copy(r.p)


# Any
# ---

proc Any*(str: String): Pattern {.inline.} =
  ## Constructs a pattern that matches a single character that is one of
  ## the characters in the given argument. The pattern fails if the current
  ## character is not in ``str``.
  Pattern(stk: 0, p: newPE(pcAnyCS, 1, EOP, toSet(str)))

proc Any*(str: Character): Pattern {.inline.} =
  Pattern(stk: 0, p: newPE(pcAnyCH, 1, EOP, str))

proc Any*(str: CharacterSet): Pattern {.inline.} =
  Pattern(stk: 0, p: newPE(pcAnyCS, 1, EOP, str))

proc Any*(str: ref String): Pattern {.inline.} =
  Pattern(stk: 0, p: newPE(pcAnySR, 1, EOP, str))

proc Any*(str: ptr String): Pattern {.inline.} =
  Pattern(stk: 0, p: newPE(pcAnySP, 1, EOP, str))

proc Any*(str: StringFunc): Pattern {.inline.} =
  Pattern(stk: 0, p: newPE(pcAnySF, 1, EOP, str))


# Arb
# ---

proc Arb*(): Pattern =
  ## Constructs a pattern that will match any string. On the first attempt,
  ## the pattern matches a ``nil`` string, then on each successive failure, it
  ## matches one more character, and only fails if matching the entire rest
  ## of the string.
  ## ::
  ##   +---+
  ##   | x |---->
  ##   +---+
  ##     .
  ##     .
  ##   +---+
  ##   | y |---->
  ##   +---+
  ##
  ## The pcArbX element is numbered 2, and the pcArbY element is 1
  let y = newPE(pcArbY, 1, EOP)
  let x = newPE(pcArbX, 2, EOP, y)
  Pattern(stk: 1, p: x)


# Arbno
# -----

proc Arbno*(ps: PString): Pattern =
  ## Pattern repetition. First matches ``nil``, then on a subsequent failure
  ## attempts to match an additional instance of the given pattern.  Equivalent
  ## to (but more efficient than) ``P & (""`` or ``(P & (""`` or ...
  if ps.len == 0:
     return Pattern(stk: 0, p: EOP)
  else:
     return Pattern(stk: 0, p: ArbnoSimple(toPE(ps)))

proc Arbno*(pc: PChar): Pattern {.inline.} =
  return Pattern(stk: 0, p: ArbnoSimple(toPE(pc)))

proc Arbno*(p: Pattern): Pattern =
  ## This is the complex case, either the pattern makes stack entries
  ## or it is possible for the pattern to match the ``nil`` string (more
  ## accurately, we don't know that this is not the case).
  ## ::
  ##   |    +--------------------------+
  ##   |    |                          ^
  ##   |    V                          |
  ##   |  +---+                        |
  ##   |  | x |---->                   |
  ##   |  +---+                        |
  ##   |    .                          |
  ##   |    .                          |
  ##   |  +---+     +---+     +---+    |
  ##   |  | e |---->| p |---->| y |--->+
  ##   |  +---+     +---+     +---+
  ##
  ## The node numbering of the constituent pattern ``p`` is not affected.
  ## Where ``n`` is the number of nodes in p, the y node is numbered ``n + 1``,
  ## the ``e`` node is ``n + 2``, and the ``x`` node is ``n + 3``.
  let pat = copy(p.p)
  if p.stk == 0 and okForSimpleArbno[pat.pCode]:
     return Pattern(stk: 0, p: ArbnoSimple(pat))
  let e = newPE(pcREnter, 0, EOP)
  let x = newPE(pcArbnoX, 0, EOP, e)
  let y = newPE(pcArbnoY, 0, x, p.stk + 3)
  let epy = Bracket(e, pat, y)
  x.alt = epy
  x.index = epy.index + 1
  return Pattern(stk: p.stk + 3, p: x)


# Bal
# ---

proc Bal*(): Pattern {.inline.} =
  ## Constructs a pattern that will match any non-empty string that is
  ## parentheses balanced with respect to the normal parentheses characters.
  ## Attempts to extend the string if a subsequent failure occurs.
  return Pattern(stk: 1, p: newPE(pcBal, 1, EOP))


# Break
# -----

proc Break*(str: String): Pattern {.inline.} =
  ## Constructs a pattern that matches a (possibly ``nil``) string which
  ## is immediately followed by a character in the given argument. This
  ## character is not part of the matched string. The pattern fails if
  ## the remaining characters to be matched do not include any of the
  ## characters in Str.
  return Pattern(stk: 0, p: newPE(pcBreakCS, 1, EOP, toSet(str)))

proc Break*(str: Character): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcBreakCH, 1, EOP, str))

proc Break*(str: CharacterSet): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcBreakCS, 1, EOP, str))

proc Break*(str: ref String): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcBreakSR, 1, EOP, str))

proc Break*(str: ptr String): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcBreakSP, 1, EOP, str))

proc Break*(str: StringFunc): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcBreakSF, 1, EOP, str))


# BreakX
# ------

proc BreakX*(str: String): Pattern {.inline.} =
  ## Like Break, but the pattern attempts to extend on a failure to find the
  ## next occurrence of a character in ``Str``, and only fails when the last
  ## such instance causes a failure.
  BreakXMake(newPE(pcBreakXCS, 3, nil, toSet(str)))

proc BreakX*(str: Character): Pattern {.inline.} =
  BreakXMake(newPE(pcBreakXCH, 3, nil, str))

proc BreakX*(str: CharacterSet): Pattern {.inline.} =
  BreakXMake(newPE(pcBreakXCS, 3, nil, str))

proc BreakX*(str: ref String): Pattern {.inline.} =
  BreakXMake(newPE(pcBreakXSR, 3, nil, str))

proc BreakX*(str: ptr String): Pattern {.inline.} =
  BreakXMake(newPE(pcBreakXSP, 3, nil, str))

proc BreakX*(str: StringFunc): Pattern {.inline.} =
  BreakXMake(newPE(pcBreakXSF, 3, nil, str))


# Abort
# ------

proc Abort*(): Pattern {.inline.} =
  ## Constructs a pattern that immediately aborts the entire match
  return Pattern(stk: 0, p: newPE(pcAbort, 1, EOP))


# Fail
# ----

proc Fail*(): Pattern {.inline.} =
  ## Constructs a pattern that always fails
  return Pattern(stk: 0, p: newPE(pcFail, 1, EOP))


# Fence
# -----

proc Fence*(): Pattern {.inline.} =
  ## Constructs a pattern that matches ``nil`` on the first attempt, and then
  ## causes the entire match to be aborted if a subsequent failure occurs.
  return Pattern(stk: 1, p: newPE(pcFence, 1, EOP))

proc Fence*(p: Pattern): Pattern =
  ## Constructs a pattern that first matches ``p``. If ``p`` fails, then the
  ## constructed pattern fails. If ``p`` succeeds, then the match proceeds, but
  ## if subsequent failure occurs, alternatives in ``p`` are not sought.  The
  ## idea of Fence is that each time the pattern is matched, just one attempt is
  ## made to match ``p``, without trying alternatives.
  ## ::
  ##   +---+     +---+     +---+
  ##   | e |---->| p |---->| x |---->
  ##   +---+     +---+     +---+
  ##
  ## The node numbering of the constituent pattern ``p`` is not affected.  Where
  ## ``n`` is the number of nodes in ``p``, the ``x`` node is numbered ``n +
  ## 1``, and the ``e`` node is ``n + 2``.
  let pat = copy(p.p)
  let e = newPE(pcREnter, 0, EOP)
  let x = newPE(pcFenceX, 0, EOP)
  return Pattern(stk: p.stk + 1, p: Bracket(e, pat, x))


# Len
# ---

proc Len*(Count: Natural): Pattern {.inline.} =
  ## Constructs a pattern that matches exactly the given number of
  ## characters. The pattern fails if fewer than this number of characters
  ## remain to be matched in the string.
  ##
  ## Note, the following is not just an optimization, it is needed to ensure
  ## that Arbno(Len(0)) does not generate an infinite matching loop(since
  ## pcLenNat is okForSimpleArbno).
  if Count == 0:
    return Pattern(stk: 0, p: newPE(pcNil, 1, EOP))
  else:
    return Pattern(stk: 0, p: newPE(pcLenNat, 1, EOP, Count))

proc Len*(Count: NaturalFunc): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcLenNF, 1, EOP, Count))

proc Len*(Count: ref Natural): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcLenNR, 1, EOP, Count))

proc Len*(Count: ptr Natural): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcLenNP, 1, EOP, Count))


# NotAny
# ------

proc NotAny*(str: String): Pattern {.inline.} =
  ## Constructs a pattern that matches a single character that is not one of the
  ## characters in the given argument. The pattern Fails if the current
  ## character is in ``Str``.
  return Pattern(stk: 0, p: newPE(pcNotAnyCS, 1, EOP, toSet(str)))

proc NotAny*(str: Character): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcNotAnyCH, 1, EOP, str))

proc NotAny*(str: CharacterSet): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcNotAnyCS, 1, EOP, str))

proc NotAny*(str: ref String): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcNotAnySR, 1, EOP, str))

proc NotAny*(str: ptr String): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcNotAnySP, 1, EOP, str))

proc NotAny*(str: StringFunc): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcNotAnySF, 1, EOP, str))

# NSpan
# -----

proc NSpan*(str: String): Pattern {.inline.} =
  ## Constructs a pattern that matches the longest possible string consisting
  ## entirely of characters from the given argument. The string may be empty, so
  ## this pattern always succeeds.
  return Pattern(stk: 0, p: newPE(pcNSpanCS, 1, EOP, toSet(str)))

proc NSpan*(str: Character): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcNSpanCH, 1, EOP, str))

proc NSpan*(str: CharacterSet): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcNSpanCS, 1, EOP, str))

proc NSpan*(str: ref String): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcNSpanSR, 1, EOP, str))

proc NSpan*(str: ptr String): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcNSpanSP, 1, EOP, str))

proc NSpan*(str: StringFunc): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcNSpanSF, 1, EOP, str))

# Pos
# ---

proc Pos*(Count: Natural): Pattern {.inline.} =
  ## Constructs a pattern that matches the ``nil`` string if exactly ``Count``
  ## characters have already been matched, and otherwise fails.
  return Pattern(stk: 0, p: newPE(pcPosNat, 1, EOP, Count))

proc Pos*(Count: NaturalFunc): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcPosNF, 1, EOP, Count))

proc Pos*(Count: ref Natural): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcPosNR, 1, EOP, Count))

proc Pos*(Count: ptr Natural): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcPosNP, 1, EOP, Count))

# Replace
# -------

proc Replace*(result: var MatchResult; Replace: String) {.inline.} =
  ## Given a previous call to match which set result, performs a pattern
  ## replacement if the match was successful. Has no effect if the match
  ## failed. This call should immediately follow the match call.
  if result.res != nil:
    result.res[][result.start..result.stop] = Replace
    #result.res = nil


# Rem
# ----

proc Rem*: Pattern {.inline.} =
  ## Constructs a pattern that always succeeds, matching the remaining unmatched
  ## characters in the pattern.
  return Pattern(stk: 0, p: newPE(pcRem, 1, EOP))


# Rpos
# ----

proc Rpos*(Count: Natural): Pattern {.inline.} =
  ## Constructs a pattern that matches the ``nil`` string if exactly ``Count``
  ## characters remain to be matched in the string, and otherwise fails.
  return Pattern(stk: 0, p: newPE(pcRPosNat, 1, EOP, Count))

proc Rpos*(Count: NaturalFunc): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcRPosNF, 1, EOP, Count))

proc Rpos*(Count: ref Natural): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcRPosNR, 1, EOP, Count))

proc Rpos*(Count: ptr Natural): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcRPosNP, 1, EOP, Count))


# Rtab
# ----

proc Rtab*(Count: Natural): Pattern {.inline.} =
  ## Constructs a pattern that matches from the current location until exactly
  ## Count characters remain to be matched in the string. The pattern fails if
  ## fewer than Count characters remain to be matched.
  return Pattern(stk: 0, p: newPE(pcRTabNat, 1, EOP, Count))

proc Rtab*(Count: NaturalFunc): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcRTabNF, 1, EOP, Count))

proc Rtab*(Count: ref Natural): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcRTabNR, 1, EOP, Count))

proc Rtab*(Count: ptr Natural): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcRTabNP, 1, EOP, Count))


# Setcur
# ------

proc Setcur*(val: ref Natural): Pattern {.inline.} =
  ## Constructs a pattern that matches the ``nil`` string, and assigns the
  ## current cursor position in the string. This value is the number of
  ## characters matched so far. So it is zero at the start of the match.
  return Pattern(stk: 0, p: newPEVal(pcSetcur, 1, EOP, val))

proc Setcur*(val: var Natural): Pattern {.inline.} =
  ## Constructs a pattern that matches the ``nil`` string, and assigns the
  ## current cursor position in the string. This value is the number of
  ## characters matched so far. So it is zero at the start of the match.
  return Pattern(stk: 0, p: newPEValPtr(pcSetcurP, 1, EOP, addr(val)))


# Span
# ----

proc Span*(str: String): Pattern {.inline.} =
  ## Constructs a pattern that matches the longest possible string consisting
  ## entirely of characters from the given argument. The string cannot be empty
  ## , so the pattern fails if the current character is not one of the
  ## characters in ``Str``.
  return Pattern(stk: 0, p: newPE(pcSpanCS, 1, EOP, toSet(str)))

proc Span*(str: Character): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcSpanCH, 1, EOP, str))

proc Span*(str: CharacterSet): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcSpanCS, 1, EOP, str))

proc Span*(str: ref String): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcSpanSR, 1, EOP, str))

proc Span*(str: ptr String): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcSpanSP, 1, EOP, str))

proc Span*(str: StringFunc): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcSpanSF, 1, EOP, str))


# Succeed
# -------

proc Succeed*(): Pattern {.inline.} =
  ## Constructs a pattern that succeeds matching nil, both on the first
  ## attempt, and on any rematch attempt, i.e. it is equivalent to an
  ## infinite alternation of ``nil`` strings.
  return Pattern(stk: 1, p: newPE(pcSucceed, 1, EOP))


# Tab
# ---

proc Tab*(Count: Natural): Pattern {.inline.} =
  ## Constructs a pattern that from the current location until ``Count``
  ## characters have been matched. The pattern fails if more than ``Count``
  ## characters have already been matched.
  return Pattern(stk: 0, p: newPE(pcTabNat, 1, EOP, Count))

proc Tab*(Count: NaturalFunc): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcTabNF, 1, EOP, Count))

proc Tab*(Count: ref Natural): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcTabNR, 1, EOP, Count))

proc Tab*(Count: ptr Natural): Pattern {.inline.} =
  return Pattern(stk: 0, p: newPE(pcTabNP, 1, EOP, Count))


# Image
# -----

proc deleteAmpersand(result: var String) =
   let l = result.len
   if l > 2:
     result.setLen(l-1)

proc imageSeq(
    result: var string;
    e: ref PE;
    Succ: ptr PE;
    refs: var PtrArray;
    killAmpersand: var bool;
    Paren: bool)

proc imageSeq(
    result: var string;
    e: ref PE;
    Succ: ref PE;
    refs: var PtrArray;
    killAmpersand: var bool;
    Paren: bool) =
  imageSeq(result, e, peAddr(Succ), refs, killAmpersand, Paren)

proc imageOne(
    result: var string;
    e: var ref PE;
    refs: var PtrArray;
    killAmpersand: var bool) =
  ## ``e`` refers to a pattern structure. This proc appends to ``result`` a
  ## representation of the single simple or compound pattern structure at the
  ## start of ``e`` and updates ``e`` to point to its successor.

  var er = e.pThen
  # Successor set as result in ``e`` unless reset

  case e.pCode

  of pcAbort:
    result.add("Abort")

  of pcAlt:
    let elmtsInL  = e.pThen.index - e.alt.index
    # Number of elements in left pattern of alternation

    let lowestInL = e.index - elmtsInL
    # Number of lowest index in elements of left pattern

    var e1: ref PE
    # The successor of the alternation node must have a lower
    # index than any node that is in the left pattern or a
    # higher index than the alternation node itself.

    while er != EOP and er.index >= lowestInL and er.index < e.index:
      er = er.pThen

    result.add('(')

    e1 = e
    while true:
      imageSeq(result, e1.pThen, er, refs, killAmpersand, false)
      result.add(" or ")
      e1 = e1.alt
      if e1.pCode != pcAlt: break

    imageSeq(result, e1, er, refs, killAmpersand, false)
    result.add(')')

  of pcAnyCS:
    result.add("Any(" & $e.es & ')')

  of pcAnySF:
    result.add("Any(" & $e.vf & ')')

  of pcAnySR:
    result.add("Any(" & $e.vr & ')')

  of pcAnySP:
    result.add("Any(" & $e.vp & ')')

  of pcArbX:
    result.add("Arb")

  of pcArbnoS:
    result.add("Arbno(")
    imageSeq(result, e.alt, e, refs, killAmpersand, false)
    result.add(')')

  of pcArbnoX:
    result.add("Arbno(")
    imageSeq(result, e.alt.pThen, refs[e.index - 3], refs, killAmpersand, false)
    result.add(')')

  of pcAssignImm:
    deleteAmpersand(result)
    result.add("* " & $(refs[e.index - 1].vr))

  of pcAssignImmP:
    deleteAmpersand(result)
    result.add("* " & $(refs[e.index - 1].vp))

  of pcAssignOnM:
    deleteAmpersand(result)
    result.add("** " & $(refs[e.index - 1].vr))

  of pcAssignOnMP:
    deleteAmpersand(result)
    result.add("** " & $(refs[e.index - 1].vp))

  of pcAnyCH:
    result.add("Any('" & e.elem & "')")

  of pcBal:
    result.add("Bal")

  of pcBreakCH:
    result.add("Break('" & e.elem & "')")

  of pcBreakCS:
    result.add("Break(" & $e.es & ')')

  of pcBreakSF:
    result.add("Break(" & $(e.vf) & ')')

  of pcBreakSR:
    result.add("Break(" & $(e.vr) & ')')

  of pcBreakSP:
    result.add("Break(" & $(e.vp) & ')')

  of pcBreakXCH:
    result.add("BreakX('" & e.elem & "')")
    er = er.pThen

  of pcBreakXCS:
    result.add("BreakX(" & $e.es & ')')
    er = er.pThen

  of pcBreakXSF:
    result.add("BreakX(" & $(e.vf) & ')')
    er = er.pThen

  of pcBreakXSR:
    result.add("BreakX(" & $(e.vr) & ')')
    er = er.pThen

  of pcBreakXSP:
    result.add("BreakX(" & $(e.vp) & ')')
    er = er.pThen

  of pcChar:
    result.add('\'' & e.elem & '\'')

  of pcFail:
    result.add("Fail")

  of pcFence:
    result.add("Fence")

  of pcFenceX:
    result.add("Fence(")
    imageSeq(result, e.alt.pThen, refs[e.index - 3], refs, killAmpersand, false)
    result.add(')')

  of pcLenNat:
    result.add("Len(" & $e.nat & ')')

  of pcLenNF:
    result.add("Len(" & $e.nf & ')')

  of pcLenNR:
    result.add("Len(" & $e.nr & ')')

  of pcLenNP:
    result.add("Len(" & $e.np & ')')

  of pcNotAnyCH:
    result.add("NotAny('" & e.elem & "')")

  of pcNotAnyCS:
    result.add("NotAny(" & $e.es & ')')

  of pcNotAnySF:
    result.add("NotAny(" & $e.vf & ')')

  of pcNotAnySR:
    result.add("NotAny(" & $e.vr & ')')

  of pcNotAnySP:
    result.add("NotAny(" & $e.vp & ')')

  of pcNSpanCH:
    result.add("NSpan('" & e.elem & "')")

  of pcNSpanCS:
    result.add("NSpan(" & $e.es & ')')

  of pcNSpanSF:
    result.add("NSpan(" & $e.vf & ')')

  of pcNSpanSR:
    result.add("NSpan(" & $e.vr & ')')

  of pcNSpanSP:
    result.add("NSpan(" & $e.vp & ')')

  of pcNil:
    result.add("Nil")

  of pcPosNat:
    result.add("Pos(" & $e.nat & ')')

  of pcPosNF:
    result.add("Pos(" & $e.nf & ')')

  of pcPosNR:
    result.add("Pos(" & $e.nr & ')')

  of pcPosNP:
    result.add("Pos(" & $e.np & ')')

  of pcREnter:
    killAmpersand = true

  of pcRem:
    result.add("Rem")

  of pcRpat:
    result.add("(+ " & $e.patRef & ')')

  of pcRpatP:
    result.add("(+ " & $e.patPtr & ')')

  of pcPredFunc:
    result.add("(+ " & $e.boolFunc & ')')

  of pcRPosNat:
    result.add("RPos(" & $e.nat & ')')

  of pcRPosNF:
    result.add("RPos(" & $e.nf & ')')

  of pcRPosNR:
    result.add("RPos(" & $e.nr & ')')

  of pcRPosNP:
    result.add("RPos(" & $e.np & ')')

  of pcRTabNat:
    result.add("RTab(" & $e.nat & ')')

  of pcRTabNF:
    result.add("RTab(" & $e.nf & ')')

  of pcRTabNR:
    result.add("RTab(" & $e.nr & ')')

  of pcRTabNP:
    result.add("RTab(" & $e.np & ')')

  of pcSetcur:
    result.add("Setcur(" & $e.val & ')')

  of pcSetcurP:
    result.add("Setcur(" & $e.valPtr & ')')

  of pcSpanCH:
    result.add("Span('" & e.elem & "')")

  of pcSpanCS:
    result.add("Span(" & $e.es & ')')

  of pcSpanSF:
    result.add("Span(" & $e.vf & ')')

  of pcSpanSR:
    result.add("Span(" & $e.vr & ')')

  of pcSpanSP:
    result.add("Span(" & $e.vp & ')')

  of pcString:
    result.add(e.str)

  of pcString2:
    result.add($e.str2)

  of pcString3:
    result.add($e.str3)

  of pcString4:
    result.add($e.str4)

  of pcString5:
    result.add($e.str5)

  of pcString6:
    result.add($e.str6)

  of pcStringSF:
    result.add("(+" & $e.vf & ')')

  of pcStringSR:
    result.add("(+" & $e.vr & ')')

  of pcStringSP:
    result.add("(+" & $e.vp & ')')

  of pcSucceed:
    result.add("Succeed")

  of pcTabNat:
    result.add("Tab(" & $e.nat & ')')

  of pcTabNF:
    result.add("Tab(" & $e.nf & ')')

  of pcTabNR:
    result.add("Tab(" & $e.nr & ')')

  of pcTabNP:
    result.add("Tab(" & $e.np & ')')

  of pcWriteImm:
    result.add('(')
    imageSeq(result, e, refs[e.index - 2], refs, killAmpersand, true)
    result.add(" * " & $(refs[e.index - 2].fileRef))
    er = refs[e.index - 2].pThen

  of pcWriteOnM:
    result.add('(')
    imageSeq(result, e.pThen, refs[e.index - 2], refs, killAmpersand, true)
    result.add(" ** " & $(refs[e.index - 2].fileRef))
    er = refs[e.index - 2].pThen

  of pcWriteImmP:
    result.add('(')
    imageSeq(result, e, refs[e.index - 2], refs, killAmpersand, true)
    result.add(" * " & $(refs[e.index - 2].filePtr))
    er = refs[e.index - 2].pThen

  of pcWriteOnMP:
    result.add('(')
    imageSeq(result, e.pThen, refs[e.index - 2], refs, killAmpersand, true)
    result.add(" ** " & $(refs[e.index - 2].filePtr))
    er = refs[e.index - 2].pThen

  # Other pattern codes should not appear as leading elements

  of
    pcArbY,
    pcArbnoY,
    pcAssign,
    pcBreakXX,
    pcEOP,
    pcFenceY,
    pcRRemove,
    pcRRemore,
    pcUnanchored:
    result.add("???")

  e = er


proc imageSeq(
    result: var string;
    e: ref PE;
    Succ: ptr PE;
    refs: var PtrArray;
    killAmpersand: var bool;
    Paren: bool) =
  ## ``e`` refers to a pattern structure whose successor is given by ``Succ``.
  ## This proc appends to result a representation of this pattern.  The
  ## ``Paren`` parameter indicates whether parentheses are required if the
  ## output is more than one element.
  let indx = result.len
  var e1 = e
  var mult = false
  # The image of EOP is ""(the ``nil`` string)
  if e == EOP:
    result.add("EOP")
  # Else generate appropriate concatenation sequence
  else:
    while true:
      imageOne(result, e1, refs, killAmpersand)
      if peAddr(e1) == Succ or e1 == EOP: break
      mult = true
      if killAmpersand:
        killAmpersand = false
      else:
        result.add(" & ")
  if mult and Paren:
    result.insert("(", indx + 1)
    result.add(")")


proc `$`(P: ref PE): string =
  ## Returns the image of the address of the referenced pattern element.
  ## This is equivalent to Image(addr(P))

  var killAmpersand = false
  # Set true to delete next & to be output to result

  # We build a reference array whose n'th element points to the
  # pattern element whose index value is N.
  var refs = buildPtrArray(P)

  # Initialize result string
  result = newString(100)
  result = ""

  # Start of processing for Image
  imageSeq(result, P, EOP, refs, killAmpersand, false)


proc `$`*(p: Pattern): string =
  ## This functions yield strings that corresponds to the syntax needed
  ## to create the given pattern using the functions in this package. The
  ## form of this string is such that it could actually be compiled and
  ## evaluated to yield the required pattern except for references to
  ## variables and functions, which are output using one of the following
  ## forms:
  ##
  ##    access Natural     NP(16#...#)
  ##    access Pattern     patPtr(16#...#)
  ##    access String     SP(16#...#)
  ##
  ##    NaturalFunc        NF(16#...#)
  ##    StringFunc        SF(16#...#)
  ##
  ## where 16#...# is the hex representation of the integer address that
  ## corresponds to the given access value
  $(p.p)


# Dump
# ----
proc dump*(pat: Pattern) =
  ## This function writes information about the pattern to ``stdout``.
  ## The format of this information is keyed to the internal data structures
  ## used to implement patterns. The information provided by dump is thus
  ## more precise than that yielded by Image, but is also a bit more obscure
  ## (i.e. it cannot be interpreted solely in terms of this spec, you have
  ## to know something about the data structures).

  var cols: Natural = 2
  # Number of columns used for pattern numbers, minimum is 2

  proc writeNodeId(e: ptr PE) =
    ## Writes out a string identifying the given pattern element
    put("index " & $e.index)
    if e == peAddr(EOP):
      put("EOP")
      for i in cols..4:
        put(' ')
    else:
      var str = newString(cols)
      var n = Natural(e.index)
      put("#")
      for i in countdown(cols-1, 0):
        str[i] = Character(48 + n mod 10)
        n = n div 10
      put(str)

  # Start of processing for dump
  newLine()
  put(
    "Pattern dump Output(pattern at " &
    $cast[int](pat.p) &
    ", S = " &
    $pat.stk & ')')
  newLine()
  newLine()

  # If uninitialized pattern, dump line and we are done
  if pat.p == nil:
    putLine("Uninitialized pattern value")
    return

  # If ``nil`` pattern, just dump it and we are all done
  if pat.p == EOP:
    putLine("EOP(nil pattern)")
    return

  # We build a reference array whose n'th element points to the
  # pattern element whose index value is N.
  var refs = buildPtrArray(pat.p)


  # Now dump the nodes in reverse sequence. We output them in reverse
  # sequence since this corresponds to the natural order used to
  # construct the patterns.
  for i in countdown(pat.p.index-1, 0):
    var e = refs[i]
    writeNodeId(e)
    put($cast[int](e))
    put("  ")
    put($e.pCode)
    put("  ")
    writeNodeId(peAddr(e.pThen))

    case e.pCode
    of
      pcAlt,
      pcArbX,
      pcArbnoS,
      pcArbnoX:
      writeNodeId(peAddr(e.alt))

    of
      pcRpat:
      put($e.patRef)

    of
      pcRpatP:
      put($e.patPtr)

    of
      pcPredFunc:
      put($e.boolFunc)

    of
      pcAssignImm,
      pcAssignOnM,
      pcAnySR,
      pcBreakSR,
      pcBreakXSR,
      pcNotAnySR,
      pcNSpanSR,
      pcSpanSR,
      pcStringSR:
      put($e.vr)

    of
      pcAssignImmP,
      pcAssignOnMP,
      pcAnySP,
      pcBreakSP,
      pcBreakXSP,
      pcNotAnySP,
      pcNSpanSP,
      pcSpanSP,
      pcStringSP:
      put($e.vp)

    of
      pcWriteImm,
      pcWriteOnM:
      put($e.fileRef)

    of
      pcWriteImmP,
      pcWriteOnMP:
      put($e.filePtr)

    of pcString: put(e.str)
    of pcString2: put($e.str2)
    of pcString3: put($e.str3)
    of pcString4: put($e.str4)
    of pcString5: put($e.str5)
    of pcString6: put($e.str6)

    of pcSetcur: put($e.val)

    of pcSetcurP: put($e.valPtr)

    of
      pcAnyCH,
      pcBreakCH,
      pcBreakXCH,
      pcChar,
      pcNotAnyCH,
      pcNSpanCH,
      pcSpanCH:
      put('\'' & e.elem & '\'')

    of
      pcAnyCS,
      pcBreakCS,
      pcBreakXCS,
      pcNotAnyCS,
      pcNSpanCS,
      pcSpanCS:
      put('"' & $e.es & '"')

    of
      pcArbnoY,
      pcLenNat,
      pcPosNat,
      pcRPosNat,
      pcRTabNat,
      pcTabNat:
      put($e.nat)

    of
      pcPosNF,
      pcLenNF,
      pcRPosNF,
      pcRTabNF,
      pcTabNF:
      put($e.nf)

    of
      pcPosNR,
      pcLenNR,
      pcRPosNR,
      pcRTabNR,
      pcTabNR:
      put($e.nr)

    of
      pcPosNP,
      pcLenNP,
      pcRPosNP,
      pcRTabNP,
      pcTabNP:
      put($e.np)

    of
      pcAnySF,
      pcBreakSF,
      pcBreakXSF,
      pcNotAnySF,
      pcNSpanSF,
      pcSpanSF,
      pcStringSF:
      put($e.vf)

    else: discard

    newLine()
  newLine()


# Debugging Routines
# ------------------

# Debugging pattern matching operations can often be quite complex,
# since there is no obvious way to trace the progress of the match.
# The declarations in this section provide some debugging assistance.

var debugNimbol* = false ## \
  ## This global variable can be set ``true`` to generate debugging on all
  ## subsequent calls to match. The debugging output is a full trace of
  ## the actions of the pattern matcher, written to ``stdout``. The
  ## level of this information is intended to be comprehensible at the
  ## abstract level of this package declaration. However, note that the
  ## use of this switch often generates large amounts of output.

proc debugMatch(regionLevel: Natural; str: string) =
  ## Output string to standard error with bars indicating region level
  ## Defined outside xMatch to avoid bug #1807 in Nim
  for i in 1..regionLevel:
    put("| ")
  putLine(str)

proc subStrEqual[T: static[int]](
    subject: String;
    cursor: Natural;
    str: CharArray[T]): bool =
  if (subject.len - cursor) < T:
    return false
  for i, c in str:
    if subject[cursor + i] != c: return false
  return true

proc subStrEqual(subject: String; cursor: Natural; str: String): bool =
  if (subject.len - cursor) < str.len:
    return false
  for i, c in str:
    if subject[cursor + i] != c: return false
  return true

# Moved the definition of ``State` outside ``xMatch``
# to work-around bug in the Nim compiler
type State = enum MatchFail, MatchSucceed, StateFail, StateSucceed, Match

proc xMatch(
    debug: static[bool];
    subject: String;
    patP: ref PE;
    patSize: Natural;
    start: var Natural;
    stop: var Natural): bool =
  ## This is the common pattern match routine. It is passed a string and
  ## a pattern, and it indicates success or failure, and on success the
  ## section of the string matched. It does not perform any assignments
  ## to the subject string, so pattern replacement is for the caller.
  ##
  ## ``subject`` The subject string. The lower bound is always one. In the match
  ##     procs, it is fine to use strings whose lower bound is not one, but we
  ##     perform a one time conversion before the call to xMatch, so that xMatch
  ##     does not have to be bothered with strange lower bounds.
  ##
  ## ``patP`` Points to initial pattern element of pattern to be matched
  ##
  ## ``patSize`` Maximum required stack entries for pattern to be matched
  ##
  ## ``start`` If match is successful, starting index of matched section.  This
  ##     value is always non-zero. A value of zero is used to indicate a failed
  ##     match.
  ##
  ## ``stop`` If match is successful, ending index of matched section.  This can
  ##     be zero if we match the ``nil`` string at the start, in which case
  ##     start is set to zero, and stop to one. If the match fails, then the
  ##     contents of stop is undefined.

  var node: ptr PE
  # Pointer to current pattern node. Initialized from patP, and:
  # updated as the match proceeds through its constituent elements.

  let len = subject.len
  # len of string(= subject.last, since subject.first is always 1)

  var cursor: int = 0
  # This value is the index showing the current position of the match in the
  # subject string. The next character to be matched is at subject[cursor]. Note
  # that since our view of the subject string in xMatch always has a lower bound
  # of one, regardless of original bounds, that this definition exactly
  # corresponds to the cursor value as referenced by functions like Pos.
  #
  # Under some conditions this is a saved stack pointer, typically a base
  # pointer of an inner or outer region.  `cursor` temporarily holds such a
  # value when it is popped from the stack by `Fail`.  In all cases, `cursor` is
  # reset to a proper cursor value before the match proceeds (e.g. by
  # propagating the failure and popping a "real" cursor value from the stack.

  var peUnanchored = PE(pCode: pcUnanchored, index: 0, pThen: patP)
  # Dummy pattern element used in the unanchored case

  var regionLevel: Natural = 0
  # Keeps track of recursive region level. This is used only for
  # debugging, it is the number of saved history stack base values.

  var stackArray = initOdArray(stackSize, patSize+1, StackEntry)
  var stack {.volatile.} = arrayPtr(stackArray)
  # The pattern matching history/failure stack for this call to match

  var stackPtr: Natural
  # Current stack pointer. This points to the top element of the stack
  # that is currently in use. At the outer level this is the special
  # entry placed on the stack according to the anchor mode.

  let stackInit = 1
  # This is the initial value of the stackPtr and stackBase. The
  # initial(stack.first) element of the stack is not used so that
  # when we pop the last element off, stackPtr is still in range.

  var stackBase: Natural
  # This value is the stack base value, i.e. the stack pointer for the
  # first history stack entry in the current stack region. See separate
  # section on handling of recursive pattern matches.

  var assignOnM: bool = false
  # Set true if assign-on-match or write-on-match operations may be
  # present in the history stack, which must then be scanned on a
  # successful match.

  # debugMatch
  # ----
  proc debugMatch(str: string) =
    ## Output string to standard error with bars indicating region level
    when debug:
      debugMatch(regionLevel, str)

  proc debugMatch(str: string; A: Character) {.inline.} =
    ## Calls debugMatch with the string S('A')
    when debug:
      debugMatch(regionLevel, str & "('" & A & "')")

  proc debugMatch(str: string; A: CharacterSet) {.inline.} =
    # Calls debugMatch with the string S("A")
    when debug:
      debugMatch(regionLevel, str & "(" & $A & ')')

  proc debugMatch(str: string; A: Natural) {.inline.} =
    ## Calls debugMatch with the string S(A)
    when debug:
      debugMatch(regionLevel, str & "(" & $A & ')')

  proc debugMatch(str: string; A: openarray[Character]) {.inline.} =
    ## Calls debugMatch with the string S("A")
    when debug:
      debugMatch(regionLevel, str & "(" & $A & ')')

  proc debugNewLine() {.inline.} =
    ## Output new blank line with newLine if ``debug`` is true
    when debug:
      newLine()

  proc debugPut(str: string) {.inline.} =
    ## Output string with ``Put`` if ``debug`` is true
    when debug:
      put(str)

  proc debugPutLine(str: string) {.inline.} =
    ## Output string with putLine if ``debug`` is true
    when debug:
      putLine(str)

  proc `$`(P: ptr PE): string =
    ## Returns a string of the form #nnn where nnn is P.index
    return "#" & $P.index & " "

  proc `$`(P: ref PE): string =
    ## Returns a string of the form #nnn where nnn is P.index
    return "#" & $P.index & " "

  proc remove {.inline.} =
    ## Remove the current stack element
    dec(stackPtr)

  proc pop {.inline.} =
    ## Pop the stack
    ##   stackBase    current stack base
    ##   stackPtr     current stack pointer
    cursor = stack[stackPtr].cursor
    node   = stack[stackPtr].node
    dec(stackPtr)

  proc popRegion {.inline.} =
    ## Pop stack region
    ## Used at the end of processing of an inner region. If the inner
    ## region left no stack entries, then all trace of it is removed.
    ## Otherwise a pcRemoreRegion entry is pushed to ensure proper
    ## handling of alternatives in the inner region.
    dec(regionLevel)
    # If nothing was pushed in the inner region, we can just get
    # rid of it entirely, leaving no traces that it was ever there
    if stackPtr == stackBase:
      stackPtr = stackBase - 2
      stackBase = stack[stackPtr + 2].cursor
    # If stuff was pushed in the inner region, then we have to
    # push a pcRRemore node so that we properly handle possible
    # rematches within the region.
    else:
      inc(stackPtr)
      stack[stackPtr].cursor = stackBase
      stack[stackPtr].node = addr(cpRRemore)
      stackBase = stack[stackBase].cursor

  proc push(node: ptr PE) {.inline.} =
    ## Push entry onto pattern matching stack with current cursor value
    inc(stackPtr)
    stack[stackPtr].cursor = cursor
    stack[stackPtr].node = node

  proc push(p: ref PE) {.inline.} =
    ## Push entry onto pattern matching stack with current cursor value
    inc(stackPtr)
    stack[stackPtr].cursor = cursor
    stack[stackPtr].node = peAddr(p)

  proc push(p: var PE) {.inline.} =
    ## Push entry onto pattern matching stack with current cursor value
    inc(stackPtr)
    stack[stackPtr].cursor = cursor
    stack[stackPtr].node = addr(p)

  proc pushBase(p: var PE) {.inline.} =
    inc(stackPtr)
    stack[stackPtr].cursor = stackBase
    stack[stackPtr].node = addr(p)

  proc pushRegion {.inline.} =
    ## Push stack region
    ## This proc makes a new region on the history stack. The
    ## caller first establishes the special entry on the stack, but
    ## does not push the stack pointer. Then this call stacks a
    ## pcRemoveRegion node, on top of this entry, using the cursor
    ## field of the pcRemoveRegion entry to save the outer level
    ## stack base value, and resets the stack base to point to this
    ## pcRemoveRegion node.
    inc(regionLevel)
    inc(stackPtr, 2)
    stack[stackPtr].cursor = stackBase
    stack[stackPtr].node = addr(cpRRemove)
    stackBase = stackPtr


  # Start of processing for xMatch
  # -------------------------------

  debugNewLine()
  debugPutLine("Initiating pattern match, subject = " & $subject)
  debugPut     ("------------------------------------")
  for i in 1..len: debugPut("-")

  debugNewLine()
  debugPutLine("subject length = " & $len)

  if patP == nil: uninitializedPattern()

  # In anchored mode, the bottom entry on the stack is an abort entry
  if anchoredMode:
    stack[stackInit].node = addr(cpAbort)
    stack[stackInit].cursor = 0
  # In unanchored more, the bottom entry on the stack references
  # the special pattern element peUnanchored, whose pThen field
  # points to the initial pattern element. The cursor value in this
  # entry is the number of anchor moves so far.
  else:
    stack[stackInit].node = addr(peUnanchored)
    stack[stackInit].cursor = 0

  stackPtr    = stackInit
  stackBase   = stackPtr
  cursor       = 0
  node         = peAddr(patP)


  # Main Pattern Matching State Control
  # -----------------------------------

  # This is a state machine which uses gotos to change state. The
  # initial state is Match, to initiate the matching of the first
  # element, so the goto Match above starts the match. In the
  # following descriptions, we indicate the global values that
  # are relevant for the state transition.

  var state {.goto.} : State = Match

  case state

  of MatchFail:
    ## Come here if entire match fails
    debugMatch("match fails")
    debugNewLine()
    start = 0
    stop  = 0
    return false

  of MatchSucceed:
    ## Come here if entire match succeeds
    ## `cursor` current position in subject string
    debugMatch("match succeeds")
    start = stack[stackInit].cursor
    if cursor > 0:
      stop  = cursor - 1
    debugMatch("first matched character index = " & $start)
    debugMatch("last matched character index = " & $stop)
    debugMatch("matched substring = " & $subject[start..stop])

    # Scan history stack for deferred assignments or writes

    if assignOnM:
      for s in 0..stackPtr:
        if stack[s].node == addr(cpAssign):
          let innerBase = stack[s + 1].cursor
          let specialEntry = innerBase - 1
          let nodeOnM = stack[specialEntry].node
          let start = stack[specialEntry].cursor
          let stop = stack[s].cursor - 1
          if nodeOnM.pCode == pcAssignOnM:
            nodeOnM.vr[] = subject[start..stop]
            debugMatch($(stack[s].node) &
                 "deferred assignment of " &
                 $subject[start..stop])
          elif nodeOnM.pCode == pcAssignOnMP:
            nodeOnM.vp[] = subject[start..stop]
            debugMatch($(stack[s].node) &
                 "deferred assignment of " &
                 $subject[start..stop])
          elif nodeOnM.pCode == pcWriteOnM:
            writeLine(nodeOnM.fileRef[], subject[start..stop])
            debugMatch($(stack[s].node) &
                 "deferred write of " &
                 $subject[start..stop])
          elif nodeOnM.pCode == pcWriteOnMP:
            writeLine(nodeOnM.filePtr[], subject[start..stop])
            debugMatch($(stack[s].node) &
                 "deferred write of " &
                 $subject[start..stop])
          else:
            logicError()
    debugNewLine()
    return true

  of StateFail:
    ## Come here if attempt to match current element fails
    pop()
    if cursor >= 0:
      debugMatch("failure, cursor reset to " & $cursor)
    state = Match

  of StateSucceed:
    ## Come here if attempt to match current element succeeds
    ##   cursor        current position in subject string
    ##   node          pointer to node successfully matched
    ##   stackBase     current stack base
    ##   stackPtr      current stack pointer
    debugMatch("success, cursor = " & $cursor)
    node = peAddr(node.pThen)

  of Match:
    ## Come here to match the next pattern element
    ##
    ##   cursor        current position in subject string
    ##   node          pointer to node to be matched
    ##   stackBase    current stack base
    ##   stackPtr     current stack pointer
    ##
    ## Main Pattern Match Element Matching Routines
    ## --------------------------------------------
    ##
    ## Here is the case statement that processes the current node. The
    ## processing for each element does one of five things:
    ##
    ##   goto Succeed        to move to the successor
    ##   goto MatchSucceed  if the entire match succeeds
    ##   goto MatchFail     if the entire match fails
    ##   goto Fail           to signal failure of current match
    ##
    ## Processing is NOT allowed to fall through
    debugMatch("Starting Match", $node.pCode)
    case node.pCode

    # Abort
    of pcAbort:
      debugMatch($(node) & "matching Abort")
      state = MatchFail

    # Alternation
    of pcAlt:
      debugMatch($(node) & "setting up alternative " & $(node.alt))
      push(node.alt)
      node = peAddr(node.pthen)
      state = Match

    # Any(one character case)
    of pcAnyCH:
      debugMatch($(node) & "matching Any", node.elem)
      if cursor < len and subject[cursor] == node.elem:
        inc(cursor)
        state = StateSucceed
      else:
        state = StateFail

    # Any(character set case)
    of pcAnyCS:
      debugMatch($(node) & "matching Any", node.es)
      if cursor < len and subject[cursor] in node.es:
        inc(cursor)
        state = StateSucceed
      else:
        state = StateFail

    # Any(string function case)
    of pcAnySF:
      let u = node.vf()
      debugMatch($(node) & "matching Any", u)
      if cursor < len and subject[cursor] in u:
        inc(cursor)
        state = StateSucceed
      else:
        state = StateFail

    # Any(string reference case)
    of pcAnySR:
      let u = node.vr[]
      debugMatch($(node) & "matching Any", u)
      if cursor < len and subject[cursor] in u:
        inc(cursor)
        state = StateSucceed
      else:
        state = StateFail

    # Any(string pointer case)
    of pcAnySP:
      let u = node.vp[]
      debugMatch($(node) & "matching Any", u)
      if cursor < len and subject[cursor] in u:
        inc(cursor)
        state = StateSucceed
      else:
        state = StateFail

    # Arb(initial match)
    of pcArbX:
      debugMatch($(node) & "matching Arb")
      push(node.alt)
      node = peAddr(node.pthen)
      state = Match

    # Arb(extension)
    of pcArbY:
      debugMatch($(node) & "extending Arb")
      if cursor < len:
        inc(cursor)
        push(node)
        state = StateSucceed
      else:
        state = StateFail

    # ArbnoS(simple Arbno initialize). This is the node that
    # initiates the match of a simple Arbno structure.
    of pcArbnoS:
      debugMatch($(node) & "setting up Arbno alternative " & $(node.alt))
      push(node.alt)
      node = peAddr(node.pthen)
      state = Match

    # ArbnoX(Arbno initialize). This is the node that initiates
    # the match of a complex Arbno structure.
    of pcArbnoX:
      debugMatch($(node) & "setting up Arbno alternative " & $(node.alt))
      push(node.alt)
      node = peAddr(node.pthen)
      state = Match

    # ArbnoY(Arbno rematch). This is the node that is executed
    # following successful matching of one instance of a complex
    # Arbno pattern.
    of pcArbnoY:
      let nilMatch = (cursor == stack[stackBase - 1].cursor)
      debugMatch($(node) & "extending Arbno")
      popRegion()
      # If arbno extension matched nil, then immediately fail
      if nilMatch:
        debugMatch("Arbno extension matched nil, so fails")
        state = StateFail
      # Here we must do a stack check to make sure enough stack
      # is left. This check will happen once for each instance of
      # the Arbno pattern that is matched. The Nat field of a
      # pcArbno pattern contains the maximum stack entries needed
      # for the Arbno with one instance and the successor pattern
      if stackPtr + node.nat >= stackArray.len:
        stack = stackArray.setLen2(stackPtr + node.nat)
      state = StateSucceed

    # Assign. If this node is executed, it means the assign-on-match
    # or write-on-match operation will not happen after all, so we
    # is propagate the failure, removing the pcAssign node.
    of pcAssign:
      debugMatch($(node) & "deferred assign/write cancelled")
      state = StateFail

    # Assign immediate. This node performs the actual assignment
    of pcAssignImm:
      debugMatch($(node) & "executing immediate assignment of " &
           $subject[stack[stackBase - 1].cursor..cursor-1])
      node.vr[] = subject[stack[stackBase - 1].cursor..cursor-1]
      popRegion()
      state = StateSucceed

    # Assign immediate. This node performs the actual assignment
    of pcAssignImmP:
      debugMatch($(node) & "executing immediate assignment of " &
           $subject[stack[stackBase - 1].cursor..cursor-1])
      node.vp[] = subject[stack[stackBase - 1].cursor..cursor-1]
      popRegion()
      state = StateSucceed

    # Assign on match. This node sets up for the eventual assignment
    of pcAssignOnM:
      debugMatch($(node) & "registering deferred assignment")
      stack[stackBase - 1].node = node
      push(cpAssign)
      popRegion()
      assignOnM = true
      state = StateSucceed

    # Assign on match. This node sets up for the eventual assignment
    of pcAssignOnMP:
      debugMatch($(node) & "registering deferred assignment")
      stack[stackBase - 1].node = node
      push(cpAssign)
      popRegion()
      assignOnM = true
      state = StateSucceed

    # Bal
    of pcBal:
      debugMatch($(node) & "matching or extending Bal")
      if cursor >= len or subject[cursor] == ')':
        state = StateFail
      elif subject[cursor] == '(':
        var parenCount: Natural = 1
        while true:
          inc(cursor)
          if cursor >= len:
            state = StateFail
          elif subject[cursor] == '(':
            inc(parenCount)
          elif subject[cursor] == ')':
            dec(parenCount)
            if parenCount == 0: break
      inc(cursor)
      push(node)
      state = StateSucceed

    # Break(one character case)
    of pcBreakCH:
      debugMatch($(node) & "matching Break", node.elem)
      while cursor < len:
        if subject[cursor] == node.elem:
          state = StateSucceed
        else:
          inc(cursor)
      state = StateFail

    # Break(character set case)
    of pcBreakCS:
      debugMatch($(node) & "matching Break", node.es)
      while cursor < len:
        if subject[cursor] in node.es:
          state = StateSucceed
        else:
          inc(cursor)
      state = StateFail

    # Break(string function case)
    of pcBreakSF:
      let u = node.vf()
      debugMatch($(node) & "matching Break", u)
      while cursor < len:
        if subject[cursor] in u:
          state = StateSucceed
        else:
          inc(cursor)
      state = StateFail

    # Break(string reference case)
    of pcBreakSR:
      let u = node.vr[]
      debugMatch($(node) & "matching Break", u)
      while cursor < len:
        if subject[cursor] in u:
          state = StateSucceed
        else:
          inc(cursor)
      state = StateFail

    # Break(string pointer case)
    of pcBreakSP:
      let u = node.vp[]
      debugMatch($(node) & "matching Break", u)
      while cursor < len:
        if subject[cursor] in u:
          state = StateSucceed
        else:
          inc(cursor)
      state = StateFail

    # BreakX(one character case)
    of pcBreakXCH:
      debugMatch($(node) & "matching BreakX", node.elem)
      while cursor < len:
        if subject[cursor] == node.elem:
          state = StateSucceed
        else:
          inc(cursor)
      state = StateFail

    # BreakX(character set case)
    of pcBreakXCS:
      debugMatch($(node) & "matching BreakX", node.es)
      while cursor < len:
        if subject[cursor] in node.es:
          state = StateSucceed
        else:
          inc(cursor)
      state = StateFail

    # BreakX(string function case)
    of pcBreakXSF:
      let u = node.vf()
      debugMatch($(node) & "matching BreakX", u)
      while cursor < len:
        if subject[cursor] in u:
          state = StateSucceed
        else:
          inc(cursor)
      state = StateFail

    # BreakX(string reference case)
    of pcBreakXSR:
      let u = node.vr[]
      debugMatch($(node) & "matching BreakX", u)
      while cursor < len:
        if subject[cursor] in u:
          state = StateSucceed
        else:
          inc(cursor)
      state = StateFail

    # BreakX(string pointer case)
    of pcBreakXSP:
      let u = node.vp[]
      debugMatch($(node) & "matching BreakX", u)
      while cursor < len:
        if subject[cursor] in u:
          state = StateSucceed
        else:
          inc(cursor)
      state = StateFail

    # BreakXX(BreakX extension). See section on "Compound Pattern
    # Structures". This node is the alternative that is stacked
    # to skip past the break character and extend the break.
    of pcBreakXX:
      debugMatch($(node) & "extending BreakX")
      inc(cursor)
      state = StateSucceed

    # Character(one character string)
    of pcChar:
      debugMatch($(node) & "matching '" & node.elem & '\'')
      if cursor < len and subject[cursor] == node.elem:
        inc(cursor)
        state = StateSucceed
      else:
        state = StateFail

    # End of Pattern
    of pcEOP:
      if stackBase == stackInit:
        debugMatch("end of pattern")
        state = MatchSucceed
      # End of recursive inner match. See separate section on
      # handing of recursive pattern matches for details.
      else:
        debugMatch("terminating recursive match")
        node = stack[stackBase - 1].node
        popRegion()
        state = Match

    # Fail
    of pcFail:
      debugMatch($(node) & "matching Fail")
      state = StateFail

    # Fence(built in pattern)
    of pcFence:
      debugMatch($(node) & "matching Fence")
      push(cpAbort)
      state = StateSucceed

    # Fence function node X. This is the node that gets control
    # after a successful match of the fenced pattern.
    of pcFenceX:
      debugMatch($(node) & "matching Fence function")
      pushBase(cpFenceY)
      dec(regionLevel)
      state = StateSucceed

    # Fence function node Y. This is the node that gets control on
    # a failure that occurs after the fenced pattern has matched.

    # Note: the cursor at this stage is actually the inner stack
    # base value. We don't reset this, but we do use it to strip
    # off all the entries made by the fenced pattern.
    of pcFenceY:
      debugMatch($(node) & "pattern matched by Fence caused failure")
      stackPtr = cursor - 2
      state = StateFail

    # Len(integer case)
    of pcLenNat:
      debugMatch($(node) & "matching Len", node.nat)
      if cursor + node.nat > len:
        state = StateFail
      else:
        cursor = cursor + node.nat
        state = StateSucceed

    # Len(Integer function case)
    of pcLenNF:
      let n = node.nf()
      debugMatch($(node) & "matching Len", n)
      if cursor + n > len:
        state = StateFail
      else:
        cursor = cursor + n
        state = StateSucceed

    # Len(integer reference case)
    of pcLenNR:
      debugMatch($(node) & "matching Len", node.nr[])
      if cursor + node.nr[] > len:
        state = StateFail
      else:
        cursor = cursor + node.nr[]
        state = StateSucceed

    # Len(integer pointer case)
    of pcLenNP:
      debugMatch($(node) & "matching Len", node.np[])
      if cursor + node.np[] > len:
        state = StateFail
      else:
        cursor = cursor + node.np[]
        state = StateSucceed

    # NotAny(one character case)
    of pcNotAnyCH:
      debugMatch($(node) & "matching NotAny", node.elem)
      if cursor < len and subject[cursor] != node.elem:
        inc(cursor)
        state = StateSucceed
      else:
        state = StateFail

    # NotAny(character set case)
    of pcNotAnyCS:
      debugMatch($(node) & "matching NotAny", node.es)
      if cursor < len and not (subject[cursor] in node.es):
        inc(cursor)
        state = StateSucceed
      else:
        state = StateFail

    # NotAny(string function case)
    of pcNotAnySF:
      let u = node.vf()
      debugMatch($(node) & "matching NotAny", u)
      if cursor < len and not(subject[cursor] in u):
        inc(cursor)
        state = StateSucceed
      else:
        state = StateFail

    # NotAny(string reference case)
    of pcNotAnySR:
      let u = node.vr[]
      debugMatch($(node) & "matching NotAny", u)
      if cursor < len and not(subject[cursor] in u):
        inc(cursor)
        state = StateSucceed
      else:
        state = StateFail

    # NotAny(string pointer case)
    of pcNotAnySP:
      let u = node.vp[]
      debugMatch($(node) & "matching NotAny", u)
      if cursor < len and not(subject[cursor] in u):
        inc(cursor)
        state = StateSucceed
      else:
        state = StateFail

    # NSpan(one character case)
    of pcNSpanCH:
      debugMatch($(node) & "matching NSpan", node.elem)
      while cursor < len and subject[cursor] == node.elem:
        inc(cursor)
      state = StateSucceed

    # NSpan(character set case)
    of pcNSpanCS:
      debugMatch($(node) & "matching NSpan", node.es)
      while cursor < len and subject[cursor] in node.es:
        inc(cursor)
      state = StateSucceed

    # NSpan(string function case)
    of pcNSpanSF:
      let u = node.vf()
      debugMatch($(node) & "matching NSpan", u)
      while cursor < len and subject[cursor] in u:
        inc(cursor)
      state = StateSucceed

    # NSpan(string reference case)
    of pcNSpanSR:
      let u = node.vr[]
      debugMatch($(node) & "matching NSpan", u)
      while cursor < len and subject[cursor] in u:
        inc(cursor)
      state = StateSucceed

    # NSpan(string pointer case)
    of pcNSpanSP:
      let u = node.vp[]
      debugMatch($(node) & "matching NSpan", u)
      while cursor < len and subject[cursor] in u:
        inc(cursor)
      state = StateSucceed

    of pcNil:
      debugMatch($(node) & "matching nil")
      state = StateSucceed

    # Pos(integer case)
    of pcPosNat:
      debugMatch($(node) & "matching Pos", node.nat)
      if cursor == node.nat:
        state = StateSucceed
      else:
        state = StateFail

    # Pos(Integer function case)
    of pcPosNF:
      let n = node.nf()
      debugMatch($(node) & "matching Pos", n)
      if cursor == n:
        state = StateSucceed
      else:
        state = StateFail

    # Pos(integer reference case)
    of pcPosNR:
      debugMatch($(node) & "matching Pos", node.nr[])
      if cursor == node.nr[]:
        state = StateSucceed
      else:
        state = StateFail

    # Pos(integer pointer case)
    of pcPosNP:
      debugMatch($(node) & "matching Pos", node.np[])
      if cursor == node.np[]:
        state = StateSucceed
      else:
        state = StateFail

    # Predicate function
    of pcPredFunc:
      debugMatch($(node) & "matching predicate function")
      if node.boolFunc():
        state = StateSucceed
      else:
        state = StateFail

    # Region Enter. Initiate new pattern history stack region
    of pcREnter:
      debugMatch($(node) & "starting match of nested pattern")
      stack[stackPtr + 1].cursor = cursor
      pushRegion()
      state = StateSucceed

    # Region Remove node. This is the node stacked by an REnter.  It removes the
    # special format stack entry right underneath, and then restores the outer
    # level stack base and signals failure.
    #
    # Note: the cursor value at this stage is actually the stack base value for
    # the outer level.
    of pcRRemove:
      debugMatch("failure, match of nested pattern terminated")
      stackBase = cursor
      dec(regionLevel)
      remove()
      state = StateFail

    # Region restore node. This is the node stacked at the end of an
    # inner level match. Its function is to restore the inner level
    # region, so that alternatives in this region can be sought.
    #
    # Note: the cursor at this stage is actually the inner stack base value,
    # which we use to restore the inner region.
    of pcRRemore:
      debugMatch("failure, search for alternatives in nested pattern")
      inc(regionLevel)
      stackBase = cursor
      state = StateFail

    # Rem
    of pcRem:
      debugMatch($(node) & "matching Rem")
      cursor = len
      state = StateSucceed

    # Initiate recursive match(pattern pointer case)
    of pcRpat:
      stack[stackPtr + 1].node = peAddr(node.pthen)
      pushRegion()
      debugMatch($(node) & "initiating recursive match")
      if stackPtr + node.patRef[].stk >= stackArray.len:
        stack = stackArray.setLen2(stackPtr + node.patRef[].stk)
      node = peAddr(node.patRef.p)
      state = Match

    # Initiate recursive match(pattern pointer case)
    of pcRpatP:
      stack[stackPtr + 1].node = peAddr(node.pthen)
      pushRegion()
      debugMatch($(node) & "initiating recursive match")
      if stackPtr + node.patPtr[].stk >= stackArray.len:
        stack = stackArray.setLen2(stackPtr + node.patPtr[].stk)
      node = peAddr(node.patPtr.p)
      state = Match

    # RPos(integer case)
    of pcRPosNat:
      debugMatch($(node) & "matching RPos", node.nat)
      if cursor == (len - node.nat):
        state = StateSucceed
      else:
        state = StateFail

    # RPos(integer function case)
    of pcRPosNF:
      let n = node.nf()
      debugMatch($(node) & "matching RPos", n)
      if len - cursor == n:
        state = StateSucceed
      else:
        state = StateFail

    # RPos(integer reference case)
    of pcRPosNR:
      debugMatch($(node) & "matching RPos", node.nr[])
      if cursor == (len - node.nr[]):
        state = StateSucceed
      else:
        state = StateFail

    # RPos(integer pointer case)
    of pcRPosNP:
      debugMatch($(node) & "matching RPos", node.np[])
      if cursor == (len - node.np[]):
        state = StateSucceed
      else:
        state = StateFail

    # RTab(integer case)
    of pcRTabNat:
      debugMatch($(node) & "matching RTab", node.nat)
      if cursor <= len - node.nat:
        cursor = len - node.nat
        state = StateSucceed
      else:
        state = StateFail

    # RTab(integer function case)
    of pcRTabNF:
      let n = node.nf()
      debugMatch($(node) & "matching RPos", n)
      if len - cursor >= n:
        cursor = len - n
        state = StateSucceed
      else:
        state = StateFail

    # RTab(integer pointer case)
    of pcRTabNR:
      debugMatch($(node) & "matching RPos", node.nr[])
      if cursor <= len - node.nr[]:
        cursor = len - node.nr[]
        state = StateSucceed
      else:
        state = StateFail

    # RTab(integer pointer case)
    of pcRTabNP:
      debugMatch($(node) & "matching RPos", node.np[])
      if cursor <= len - node.np[]:
        cursor = len - node.np[]
        state = StateSucceed
      else:
        state = StateFail

    # cursor assignment
    of pcSetcur:
      debugMatch($(node) & "matching Setcur")
      node.val[] = cursor
      state = StateSucceed

    # cursor assignment
    of pcSetcurP:
      debugMatch($(node) & "matching Setcur")
      node.valPtr[] = cursor
      state = StateSucceed

    # Span(one character case)
    of pcSpanCH:
      var p = cursor
      debugMatch($(node) & "matching Span", node.elem)
      while p < len and subject[p] == node.elem:
        inc(p)
      if p != cursor:
        cursor = p
        state = StateSucceed
      else:
        state = StateFail

    # Span(character set case)
    of pcSpanCS:
      var p = cursor
      debugMatch($(node) & "matching Span", node.es)
      while p < len and subject[p] in node.es:
        inc(p)
      if p != cursor:
        cursor = p
        state = StateSucceed
      else:
        state = StateFail

    # Span(string function case)
    of pcSpanSF:
      let u = node.vf()
      debugMatch($(node) & "matching Span", u)
      var p = cursor
      while p < len and subject[p] in u:
        inc(p)
      if p != cursor:
        cursor = p
        state = StateSucceed
      else:
        state = StateFail

    # Span(string reference case)
    of pcSpanSR:
      let u = node.vr[]
      debugMatch($(node) & "matching Span", u)
      var p = cursor
      while p < len and subject[p] in u:
        inc(p)
      if p != cursor:
        cursor = p
        state = StateSucceed
      else:
        state = StateFail

    # Span(string pointer case)
    of pcSpanSP:
      let u = node.vp[]
      debugMatch($(node) & "matching Span", u)
      var p = cursor
      while p < len and subject[p] in u:
        inc(p)
      if p != cursor:
        cursor = p
        state = StateSucceed
      else:
        state = StateFail

    # String(two character case)
    of pcString2:
      debugMatch($(node) & "matching " & $node.str2)
      if subject.subStrEqual(cursor, node.str2):
        inc(cursor, 2)
        state = StateSucceed
      else:
        state = StateFail

    # String(three character case)
    of pcString3:
      debugMatch($(node) & "matching " & $node.str3)
      if subject.subStrEqual(cursor, node.str3):
        inc(cursor, 3)
        state = StateSucceed
      else:
        state = StateFail

    # String(four character case)
    of pcString4:
      debugMatch($(node) & "matching " & $node.str4)
      if subject.subStrEqual(cursor, node.str4):
        inc(cursor, 4)
        state = StateSucceed
      else:
        state = StateFail

    # String(five character case)
    of pcString5:
      debugMatch($(node) & "matching " & $node.str5)
      if subject.subStrEqual(cursor, node.str5):
        inc(cursor, 5)
        state = StateSucceed
      else:
        state = StateFail

    # String(six character case)
    of pcString6:
      debugMatch($(node) & "matching " & $node.str6)
      if subject.subStrEqual(cursor, node.str6):
        inc(cursor, 6)
        state = StateSucceed
      else:
        state = StateFail

    # String(case of more than six characters)
    of pcString:
      debugMatch($(node) & "matching " & node.str)
      if subject.subStrEqual(cursor, node.str):
        inc(cursor, node.str.len)
        state = StateSucceed
      else:
        state = StateFail

    # String(function case)
    of pcStringSF:
      let u = node.vf()
      debugMatch($(node) & "matching " & u)
      if subject.subStrEqual(cursor, u):
        inc(cursor, u.len)
        state = StateSucceed
      else:
        state = StateFail

    # String(String reference case)
    of pcStringSR:
      let u = node.vr[]
      debugMatch($(node) & "matching " & u)
      if subject.subStrEqual(cursor, u):
        inc(cursor, u.len)
        state = StateSucceed
      else:
        state = StateFail

    # String(String pointer case)
    of pcStringSP:
      let u = node.vp[]
      debugMatch($(node) & "matching " & u)
      if subject.subStrEqual(cursor, u):
        inc(cursor, u.len)
        state = StateSucceed
      else:
        state = StateFail

    # Succeed
    of pcSucceed:
      debugMatch($(node) & "matching Succeed")
      push(node)
      state = StateSucceed

    # Tab(integer case)
    of pcTabNat:
      debugMatch($(node) & "matching Tab", node.nat)
      if cursor <= node.nat:
        cursor = node.nat
        state = StateSucceed
      else:
        state = StateFail

    # Tab(integer function case)
    of pcTabNF:
      let n = node.nf()
      debugMatch($(node) & "matching Tab ", n)
      if cursor <= n:
        cursor = n
        state = StateSucceed
      else:
        state = StateFail

    # Tab(integer pointer case)
    of pcTabNR:
      debugMatch($(node) & "matching Tab ", node.nr[])
      if cursor <= node.nr[]:
        cursor = node.nr[]
        state = StateSucceed
      else:
        state = StateFail

    # Tab(integer pointer case)
    of pcTabNP:
      debugMatch($(node) & "matching Tab ", node.np[])
      if cursor <= node.np[]:
        cursor = node.np[]
        state = StateSucceed
      else:
        state = StateFail

    # Unanchored movement
    of pcUnanchored:
      debugMatch("attempting to move anchor point")
      # All done if we tried every position
      if cursor > len:
        state = MatchFail
      # Otherwise extend the anchor point, and restack ourself
      else:
        inc(cursor)
        push(node)
        state = StateSucceed

    # Write immediate. This node performs the actual write
    of pcWriteImm:
      debugMatch($(node) & "executing immediate write of " &
           subject[stack[stackBase - 1].cursor..cursor-1])
      writeLine(node.fileRef[], subject[stack[stackBase - 1].cursor..cursor-1])
      popRegion()
      state = StateSucceed

    # Write immediate. This node performs the actual write
    of pcWriteImmP:
      debugMatch($(node) & "executing immediate write of " &
           subject[stack[stackBase - 1].cursor..cursor-1])
      writeLine(node.filePtr[], subject[stack[stackBase - 1].cursor..cursor-1])
      popRegion()
      state = StateSucceed

    # Write on match. This node sets up for the eventual write
    of pcWriteOnM:
      debugMatch($(node) & "registering deferred write")
      stack[stackBase - 1].node = node
      push(cpAssign)
      popRegion()
      assignOnM = true
      state = StateSucceed

    # Write on match. This node sets up for the eventual write
    of pcWriteOnMP:
      debugMatch($(node) & "registering deferred write")
      stack[stackBase - 1].node = node
      push(cpAssign)
      popRegion()
      assignOnM = true
      state = StateSucceed

  # We are NOT allowed to fall though this case statement, since every match
  # routine must end by executing a state equal to the appropriate point in the
  # finite state machine model.
  logicError()


# Match
# -----

# Simple match functions. The subject is matched against the pattern.
# Any immediate or deferred assignments or writes are executed, and
# the returned value indicates whether or not the match succeeded.

proc match*(subject: String; pat: Pattern): bool =
  var start, stop: Natural
  if debugNimbol:
    xMatch(true, subject, pat.p, pat.stk, start, stop)
  else:
    xMatch(false, subject, pat.p, pat.stk, start, stop)

# Replacement functions. The subject is matched against the pattern.
# Any immediate or deferred assignments or writes are executed, and
# the returned value indicates whether or not the match succeeded.
# If the match succeeds, then the matched part of the subject string
# is replaced by the given Replace string.

proc match*(subject: var String; pat: Pattern; Replace: String): bool =
  var start, stop: Natural
  if debugNimbol:
    result = xMatch(true, subject, pat.p, pat.stk, start, stop)
  else:
    result = xMatch(false, subject, pat.p, pat.stk, start, stop)
  if result:
    subject[start..stop] = Replace

proc match*(subject: String; pat: PString): bool =
  let l = subject.len
  let patLen = pat.len
  if anchoredMode:
    if patLen > l:
      return false
    else:
      return subject.subStrEqual(0, pat)
  else:
    for i in 0..l-patLen:
      if subject.subStrEqual(i, pat):
        return true
    return false

proc match*(subject: var String; pat: PString; Replace: String): bool =
  var start, stop: Natural
  if debugNimbol:
    result = xMatch(true, subject, toPE(pat), 0, start, stop)
  else:
    result = xMatch(false, subject, toPE(pat), 0, start, stop)
  if result:
    subject[start..stop] = Replace

proc match*(subject: String; pat: PString) =
  var start, stop: Natural
  if debugNimbol:
    discard xMatch(true, subject, toPE(pat), 0, start, stop)
  else:
    discard xMatch(false, subject, toPE(pat), 0, start, stop)

proc match*(subject: var String; pat: PString; Replace: String) =
  var start, stop: Natural
  if debugNimbol:
    discard xMatch(true, subject, toPE(pat), 0, start, stop)
  else:
    discard xMatch(false, subject, toPE(pat), 0, start, stop)
  if start != 0:
    subject[start..stop] = Replace

proc match*(subject: var String; pat: Pattern; res: var MatchResult): bool =
  var start, stop: Natural
  var matched: bool
  if debugNimbol:
    matched = xMatch(true, subject, pat.p, pat.stk, start, stop)
  else:
    matched = xMatch(false, subject, pat.p, pat.stk, start, stop)
  if matched:
    res.res   = addr(subject)
    res.start = start
    res.stop  = stop
    true
  else:
    res.res = nil
    false


when isMainModule:

  import strutils

  const
    LowerLetters = {'a'..'z'}
    UpperLetters = {'A'..'Z'}
    LettersDigits = Letters + Digits
    Punctuation = {'\33'..'\47', '\58'..'\64', '\91'..'\96', '\123'..'\126'}

  let hello = "Hello"
  let helloPattern = pattern(hello)
  let worldPattern = pattern("World!")
  var s1, s2, s3, s4 : string

  # Any
  block:
    let subject1 = "Hello World!"
    let subject2 = "Hello World!abc"
    let subject3 = "Hello abcWorld!"

    let p1 = helloPattern & ' ' & worldPattern

    let p2 =
        helloPattern ** s2  &
        ' '                 &
        worldPattern        &
        Any("abcdef") ** s1 &
        pattern("bc")

    let p3 =
        helloPattern        &
        ' '                 &
        Any("abc") ** s3    &
        pattern("bc")       &
        worldPattern ** s4

    let p4 =
        helloPattern ** s2  &
        ' '                 &
        worldPattern        &
        Any('a') ** s1      &
        "bc"

    let p5 =
        helloPattern        &
        ' '                 &
        Any('a') ** s3      &
        "bc"                &
        worldPattern ** s4

    let anyStr = "abc"

    let p6 =
        helloPattern ** s2  &
        ' '                 &
        worldPattern        &
        Any(anyStr) ** s1   &
        "bc"

    let p7 =
        helloPattern        &
        ' '                 &
        Any(anyStr) ** s3   &
        "bc"                &
        worldPattern ** s4

    let p8 = Any(Letters) & "ello World!"

    let p9 =
        "Hello"             &
        Any(Whitespace)     &
        "World"             &
        Any(Punctuation)

    assert match(subject1, p1) == true
    assert match(subject2, p1) == true
    assert match(subject3, p1) == false
    assert match(subject1, p2) == false
    assert match(subject2, p2) == true
    assert s1 == "a"
    assert s2 == "Hello"
    assert match(subject3, p2) == false
    assert match(subject1, p3) == false
    assert match(subject2, p3) == false
    assert match(subject3, p3) == true
    assert s3 == "a"
    assert s4 == "World!"
    assert match(subject2, p4) == true
    assert s1 == "a"
    assert s2 == "Hello"
    assert match(subject3, p4) == false
    assert match(subject1, p5) == false
    assert match(subject2, p5) == false
    assert match(subject3, p5) == true
    assert s3 == "a"
    assert s4 == "World!"
    assert match(subject1, p6) == false
    assert match(subject2, p6) == true
    assert s1 == "a"
    assert s2 == "Hello"
    assert match(subject3, p6) == false
    assert match(subject1, p7) == false
    assert match(subject2, p7) == false
    assert match(subject3, p7) == true
    assert s3 == "a"
    assert s4 == "World!"
    assert match(subject1, p8) == true
    assert match(subject1, p9) == true

  # Arb
  block:
    let subject1 = "Hello World!"
    let subject2 = "Hello World!abc"
    let subject3 = "Hello abcWorld!"

    let p1 = helloPattern & ' ' & worldPattern
    let p2 = helloPattern & ' ' & worldPattern & Arb()
    let p3 = helloPattern & Arb() & worldPattern

    assert match(subject1, p1) == true
    assert match(subject2, p1) == true
    assert match(subject3, p1) == false
    assert match(subject1, p2) == true
    assert match(subject2, p2) == true
    assert match(subject3, p2) == false
    assert match(subject1, p3) == true
    assert match(subject2, p3) == true
    assert match(subject3, p3) == true

  # Arbno
  block:
    let subject1 = "HelloHelloHello World!"
    let subject2 = "Hello World!abcabc"
    let subject3 = "Hello abcabcWorld!"
    let subject4 = "Hello World!aa"
    let subject5 = "Hello aaWorld!"
    let subject6 = "HelloHelloHello World!HelloHelloHello World!"
    let subject8 = "Hello World!abcabcHello World!abcabc"
    let subject9 = "Hello abcabcWorld!Hello abcabcWorld!"

    let p1 = Arbno(helloPattern) & ' ' & worldPattern
    let p2 = helloPattern & ' ' & worldPattern & Arbno("abc")
    let p3 = helloPattern & ' ' & Arbno("abc") & worldPattern
    let p4 = helloPattern & ' ' & worldPattern & Arbno('a')
    let p5 = helloPattern & ' ' & Arbno('a') & worldPattern
    let p6 = Arbno(p1)
    let p7 = Arbno(p2)
    let p8 = "Never!" & Arbno(p3)

    assert match(subject1, p1) == true
    assert match(subject2, p1) == true
    assert match(subject3, p1) == false
    assert match(subject4, p1) == true
    assert match(subject5, p1) == false
    assert match(subject1, p2) == true
    assert match(subject2, p2) == true
    assert match(subject3, p2) == false
    assert match(subject1, p3) == true
    assert match(subject2, p3) == true
    assert match(subject3, p3) == true
    assert match(subject1, p4) == true
    assert match(subject4, p4) == true
    assert match(subject5, p4) == false
    assert match(subject1, p5) == true
    assert match(subject4, p5) == true
    assert match(subject5, p5) == true
    assert match(subject6, p6) == true
    assert match(subject8, p6) == true
    assert match(subject9, p6) == true
    assert match(subject6, p7) == true
    assert match(subject8, p7) == true
    assert match(subject9, p7) == true
    assert match(subject6, p8) == false
    assert match(subject8, p8) == false
    assert match(subject9, p8) == false

  # Assign
  block:
    # Assigment on match
    var vowel: string
    let p1 = Any("aeiou") ** vowel
    assert match("Hello", p1) == true
    assert vowel == "e"

    # Immediate assigment
    var nonv: string
    var pos: Natural
    let p2 = Setcur(pos) & 'l' * nonv & Abort()
    assert match("Hello", p2) == false
    assert nonv == "l"

    # Assign on match & replace with value from match
    var sss: string
    var subject1 = "hello"
    var subject2 = "goodbye"

    var match1: MatchResult
    let p3 = pattern("good") ** sss
    assert match(subject1, p3, match1) == false

    var match2: MatchResult
    assert match(subject2, p3, match2) == true
    match2.Replace("<b>" & sss & "</b>")
    assert match2.res[] == "<b>good</b>bye"

    # test "delayed evaluation" of string value
    var str = ""
    proc s: String = str
    let p4 = "H" & vowel & +s
    str = "ll"  # AFTER p4 creation
    var subject = "Hello"
    var r = ""
    assert match(subject, p4, r) == true
    assert subject == "o"

  # Bal
  block:
    let subject1 = "HelloHelloHello World!"
    let subject2 = "()(())(pp())"
    let subject3 = "())"
    let subject4 = "())("
    let subject5 = "((())"

    let p1 = Pos(0) & Bal() & Rpos(0)

    assert match(subject1, p1) == true
    assert match(subject2, p1) == true
    assert match(subject3, p1) == false
    assert match(subject4, p1) == false
    assert match(subject5, p1) == false

  # Break
  block:
    let subject1 = "Hello World!"
    let subject2 = "Hello World!123"
    let subject3 = "HelloWorld!"
    let subject4 = "Hello 123World!"
    let subject5 = "HelloWorld!WordBye"

    let p1 = helloPattern & ' ' & worldPattern
    let p2 = helloPattern & ' ' & Break("ZP") & worldPattern
    let p3 = helloPattern & Break("WZ") & worldPattern
    let p4 = helloPattern & Break("WZ") & worldPattern & "Bye"

    assert match(subject1, p1) == true
    assert match(subject2, p1) == true
    assert match(subject3, p1) == false
    assert match(subject1, p2) == false
    assert match(subject2, p2) == false
    assert match(subject3, p2) == false
    assert match(subject1, p3) == true
    assert match(subject2, p3) == true
    assert match(subject3, p3) == true
    assert match(subject4, p3) == true
    assert match(subject5, p3) == true
    assert match(subject5, p4) == false

  # Defer
  block:
    var subject = "Hello"
    var s: string
    var p1 = pattern('c')
    let p2 = "H" & +p1 & +s
    p1 = Any("aeiuo")  # AFTER p2 creation
    s = "ll"           # AFTER p2 creation

    assert match(subject, p2, "") == true
    assert subject == "o"

  # Fence
  block:
    let subject1 = "Hello World!"
    let subject2 = "Hello World!123"
    let subject3 = "Hello123World!"
    let ello = "ello"

    let elloPattern = pattern(ello)

    let p1 = Fence() & helloPattern & ' ' & worldPattern
    let p2 = Fence() & helloPattern & ' ' & worldPattern & Span("0123456789")
    let p3 = Fence() & elloPattern & Span("0123456789") & worldPattern
    let p4 = Fence() & elloPattern & ' ' & worldPattern
    let p5 = Fence() & elloPattern & ' ' & worldPattern & Span("0123456789")
    let p6 = Fence() & elloPattern & Span("0123456789") & worldPattern

    assert match(subject1, p1) == true
    assert match(subject2, p1) == true
    assert match(subject3, p1) == false
    assert match(subject1, p2) == false
    assert match(subject2, p2) == true
    assert match(subject3, p2) == false
    assert match(subject1, p3) == false
    assert match(subject2, p3) == false
    assert match(subject3, p3) == false
    assert match(subject1, p4) == false
    assert match(subject2, p4) == false
    assert match(subject3, p4) == false
    assert match(subject1, p5) == false
    assert match(subject2, p5) == false
    assert match(subject3, p5) == false
    assert match(subject1, p6) == false
    assert match(subject2, p6) == false
    assert match(subject3, p6) == false

  # Let
  block:
    let p1 = 'a' & Len(1) & 'a'
    assert match("indiana", p1) == true
    assert match("Alabama", p1) == true
    assert match("arkansas", p1) == false

  # NotAny
  block:
    let subject1 = "Hello World!"
    let subject2 = "Hello World!abc"
    let subject3 = "Hello abcWorld!"

    let p1 = helloPattern & ' ' & worldPattern

    let p2 =
        helloPattern ** s2  &
        ' '                 &
        worldPattern        &
        NotAny("DEF") ** s1 &
        pattern("bc")

    let p3 =
        helloPattern        &
        ' '                 &
        NotAny("DEF") ** s3 &
        pattern("bc")       &
        worldPattern ** s4

    assert match(subject1, p1) == true
    assert match(subject2, p1) == true
    assert match(subject3, p1) == false
    assert match(subject1, p2) == false
    assert match(subject2, p2) == true
    assert s1 == "a"
    assert s2 == "Hello"
    assert match(subject3, p2) == false
    assert match(subject1, p3) == false
    assert match(subject2, p3) == false
    assert match(subject3, p3) == true
    assert s3 == "a"
    assert s4 == "World!"

  # NSpan
  block:
    let subject1 = "Hello World!"
    let subject2 = "Hello World!123"
    let subject3 = "Hello 123World!"

    let p1 = helloPattern & ' ' & worldPattern

    let p2 =
        helloPattern                &
        ' '                         &
        worldPattern                &
        NSpan("0123456789")

    let p3 =
        helloPattern                &
        ' '                         &
        NSpan("0123456789")         &
        worldPattern

    assert match(subject1, p1) == true
    assert match(subject2, p1) == true
    assert match(subject3, p1) == false
    assert match(subject1, p2) == true
    assert match(subject2, p2) == true
    assert match(subject3, p2) == false
    assert match(subject1, p3) == true
    assert match(subject2, p3) == true
    assert match(subject3, p3) == true

  # Pos
  block:
    let subject1 = "indiana"
    let subject2 = "alabama"
    let subject3 = "arkansas"

    let p1 = "a" & Pos(4) & Len(1) & "a"
    let p2 = Pos(0) & "a" & Len(1) & "a"

    assert match(subject1, p1) == false
    assert match(subject2, p1) == false
    assert match(subject3, p1) == false
    assert match(subject1, p2) == false
    assert match(subject2, p2) == true
    assert match(subject3, p1) == false

  # Rem
  block:
    # assigment on match
    var vowel: string
    var r: string

    let p5 = Any("aeiou") ** vowel & Rem() * r

    assert match("Hello", p5) == true
    assert vowel == "e"
    assert r == "llo"

  # Rpos
  block:
    let subject1 = "indiana"
    let subject2 = "alabama"
    let subject3 = "arkansas"

    let p1 = Rpos(3) & "a" & Len(1) & "a"
    let p2 = Rpos(7) & "a" & Len(1) & "a"

    assert match(subject1, p1) == true
    assert match(subject2, p1) == true
    assert match(subject3, p1) == false
    assert match(subject1, p2) == false
    assert match(subject2, p2) == true
    assert match(subject3, p1) == false

  # Rtab
  block:
    let subject1 = "indiana"
    let subject2 = "alabama"
    let subject3 = "arkansas"

    let p1 = Rtab(2) & Len(1) & "a"

    assert match(subject1, p1) == true
    assert match(subject2, p1) == true
    assert match(subject3, p1) == false

  # Span
  block:
    let subject1 = "Hello World!"
    let subject2 = "Hello World!123"
    let subject3 = "Hello123World!"

    let p1 = helloPattern & ' ' & worldPattern

    let p2 =
        helloPattern  &
        ' '           &
        worldPattern  &
        Span("0123456789")

    let p3 = helloPattern & Span("0123456789") & worldPattern

    assert match(subject1, p1) == true
    assert match(subject2, p1) == true
    assert match(subject3, p1) == false
    assert match(subject1, p2) == false
    assert match(subject2, p2) == true
    assert match(subject3, p2) == false
    assert match(subject1, p3) == false
    assert match(subject2, p3) == false
    assert match(subject3, p3) == true

  # Tab
  block:
    let subject1 = "indiana"
    let subject2 = "alabama"
    let subject3 = "arkansas"

    let p1 = Tab(1) & Len(1) & "a"

    assert match(subject1, p1) == false
    assert match(subject2, p1) == true
    assert match(subject3, p1) == false

  echo("Nimbol passed!")
