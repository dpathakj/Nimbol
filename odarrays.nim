#
#
# Based on code from rtarray.nim in Nim's Runtime Library
#        (c) Copyright 2015 Andreas Rumpf
#        (c) Copyright 2015 Henry G. Weller
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Module that implements ``OdArray[As: static[int], T]`` an optimized dynamic
## array the size of which may be specified and may be adjusted at runtime.

type
  OdArray*[As: static[int], T] = object
    ## an optimized dynamic array the size of which may be specified and may be
    ## adjusted at runtime.
    ##
    ## For small sizes an ``array[T]`` is used but if the specified size is
    ## larger than the ``As: static[int]`` size specified then a ``seq[T]`` is
    ## allocated with the required size.
    len: Natural
    dynArray: seq[T]
    smallArray: array[As, T]
  UncheckedArray* {.unchecked.}[T] = array[0..100_000_000, T]

proc len*[As: static[int], T](a: OdArray[As, T]): Natural =
  ## returns the length of the ``OdArray`` `a`
  a.len

# This doesn't work due to bugs in the handling for static[T]: #3309
# proc initOdArray*[As: static[int], T](len: Natural): OdArray[As, T] =
#   result.len = len
#   if len > As:
#     newSeq(result.dynArray, len)

proc initOdArray*(As: static[int]; len: Natural; T: typedesc): OdArray[As, T] =
  # initializes and return an ``OdArray`` with the length `len`
  result.len = len
  if len > As:
    newSeq(result.dynArray, len)

proc init*[As: static[int], T](a: var OdArray[As, T]; len: Natural) =
  # initializes the ``OdArray`` `a` with the length `len`
  a.len = len
  if len > As:
    newSeq(a.dynArray, len)

proc arrayPtr*[As: static[int], T](x: var OdArray[As, T]):
    ptr UncheckedArray[T] =
  ## returns the currently active data in the form of a ``ptr`` to an unchecked
  ## ``array``
  if x.len > As:
    cast[ptr UncheckedArray[T]](addr(x.dynArray[0]))
  else:
    cast[ptr UncheckedArray[T]](addr(x.smallArray[0]))

proc setLen*[As: static[int], T](a: var OdArray[As, T]; len: Natural):
  ptr UncheckedArray[T] =
  ## sets the length of the ``OdArray`` `a` to `len` preserving element values.
  ##
  ## **Note**: ``setLen`` may change the pointer returned by ``arrayPtr`` if the
  ## length change causes a change between the two storage mechanisms and so
  ## returns the new array pointer.
  if len > As:
    if a.len > As:
      a.dynArray.setLen(len)
    else:
      newSeq(a.dynArray, len)
      for i in 0..min(len, a.len):
        a.dynArray[i] = a.smallArray[i]
  else:
    if a.len > As:
      for i in 0..min(len, a.len):
        a.smallArray[i] = a.dynArray[i]
      a.dynArray.setLen(0)
  a.len = len
  arrayPtr a

proc bucketLen(As: Natural; len: Natural): Natural =
  ## returns the nearest length larger than `len` which is a power of 2 multiple
  ## of `As`
  result = As
  while result < len:
    result *= 2

proc setLen2*[As: static[int], T](a: var OdArray[As, T]; len: Natural):
  ptr UncheckedArray[T] =
  ## sets the length of the ``OdArray`` `a` to `len` preserving element values
  ## and for large arrays increasing the size of ``seq`` in powers of 2 to
  ## reduce the number of repeated allocations.
  ##
  ## **Note**: ``setLen2`` may change the pointer returned by ``arrayPtr`` if
  ## the length change causes a change between the two storage mechanisms and so
  ## returns the new array pointer.
  if len > As:
    if a.len > As and len > a.dynArray.len:
      a.dynArray.setLen(bucketLen(a.dynArray.len, len))
    else:
      newSeq(a.dynArray, bucketLen(As, len))
      for i in 0..min(len, a.len):
        a.dynArray[i] = a.smallArray[i]
  else:
    if a.len > As:
      for i in 0..min(len, a.len):
        a.smallArray[i] = a.dynArray[i]
      a.dynArray.setLen(0)
  a.len = len
  arrayPtr a

proc collectionToString[T](x: T; len: Natural; b, e: string): string =
  result = b
  var firstElement = true
  for i in 0..len-1:
    if not firstElement: result.add(", ")
    result.add($x[i])
    firstElement = false
  result.add(e)

proc `$`*[As: static[int], T](a: OdArray[As, T]): string =
  ## generic ``$`` operator for ``OdArray`` which is equivalent to ``$`` for
  ## ``seq``
  if a.len > As:
    collectionToString(a.dynArray, a.len, "@[", "]")
  else:
    collectionToString(a.smallArray, a.len, "@[", "]")

when isMainModule:
  var a = initOdArray(10, 1, int)
  var aPtr = arrayPtr(a)
  aPtr[0] = 1
  echo($a)
  aPtr = a.setLen(2)
  aPtr[1] = 2
  echo($a)
  aPtr = a.setLen(30)
  aPtr[23] = 24
  echo($a)
  aPtr = a.setLen(5)
  echo($a)
  aPtr = a.setLen(30)
  echo($a)
