# Intervalidus

## In what intervals are your data valid?

A Scala library with zero dependencies for representing data as valid only in discrete or continuous intervals, in one,
two, three, or more (!) dimensions. This seems to come up a lot in application design both in terms of effective dating
and versioning data.

## Usage

Add the following to your **build.sbt** file:

```sbt
resolvers += "Intervalidus" at "https://maven.pkg.github.com/rremple/intervalidus"
libraryDependencies += "rremple" %% "intervalidus" % "<version>"
```

For more on usage including other artifacts, Scala 2 considerations, GitHub Package authentication, etc., see 
[expanded usage](usage.md).

### Goals, Non-Goals, Background, and Motivation:

This is more of a personal exploration than anything. Mostly just revisiting my many decades of past enterprise
software projects and thinking about what generic libraries would have been useful. Also exploring the new capabilities
and cleaner syntax of Scala 3 to solve real-world problems elegantly. This is a very generic, low-level library that
solves a very narrowly defined problem. (If you were looking for yet another cool application of AI that claims to solve
everything wrong in the world, look elsewhere…)

In my experience, there are lots of ways to represent and collect commonly used data types. For example, just using the
types that come with the Java/Scala standard libraries and other popular libraries (i.e., primitive types, strings, time
types, various collections, etc.) is great. But the notion of how to express the conditions under which data are valid
(not to be confused with validating data), although it comes up in project after project, is generally left as an
application design consideration. But it is such a common pattern, it is odd that there isn’t some more direct support
in libraries for representing validity as a kind of collection. There are lots of libraries for representing intervals
and even interval sets, but not intervals as they relate to data as a kind of collection, and not multidimensional
intervals. (Intervalidus has no upper bound on the number of dimensions supported!)

For example, what is valid may depend on time. Say that on December 25<sup>th</sup> a user signs up for a basic tier of
service effective January 1<sup>st</sup>. Then, on March 15<sup>th</sup>, they upgrade to the premium service tier
effective on April 1<sup>st</sup>. Later, on June 28<sup>th</sup>, they choose not to renew and cancel their service,
expecting it to expire at the end of the month. The following shows what tier is known to be valid, and how that evolves
over time. (Here we use +∞ to represent no planned termination, or what is sometimes called the “end of time”.)

| On    | Evolving valid tier data               |
|-------|----------------------------------------|
| 12/25 | Basic: 1/1 – +∞                        |
| 3/15  | Basic: 1/1 – 3/31; Premium: 4/1 – +∞   |
| 6/28  | Basic: 1/1 – 3/31; Premium: 4/1 – 6/30 |

Intervalidus provides composable multidimensional data structures, both mutable and
immutable, to address storage and management of data like this. For more information, see the
[full API documentation](https://rremple.github.io/intervalidus/latest/api/intervalidus)

### Usage:

You could use Intervalidus `Data` to represent the above as a one-dimensional structure that treats dates as 
discrete values like this:

```scala 3
import java.time.LocalDate.of as date
import intervalidus.DiscreteValue.given
import intervalidus.Interval1D.*
import intervalidus.mutable.Data

val plan1d = Data.of(intervalFrom(date(2024, 1, 1)) -> "Basic")
plan1d.set(intervalFrom(date(2024, 4, 1)) -> "Premium")
plan1d.remove(intervalFromAfter(date(2024, 6, 30)))
println(plan1d)
```

Which outputs a handy little Ascii Gantt of the valid values in the structure:

```text
| 2024-01-01 .. 2024-03-31 | 2024-04-01 .. 2024-06-30 |
| Basic                    |
                           | Premium                  |
```

Since `Data` is a [partial function](https://docs.scala-lang.org/scala3/book/fun-partial-functions.html),
you can query individual valid values using that interface (many other query
methods exist as well). For example, using function application:

```scala 3 
plan1d(date(2024, 3, 15))
```

will return `Basic` because the user had the Basic tier on 3/15.

We could have just as easily used the immutable variant of `Data`, and the output would have been the same as
the above output:

```scala 3
import java.time.LocalDate.of as date
import intervalidus.DiscreteValue.given
import intervalidus.Interval1D.*
import intervalidus.immutable.Data

val plan1d = Data
  .of(intervalFrom(date(2024, 1, 1)) -> "Basic")
  .set(intervalFrom(date(2024, 4, 1)) -> "Premium")
  .remove(intervalFromAfter(date(2024, 6, 30)))
println(plan1d)
```

On the other hand, you may want two dimensions of time: one to track what is effective and one to track when these facts
were known (sometimes referred to as [bitemporal modeling](https://en.wikipedia.org/wiki/Bitemporal_modeling)).
For that, you could use Intervalidus `Data` to represent the above as a two-dimensional structure like
this:

```scala 3
import java.time.LocalDate.of as date
import intervalidus.DiscreteValue.given
import intervalidus.Interval1D.*
import intervalidus.immutable.Data

val plan2d = Data
  .of((intervalFrom(date(2024, 1, 1)) x intervalFrom(date(2023, 12, 25))) -> "Basic")
  .set((intervalFrom(date(2024, 4, 1)) x intervalFrom(date(2024, 3, 15))) -> "Premium")
  .remove(intervalFromAfter(date(2024, 6, 30)) x intervalFrom(date(2024, 6, 28)))
println(plan2d)
```

Which results in the following (slightly less straightforward) output:

```text
| 2024-01-01 .. 2024-03-31         | 2024-04-01 .. 2024-06-30         | 2024-07-01 .. +∞                 |
| Basic [2023-12-25..2024-03-14]                                                                         |
| Basic [2024-03-15..+∞)           |
                                   | Premium [2024-03-15..2024-06-27]                                    |
                                   | Premium [2024-06-28..+∞)         |
```

Here, the second time dimension is shown next to each valid value. Reading this line by line, the interpretation is:

- From 12/25/2023 until 3/14, the user was known to have the Basic tier effective from 1/1, without any planned
  termination.
- From 3/15 and thereafter, the user was known to have the Basic tier effective only from 1/1 until 3/31.
- Also from 3/15 until 6/27, the user was known to have the Premium tier effective from 4/1, without any planned
  termination.
- From 6/28 and thereafter, the user was known to have the Premium tier effective only from 4/1 until 6/30.

Since decoding the `toString` output of `Data` can get complicated as more intervals are present, there is a utility
(in the test package) called `Visualize2D` that can help with debugging and testing tasks. It uses 2D graphics to render
the horizontal and vertical dimensions more clearly. For example, `Visualize2D(plan2d)` displays the following, which is a
bit easier to decipher:

![2D data visualization](/doc/intervalidus-visualize.png)

(A similar `Visualize3D` is provided for visualizing 3D data. It is a [Three.js](https://threejs.org/) app -- 100% 
vibe-coded using Gemini 2.5 Pro Preview 05-06. It renders the non-metric representation of data, allowing it
to be rotated, sliced, and understood.)

One might query this structure to find what the August forecast was at various sampled dates in
the past (or future). For example, leveraging `plan2d` as a partial function (with an `unapply`):

```scala 3
val futureEffectiveDate: Domain1D[LocalDate] = date(2024, 8, 1)
List(date(2023, 12, 15), date(2024, 1, 15), date(2024, 5, 15), date(2024, 7, 15)).foreach: knownDate => 
  futureEffectiveDate x knownDate match
    case plan2d(tier) => println(s"On $knownDate, forecasted $tier tier on $futureEffectiveDate")
    case _            => println(s"On $knownDate, no forecasted tier on $futureEffectiveDate")
```

The result shows how this August forecast changed over time:

```text
On 2023-12-15, no forecasted tier on 2024-08-01
On 2024-01-15, forecasted Basic tier on 2024-08-01
On 2024-05-15, forecasted Premium tier on 2024-08-01
On 2024-07-15, no forecasted tier on 2024-08-01
```

Going further, lets say tracking the known date is not enough. There are transactions that drive this new knowledge, 
and those transactions are timestamped. Then using an interval in the "knowledge" dimension based on a datetime rather
than a date would be more appropriate. But timestamps are better thought of as continuous rather than discrete values.
So we can use a continuous value in the knowledge dimension, and continue to use a discrete value in the effective date
dimension.

```scala 3
import java.time.LocalDate.of as date
import intervalidus.DiscreteValue.LocalDateDiscreteValue
import intervalidus.ContinuousValue.LocalDateTimeContinuousValue
import intervalidus.Interval1D.*
import intervalidus.immutable.Data

val plan2d = Data
  .of((intervalFrom(date(2024, 1, 1)) x intervalFrom(date(2023, 12, 25).atTime(10, 23, 33, 123456789))) -> "Basic")
  .set((intervalFrom(date(2024, 4, 1)) x intervalFrom(date(2024, 3, 15).atTime(14, 10, 15, 987654321))) -> "Premium")
println(plan2d)
```

Which results in the following output, which is similar to what was shown in the previous example:

```text
| 2024-01-01 .. 2024-03-31                                             | 2024-04-01 .. +∞                                                     |
| Basic [2023-12-25T10:23:33.123456789, 2024-03-15T14:10:15.987654321)                                                                        |
| Basic [2024-03-15T14:10:15.987654321, +∞)                            |
                                                                       | Premium [2024-03-15T14:10:15.987654321, +∞)                          |
```

The two main differences are:
- The timestamps are included in the knowledge dimension
- The notation for the intervals in this dimension is different, where the boundary of each interval can either be
  closed (denoted with brackets) or open (denoted with parens), and the start and end of each interval is separated by
  a comma rather than two dots. 

(How discrete and continuous domain values differ in behavior is discussed later.)

These notational differences are carried in the representation of the horizontal dimension too. For example, if we flip
the horizontal and vertical dimensions (using pattern matching) and print the result:

```scala 3
import intervalidus.Interval.Patterns.*

val plan2dFlip = plan2d.mapIntervals:
  case horizontal x_: vertical => vertical x horizontal
println(plan2dFlip)
```

The result shows how the continuous notation is pulled to the top and the discrete notation is used with each piece of 
effective-dated data:

```text
[ 2023-12-25T10:23:33.123456789, 2024-03-15T14:10:15.987654321 ) [ 2024-03-15T14:10:15.987654321, +∞ )                            |
| Basic [2024-01-01..2024-03-31]                                                                                                  |
| Basic [2024-04-01..+∞)                                         |
                                                                 | Premium [2024-04-01..+∞)                                       |
```

There is a sample billing application provided that demonstrates how both of these 1D and 2D structures could be used to 
support time-oriented logic, like billing, directly, including prospective calculation and retrospective
adjustments/refunds.

Sometimes one wants to treat multiple values as valid in the same interval. For example, a product team may add/remove
features in each numbered release of a product. `DataMulti` could be used to model what features belong in what
releases, this time using intervals based on integers rather than dates.
```scala 3
import intervalidus.DiscreteValue.given
import intervalidus.Interval1D.*
import intervalidus.immutable.DataMulti

case class Feat(id: String)

val multi = DataMulti.In1D[Feat,Int]()
  .addOne(intervalFrom(1) -> Feat("A")) // release 1 includes only feature A
  .addOne(intervalFrom(2) -> Feat("B")) // release 2 adds feature B
  .addOne(intervalFrom(3) -> Feat("C")) // release 3 adds feature C...
  .removeOne(intervalFrom(3) -> Feat("B")) // ...and also drops feature B
  // releases 4 and 5 were bug fixes, and no features were added or removed
  .addOne(intervalFrom(6) -> Feat("D")) // release 6 adds feature D...
  .removeOne(intervalFrom(6) -> Feat("A")) // ...and also drops feature A

println(multi(4)) // what are the features in release 4?
println
println(multi) // what are all the feature combinations in all release intervals?
```

The result shows the unique feature combinations in each release-based interval (note that releases 3, 4, and 5 share
the same feature set, i.e., the combination of feature A and C is "valid" in the [3..5] release interval):

```text
Set(Feat(A), Feat(C))

| 1 .. 1            | 2 .. 2            | 3 .. 5            | 6 .. +∞           |
| {Feat(A)}         |
                    | {Feat(A),Feat(B)} |
                                        | {Feat(A),Feat(C)} |
                                                            | {Feat(C),Feat(D)} |
```

For the same reasons explained earlier, one might want to use `DataMulti` instead to model what features belong in
what releases as well as when the product management decisions were made to include/exclude features in each release.

Another example might be representing [piecewise functions](https://en.wikipedia.org/wiki/Piecewise_function) coherently
by using a function type as the value where different function pieces are valid in different domain intervals. The reLU
([rectified linear unit](https://en.wikipedia.org/wiki/Rectifier_(neural_networks))) function is a simple piecewise
function which, given some `x`, returns `x` when `x` is greater than zero and zero otherwise. This can be expressed
directly using intervalidus to manage the function pieces as follows:

```scala 3
import intervalidus.ContinuousValue.given
import intervalidus.Interval1D.*
import intervalidus.immutable.Data

val reLU = new (Double => Double):
  private val pieceAt = Data[Double => Double, Domain.In1D[Double]]()
    .set(intervalFrom(0.0) -> identity)
    .set(intervalToBefore(0.0) -> (_ => 0.0))
  override def apply(d: Double): Double = pieceAt(d).apply(d)

println(s"reLU at -1 = ${reLU(-1)}") // reLU at -1 = 0
println(s"reLU at  0 = ${reLU( 0)}") // reLU at  0 = 0
println(s"reLU at  1 = ${reLU( 1)}") // reLU at  1 = 1
```

---

The same methods are available in both mutable/immutable varients (though parameter and
return types vary). See the [full API documentation](https://rremple.github.io/intervalidus/latest/api/intervalidus) for details on
these methods.

These query methods provide various data, difference, and Boolean results:

- `get` / `getOption` / `getAt` / `getDataAt` / `getAll` / `getIntersecting`
- `intersects`
- `foldLeft`
- `isEmpty`
- `domain` / `domainComplement`
- `values` / `intervals`
- `diffActionsFrom`

These methods return a new structure:

- `copy` / `toImmutable` / `toMutable`
- `zip` / `zipAll`
- `getByHeadIndex`

These mutation methods return a new structure when using immutable and `Unit` when using mutable:

- `remove` (`-`) / `removeMany` (`--`) / `removeValue`
- `replace` / `replaceByKey` / `update` / `merge`
- `set` (`+`) / `setMany` (`++`) / `setIfNoConflict` / `fill`
- `compress` / `compressAll` / `recompressAll`
- `filter`
- `map` / `mapValues` / `mapIntervals` / `collect` / `flatMap` (the immutable variant allows altering type parameters)
- `applyDiffActions` / `syncWith`

## Using and Extending

There is nothing remarkable about `LocalDate` and the other types used in the examples above. These are examples of
something called "domain values" over which a
"domain" can be defined. Domain values are finite with min/max values, an order, and an ordered hash function. There are
two kinds of domain values: discrete and continuous. Discrete values have methods for finding successors/predecessors
where continuous values do not. It is the domain built on top of the domain value that is used to define "interval"
boundaries. Intervals using continuous domain values have boundaries that are either open or closed where those using
discrete domain boundaries (apart from -∞ and +∞) are always closed. Note that domains and
intervals with more than one dimension may include both discrete and continuous dimensions, as was shown in an example
earlier.

Discrete and continuous domain values have different notions of adjacency.

- Two elements of a domain over a set of discrete values are adjacent only if one of the elements is the successor (or
  predecessor) of the other. For example, in a domain over discrete integers, `domain(1)` is adjacent to `domain(2)`
  because there are no domain elements between `1` and `2`. The general notions of `leftAdjacent` (i.e., before) and
  `rightAdjacent` (i.e., after) are based on the `successorOf` and `predecessorOf` methods of the discrete value type
  class respectively.

- Two elements of a domain over a set of continuous values are adjacent only if the two elements have the same value and
  one is open and the other is closed. For example, in a domain over continuous doubles, `domain(1.0)` is adjacent to
  `open(1.0)` because there are no domain elements between them. The general notions of `leftAdjacent` (i.e., before)
  and `rightAdjacent` (i.e., after) yield the same result for a point: open if closed, and closed if open.

A discrete value is a [type class](https://docs.scala-lang.org/scala3/book/ca-type-classes.html),
and there are implementations given for the following types:

- `Int`
- `Long`
- `LocalDate`
- `BigInteger`

A continuous value is also a type class, and there are implementations given for the following types:

- `Double`
- `LocalDateTime`
- `Int`
- `Long`
- `LocalDate`

But if you have your own type with these properties, you can certainly give a `DiscreteValue` or `ContinuousValue` type
class definition for that type and use it in the definition of intervals, etc. To motivate
an example where this makes sense, first consider a structure that represents the different colors of visible light as 
intervals of integer-valued nanometer wavelengths.

```scala 3
import intervalidus.DiscreteValue.given
import intervalidus.Interval1D.*
import intervalidus.immutable.Data

enum Color:
  case Violet, Indigo, Blue, Green, Yellow, Orange, Red

val colorByWavelength = Data[Color, Domain.In1D[Int]]()
  .set(intervalFrom(380) -> Color.Violet)
  .set(intervalFrom(450) -> Color.Blue)
  .set(intervalFrom(495) -> Color.Green)
  .set(intervalFrom(570) -> Color.Yellow)
  .set(intervalFrom(590) -> Color.Orange)
  .set(intervalFrom(620) -> Color.Red)
  .remove(intervalFrom(750))

println(colorByWavelength)
```
This prints the following (rather sad) representation of a rainbow.
```text
| 380 .. 449 | 450 .. 494 | 495 .. 569 | 570 .. 589 | 590 .. 619 | 620 .. 749 |
| Violet     |
             | Blue       |
                          | Green      |
                                       | Yellow     |
                                                    | Orange     |
                                                                 | Red        |
```
As shown earlier, the structure can be interrogated to get the color of a specific integer wavelength.
```scala 3
println(colorByWavelength(480)) // prints "Blue"
```
Say we also want to track our thoughts about colors. We could make use of a similar structure that associates thoughts
with these same integer-valued wavelength intervals like so.
```scala 3
val thoughtsByWavelength = Data[String, Domain.In1D[Int]]()
  .set(intervalFrom(380).toBefore(570) -> "I like violet, blue, and green")
  .set(intervalFrom(620).to(750) -> "I like this too")
println(thoughtsByWavelength)
```
Although this does track the information we need to track, it seems burdensome to have to know the wavelengths of the
noted colors, and too fine-grained. Also, unless we include them in the noted thoughts, we no longer see any color 
names in the output, only wavelengths, which makes things a bit too cryptic.
```text
| 380 .. 569                     | 570 .. 619                     | 620 .. 750                     |
| I like violet, blue, and green |                                | I like this too                |
```

A more elegant approach would be to use intervals based on the colors themselves rather than their associated
wavelengths. All we have to do is give a `DiscreteValue` type class definition for `Color` and then we can use it as the
basis for discrete intervals.

```scala 3
given DiscreteValue[Color] with
  override val minValue: Color = Color.values.head // Violet
  override val maxValue: Color = Color.values.last // Red

  override def predecessorOf(x: Color): Option[Color] =
    if x == minValue then None else Some(Color.fromOrdinal(x.ordinal - 1))
  override def successorOf(x: Color): Option[Color] =
    if x == maxValue then None else Some(Color.fromOrdinal(x.ordinal + 1))

  override def compare(lhs: Color, rhs: Color): Int = lhs.ordinal.compareTo(rhs.ordinal)
  override def orderedHashOf(x: Color): Double = x.ordinal

val thoughtsByColor = Data[String, Domain.In1D[Color]]()
  .set(intervalFrom(Color.Violet).to(Color.Green) -> "I like violet, blue, and green")
  .set(intervalAt(Color.Red) -> "I like this too")
println(thoughtsByColor)
```
This associates our thoughts with color-based intervals rather than a wavelength-based intervals, with the following
output.
```text
| Violet .. Green                | Yellow .. Orange               | Red .. Red                     |
| I like violet, blue, and green |
                                                                  | I like this too                |
```
And this structure can be interrogated to get the thought for a specific color without knowing anything about wavelengths.
```scala 3
println(thoughtsByColor(Color.Blue)) // prints "I like violet, blue, and green"
```

Because creating a `DiscreteValue` type class definition based on a finite sequence of unique items is a common need, a
helper method `fromSeq` is provided that reduces boilerplate code. The following code is equivalent to what is shown 
above.

```scala 3
enum Color:
  case Violet, Indigo, Blue, Green, Yellow, Orange, Red

given DiscreteValue[Color] = DiscreteValue.fromSeq(Color.values.toIndexedSeq)

val thoughtsByColor = Data[String, Domain.In1D[Color]]() //...
```

Or, even briefer, most enums can have the discrete value 
[type class derived automatically](https://docs.scala-lang.org/scala3/reference/contextual/derivation.html)
(some restrictions apply, 
e.g., the enum cannot be declared inside a function).
```scala 3
enum Color derives DiscreteValue:
  case Violet, Indigo, Blue, Green, Yellow, Orange, Red

val thoughtsByColor = Data[String, Domain.In1D[Color]]() //...
```

You can extend through composition. For example `DataVersioned` mimics the `Data` API but uses an underlying `Data`
structure of a higher dimension (i.e., with an additional integer head dimension) to create a versioned data structure.
The "current" version is tracked as internal state and methods accept version selection as a
[context parameter](https://docs.scala-lang.org/scala3/book/ca-context-parameters.html), with "current" as the
default version selection applied. Also, a notion of approval is supported by specifying a specific future version for
anything unapproved.

You can also extend through object-oriented inheritance. For example `DataMulti` extends the underlying class hierarchy
of normal `Data` structures to provide multimap-like
capabilities. The inherited components store and operate on sets of values rather than individual values, which allows
multiple values to be valid in the same interval. When queried, values are returned as sets. There are also `add` and
`remove` methods which allow mutation of individual values across intervals, and a `merge` method for combining two
structures (conceptually similar to `zip`, but operating on individual values, and more appropriate for a multi-value
structure).

## Software Structure

Below is the class diagram for the core bits of Intervalidus:

![core class diagram](/doc/intervalidus-core.svg)

Intervalidus supports data in an arbitrary number of dimensions by leveraging Scala 3's
[generic programming with tuples](https://www.scala-lang.org/2021/02/26/tuples-bring-generic-programming-to-scala-3.html). 
The only dimension-specific classes are the base 1D cases of domains and intervals. A more generic notion of a
`DomainLike` tuple underpins multidimensional support across all Intervalidus data structures. You may never need more 
than three or four dimensions, but if you do, the support is available to you (but remember: flatter is faster).

Furthermore, the definitions and implementations of methods across mutable/immutable variants of `Data` and `DataMulti` 
have been made as generic as possible to avoid repetitive code/scaladoc (DRY). However, this can make it
harder to navigate to methods. (Although this was a larger issue back when Intervalidus had separate class hierarchies
for each dimension supported.) The following (rather unorthodox) diagram shows where to find each method in a
kind of Venn-like way, where overlaps indicate a definition (and documentation) is in the lower trait with the
implementation in the higher, inheriting trait/class:

![core trait stack diagram](/doc/intervalidus-trait-stack.svg)

## Internals and extras

Both the mutable and immutable variants of `Data`, `DataMulti`, and `DataVersioned` use three mutable data
structures internally for managing state, two of which are custom (in the `collection` subproject):

- A mutable standard library `TreeMap`, ordered by the start of each interval. This allows for fast in-order retrieval
  in methods like `getAll`, and is essential for deterministic results in methods like `foldLeft` that use `getAll`.

- A mutable multimap that associates each value with all the intervals in which that value is valid, ordered by the
  start of each interval. This speeds up operations like `compress`, which improves performance for all mutation
  operations that use `compress`. Intervalidus uses its own compact implementation of a `MultiMapSorted` (based on
  standard library `Map` and `SortedSet`) which is somewhat similar to `SortedMultiDict` in `scala-collection-contrib`,
  but returns the _values_ in order, not the _keys_. Having values in order is essential for deterministic results from
  `compress` and all mutation operations relying on `compress`. Only the mutable variant is used internally, but an
  immutable variant is also provided. Note that the sample billing application uses this multimap directly for managing
  customer transactions.

- A "box search tree", which is a hyperoctree (i.e., a B-tree, quadtree, octree, etc., depending on the dimension) that
  supports quick
  retrieval by interval. Box search trees manage "boxed" data in multidimensional double space. Unlike classic
  spacial search trees (used in collision detection and the like), these data structures manage "boxes" rather than
  individual points, where boxes are split and stored in all applicable subtrees (hyperoctants) of the data structure
  as subtrees are
  split. Intervalildus uses the ordered hashes defined on domain components of intervals to approximate all
  domain intervals as boxes in double space, and then manages valid data associated with these boxes in the box
  search tree. This not only results in dramatically faster retrieval (e.g., `getAt` and `getIntersecting`), since many
  mutation operations use intersection retrieval in their own logic, they are made dramatically faster as well. Only the
  mutable variant is used internally, but an immutable variant is also provided.

Although the custom multimap and box search tree data structures were built for internal use, they may be useful outside
the Intervalidus context. As described above, there are both immutable and mutable variants of each data structure, as
shown in the following diagram:

![box tree and multimap diagram](/doc/intervalidus-box-tree-multimap.svg)

Again, looking at method definition/implementation in a Venn-like way, here's the full API for a `BoxTree` and related
objects:

![box tree trait stack diagram](/doc/intervalidus-trait-stack-box-tree.svg)

Note that box search trees are tunable via environment variables.

- The default capacity of leaf nodes is 256, which was found to be optimal in micro-benchmarks. This can be overridden
  by setting the environment variable `INTERVALIDUS_TREE_NODE_CAPACITY`.

- The default depth limit of trees is 32, which was found to be optimal in micro-benchmarks. This can be overridden by
  setting the environment variable `INTERVALIDUS_TREE_DEPTH_LIMIT`.

Apart from `collection`, there are a few other subprojects that are worth mentioning:

- As described earlier, in the `intervalidus-examples` subproject there is a sample billing application that shows how
  Intervalidus structures can be used to support time-oriented logic like billing directly.

- Also in the `intervalidus-examples` subproject there is a sample rules-based application that shows how Intervalidus
  structures can be used to capture the outcomes of patients in a clinical trial. It uses a toy rules engine to
  interpret daily measurements, and Intervalidus to capture progress concisely.

- The above example leverages a toy rules engine that is available in a separate subproject: `intervalidus-tinyrule`.
  (This is not strictly related to Intervalidus, so it is tucked under a directory called `sidequests`.) It uses some
  interesting generic and metaprogramming features of Scala 3 to represent and process facts.

- There is a separate `bench` subproject that leverages the Java Microbenchmark Harness (jmh) framework to benchmark
  methods, including relative performance of experimental vs. non-experimental features.

- There are sample JSON pickling subprojects `intervalidus-upickle` and `intervalidus-weepickle` which could be useful
  when managing Intervalidus data in a JSON data store (e.g. MongoDB) and/or serializing data through web services.
  (There are many JSON frameworks, and one could use these subprojects as a starting point to add support for others.)

Lastly, there is a context parameter component used to enable/disable experimental features (a.k.a., feature flagging)
called `Experimental`. The default implementation given disables all experimental features. But one can enable something
experimental simply by giving an alternative implementation of `Experimental` when the structure is constructed.

E.g., consider the requirement for intervals to be disjoint (i.e., non-overlapping) in all valid data. All mutation
operations (e.g. `set`) maintain this invariant automatically, but one could still construct a structure incorrectly
with data that are not disjoint to start (which will cause Intervalidus to be weirdly unpredictable). Or maybe
Intervalidus has a bug, and some mutation operation isn't maintaining the invariant. Although constantly checking for
disjointedness is a huge performance burden -- especially for immutable structures and/or higher-dimensional
structures -- it may be worth doing during initial development and testing. So there is an experimental feature called
**"requireDisjoint"** that, if set, will validate the disjointedness of all data with every construction.

```scala 3
import intervalidus.DiscreteValue.given
import intervalidus.Interval1D.*
import intervalidus.immutable.Data
import java.time.LocalDate.of as date

given Experimental = Experimental("requireDisjoint")

val plan1d = Data(
  Seq(
    intervalFrom(date(2024, 4, 1)) -> "Premium",
    intervalFrom(date(2024, 1, 1)) -> "Basic" // <-- wrong, throws an IllegalArgumentException: requirement failed: data must be disjoint
    // interval(date(2024, 1, 1), date(2024, 3, 31)) -> "Basic" // <-- right, does not throw
  )
)
```

Many experimental features such as **"noSearchTree"** and **"noBruteForceUpdate"** have come and gone. 
Existing experimental features that can be toggled are:

- **"requireDisjoint"** This is described above.

- **"printExperimental"** This feature simply prints a line when an experimental feature is/isn't being used. Useful if
  there is uncertainty around if the context parameter is being set and passed along correctly in the correct scope.
