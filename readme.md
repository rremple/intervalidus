# Intervalidus

## In what intervals are your data valid?

A Scala library with zero dependencies for representing data as valid only in discrete or continuous intervals, one-,
two-, and three-dimensional. This seems to come up a lot in application design both in terms of effective dating and
versioning data.

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
in libraries for representing validity as a kind of collection.

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

Intervalidus provides composable data structures -- one-, two-, and three-dimensional, both mutable and
immutable -- to address storage and management of data like this. For more information, see the
[full API documentation](https://rremple.github.io/intervalidus/latest/api/intervalidus)

### Usage:

You could use Intervalidus `DataIn1D` to represent the above as a one-dimensional structure that treats dates as 
discrete values like this:

```scala 3
import intervalidus.DiscreteValue.given
import intervalidus.Interval1D.*
import intervalidus.mutable.DataIn1D
import java.time.LocalDate.of as date

val plan1d = DataIn1D.of(intervalFrom(date(2024, 1, 1)) -> "Basic")
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

Since `DataIn1D` is a partial function, you can query individual valid values using that interface (many other query
methods exist as well). For example, using function application:

```scala 3 
plan1d(date(2024, 3, 15))
```

will return `Basic` because the user had the Basic tier on 3/15.

We could have just as easily used the immutable variant of `DataIn1D`, and the output would have been the same as
the above output:

```scala 3
import intervalidus.DiscreteValue.given
import intervalidus.Interval1D.*
import intervalidus.immutable.DataIn1D

val plan1d = DataIn1D
  .of(intervalFrom(date(2024, 1, 1)) -> "Basic")
  .set(intervalFrom(date(2024, 4, 1)) -> "Premium")
  .remove(intervalFromAfter(date(2024, 6, 30)))
println(plan1d)
```

On the other hand, you may want two dimensions of time: one to track what is effective and one to track when these facts
were known. For that, you could use Intervalidus `DataIn2D` to represent the above as a two-dimensional structure like
this:

```scala 3
import intervalidus.DiscreteValue.given
import intervalidus.Interval1D.*
import intervalidus.immutable.DataIn2D

val plan2d = DataIn2D
  .of((intervalFrom(date(2024, 1, 1)) x intervalFrom(date(2023, 12, 25))) -> "Basic")
  .set((intervalFrom(date(2024, 4, 1)) x intervalFrom(date(2024, 3, 15))) -> "Premium")
  .remove(intervalFromAfter(date(2024, 6, 30)) x intervalFrom(date(2024, 6, 28)))
println(plan2d)
```

Which results in the following (slightly less straightforward) output:

```text
| 2024-01-01 .. 2024-03-31         | 2024-04-01 .. 2024-06-30         | 2024-07-01 .. +∞            |
| Basic [2023-12-25..2024-03-14]                                                                    |
                                   | Premium [2024-03-15..2024-06-27]                               |
| Basic [2024-03-15..+∞)           |
                                   | Premium [2024-06-28..+∞)         |
```

Here, the second time dimension is shown next to each valid value. Reading this line by line, the interpretation is:

- From 12/25/2023 until 3/14, the user was known to have the Basic tier effective from 1/1, without any planned
  termination.
- From 3/15 until 6/27, the user was known to have the Premium tier effective from 4/1.
- Also from 3/15 and thereafter, the user was known to have the Basic tier effective only from 1/1 until 3/31.
- From 6/28 and thereafter, the user was known to have the Premium tier effective only from 4/1 until 6/30.

Since decoding the `toString` output of `DataIn2D` can get complicated as more intervals are present, there is a utility
(in the test package) called `Visualize` that can assist with debugging and testing tasks. It uses 2D graphics to render
the horizontal and vertical dimensions more clearly. For example, `Visualize(plan2d)` displays the following, which is a
bit easier to decipher:

![2D data visualization](/doc/intervalidus-visualize.png)

One might query this structure to find what was known about expected August effective tiers at various sampled dates in
the past or future. For example, leveraging `plan2d` as a partial function (with an `unapply`):

```scala 3
val futureEffectiveDate: Domain1D[LocalDate] = date(2024, 8, 1)
List(date(2023, 12, 15), date(2024, 1, 15), date(2024, 5, 15), date(2024, 7, 15)).foreach: knownDate => 
  futureEffectiveDate x knownDate match
    case plan2d(tier) => println(s"On $knownDate, expected $tier tier on $futureEffectiveDate")
    case _            => println(s"On $knownDate, no expected tier on $futureEffectiveDate")
```

The result shows how what is known about this expected future effectivity changed over time:

```text
On 2023-12-15, no expected tier on 2024-08-01
On 2024-01-15, expected Basic tier on 2024-08-01
On 2024-05-15, expected Premium tier on 2024-08-01
On 2024-07-15, no expected tier on 2024-08-01
```

Going further, lets say tracking the known date is not enough. There are transactions that drive this new knowledge, 
and those transactions are timestamped. Then using an interval in the "knowledge" dimension based on a datetime rather
than a date would be more appropriate. But timestamps are better thought of as continuous rather than discrete values.
So we can use a continuous value in the knowledge dimension, and continue to use a discrete value in the effective date
dimension.

```scala 3
import intervalidus.DiscreteValue.LocalDateDiscreteValue
import intervalidus.ContinuousValue.LocalDateTimeContinuousValue
import intervalidus.Interval1D.*
import intervalidus.immutable.DataIn2D

val plan2d = DataIn2D
  .of((intervalFrom(date(2024, 1, 1)) x intervalFrom(date(2023, 12, 25).atTime(10, 23, 33, 123456789))) -> "Basic")
  .set((intervalFrom(date(2024, 4, 1)) x intervalFrom(date(2024, 3, 15).atTime(14, 10, 15, 987654321))) -> "Premium")
println(plan2d)
```

Which results in the following output, which is similar to what was shown in the previous example:

```text
| 2024-01-01 .. 2024-03-31                                             | 2024-04-01 .. +∞                             |
| Basic [2023-12-25T10:23:33.123456789, 2024-03-15T14:10:15.987654321)                                                |
| Basic [2024-03-15T14:10:15.987654321, +∞)                            |
                                                                       | Premium [2024-03-15T14:10:15.987654321, +∞)  |
```

The two main differences are:
- The timestamps are included in the knowledge dimension
- The notation for the intervals in this dimension is different, where the boundary of each interval can either be
  closed (denoted with brackets) or open (denoted with parens), and the start and end of each interval is separated by
  a comma rather than two dots. 

These notational differences are carried in the representation of the horizontal dimension too. For example, if we flip
the dimensions and print the result:

```scala 3
println(plan2d.flip)
```

The result shows how the continuous notation is pulled to the top and the discrete notation is used with each piece of 
effective-dated data:

```text
[ 2023-12-25T10:23:33.123456789, 2024-03-15T14:10:15.987654321 ) [ 2024-03-15T14:10:15.987654321, +∞ )                |
                                                                 | Basic [2024-01-01..2024-03-31]                     |
| Basic [2024-01-01..+∞)                                         |
                                                                 | Premium [2024-04-01..+∞)                           |
```

There is a sample billing application provided that demonstrates how both of these 1D and 2D structures could be used to 
support time-oriented logic, like billing, directly, including prospective calculation and retrospective
adjustments/refunds.

Sometimes one wants to treat multiple values as valid in the same interval. For example, a product team may add/remove
features in each numbered release of a product. `DataIn1DMulti` could be used to model what features belong in what
releases, this time using intervals based on integers rather than dates.
```scala 3
import intervalidus.Interval1D.*
import intervalidus.immutable.DataIn1DMulti

case class Feat(id: String)

val multi = DataIn1DMulti[Feat, Int]()
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
the same feature set, i.e., the combination of feature A and C is "valid" in all in the [3..5] release interval):

```text
Set(Feat(A), Feat(C))

| 1 .. 1            | 2 .. 2            | 3 .. 5            | 6 .. +∞           |
| {Feat(A)}         |
                    | {Feat(A),Feat(B)} |
                                        | {Feat(A),Feat(C)} |
                                                            | {Feat(C),Feat(D)} |
```

For the same reasons explained earlier, one might want to use `DataIn2DMulti` instead to model what features belong in
what release as well as when the product management decision was made to include/exclude a feature in a release.

The same methods are available in both mutable/immutable and one-/two-/three-dimensional forms (though parameter and
return types vary). See the [full API documentation](https://rremple.github.io/intervalidus/latest/api/intervalidus) for details on
these methods.

These query methods provide various data, difference, and Boolean results:

- `get` / `getOption` / `getAt` / `getDataAt` / `getAll` / `getIntersecting`
- `intersects`
- `foldLeft`
- `isEmpty`
- `domain`
- `diffActionsFrom`

These methods return a new structure:

- `copy` / `toImmutable` / `toMutable`
- `zip` / `zipAll`
- `flip` (only available on 2D)
- `flipAboutHorizontal` / `flipAboutVertical` / `flipAboutDepth` /  (only available on 3D)
- `getByHorizontalIndex` / `getByVerticalIndex` (only available on 2D and 3D)
- `getByDepthIndex` (only available on 3D)

These mutation methods return a new structure when using immutable and `Unit` when using mutable:

- `remove`
- `replace` / `replaceByKey`
- `set` / `setIfNoConflict`
- `update` / `fill` / `merge`
- `compress` / `compressAll` / `recompressAll`
- `filter`
- `map` / `mapValues`
- `flatMap`
- `applyDiffActions` / `syncWith`

## Using and Extending

There is nothing remarkable about `LocalDate` here. It is an example of something called a "domain value" upon which a
"domain" can be defined. Domain values are finite with min/max values, an order, and an ordered hash function. There are
two kinds of domain values: discrete and continuous. Discrete values have methods for finding successors/predecessors
where continuous values do not. It is the domain built on top of the domain value that is used to define "interval"
boundaries. Intervals using a continuous domain values have boundaries that are either open or closed. Note that
intervals with more than one dimension may include both discrete and continuous dimensions, as was shown in an example
earlier.

A discrete value is a type class, and there are implementations given for the following types:

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
class definition for that type and use it in the definition of one-, two-, or three-dimensional intervals. To motivate
an example where this makes sense, first consider a structure that represents the different colors of visible light as 
intervals of integer-valued nanometer wavelengths.

```scala 3
import intervalidus.DiscreteValue.given
import intervalidus.Interval1D.*
import intervalidus.immutable.DataIn1D

enum Color:
  case Violet, Indigo, Blue, Green, Yellow, Orange, Red

val colorByWavelength = DataIn1D[Color, Int]()
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
val thoughtsByWavelength = DataIn1D[String, Int]()
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

val thoughtsByColor = DataIn1D[String, Color]()
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

val thoughtsByColor = DataIn1D[String, Color]() //...
```

You can extend through composition. For example `DataIn1DVersioned` mimics the `DataIn1D` API but uses an
underlying `DataIn2D` structure with an integer vertical dimension to create a versioned data structure. The "current"
version is tracked as internal state and methods accept version selection as a context parameter, with "current" as the
default version selection applied. Also, a notion of approval is supported by specifying a specific future version for
anything unapproved. Similarly, there is a `DataIn2DVersioned` mimicking the `DataIn2D` API using an
underlying `DataIn3D` structure.

You can also extend through object-oriented inheritance. For example `DataIn1DMulti`, `DataIn2DMulti`, and
`DataIn3DMulti` extend the underlying class hierarchy of normal 1D, 2D, and 3D structures to provide multimap-like
capabilities. The inherited components store and operate on sets of values rather than individual values, which allows
multiple values to be valid in the same interval. When queried, values are returned as sets. There are also `add` and
`remove` methods which allow mutation of individual values across intervals, and a `merge` method for combining two
structures (conceptually similar to `zip`, but operating on individual values, and more appropriate for these multiple
values structures).

## Software Structure

Below is the class diagram for the core bits of Intervalidus
(three-dimensional is not shown, but it is very similar to two-dimensional):

![core class diagram](/doc/intervalidus-core.svg)

As described above, `DataIn1DVersioned` and `DataIn2DVersioned` leverage the core classes to provide specific
functionality you might want when versioning data (such as approval). Below is the class diagram for them:

![versioned class diagram](/doc/intervalidus-versioned.svg)

Lastly, the definitions and implementations of methods across mutable/immutable and one-/two-/three-dimensional 
variants have been made as generic as possible to avoid repetitive code/scaladoc (DRY). However, this can make it
harder to navigate to these methods. The following (rather unorthodox) diagram shows where to find each method in a
kind of Venn-like way, where overlaps indicate a definition (and documentation) is in the lower trait with the
implementation in the higher, inheriting trait/class
(three-dimensional is not shown, but it is very similar to two-dimensional):

![trait stack diagram](/doc/intervalidus-trait-stack.svg)

And the definitions and implementations of methods across the common structural elements have been made similarly
generic:

![trait stack structural diagram](/doc/intervalidus-trait-stack-structural.svg)

## Internals and extras

Both the mutable and immutable variants of `DataIn#` (where `#` is `1D`, `2D`, and `3D`) use three mutable data
structures internally for managing state, two of which are custom:

- A mutable standard library `TreeMap`, ordered by the start of each interval. This allows for fast in-order retrieval
  in methods like `getAll`, and is essential for deterministic results in methods like `foldLeft` that use `getAll`.

- A mutable multimap that associates each value with all the intervals in which that value is valid, ordered by the
  start of each interval. This speeds up operations like `compress`, which improves performance for all mutation
  operations that use `compress`. Intervalidus uses its own compact implementation of a `MultiMapSorted` (based on
  standard library `Map` and `SortedSet`) which is somewhat similar to `SortedMultiDict` in `scala-collection-contrib`,
  but returns the _values_ in order, not the _keys_. Having values in order is essential for deterministic results from
  `compress` and all mutation operations relying on `compress`. Only the mutable variant is used internally, but an
  immutable variant is also provided.

- A "box search tree" -- like a B-tree, quadtree, or octree, depending on the dimension -- that supports quick
  retrieval by interval. Box search trees manage boxed data structures in multidimensional double space. Unlike classic
  spacial search trees (used in collision detection and the like), these data structures manage "boxes" rather than
  individual points, where boxes are split and stored in all applicable subtrees of the data structure as subtrees are
  split. Intervalildus uses the ordered hashes defined on domain components of intervals to approximate all
  domain intervals as boxes in double space, and then manages valid data associated with these boxes in the box
  search tree. This not only results in dramatically faster retrieval (e.g., `getAt` and `getIntersecting`), since many
  mutation operations use intersection retrieval in their own logic, they are made dramatically faster as well. Only the
  mutable variant is used internally, but an immutable variant is also provided.

Although the custom multimap and box search tree data structures were built for internal use, they may be useful outside
the Intervalidus context. As described above, there are both immutable and mutable variants of each data structure, as
shown in the following diagram:

![box search tree and multimap diagram](/doc/intervalidus-box-tree-multimap.svg)

Note that box search trees are tunable via environment variables.

- The default capacity of leaf nodes is 256, which was found to be optimal in micro-benchmarks. This can be overridden
  by setting the environment variable `INTERVALIDUS_TREE_NODE_CAPACITY`.

- The default depth limit of trees is 32, which was found to be optimal in micro-benchmarks. This can be overridden by
  setting the environment variable `INTERVALIDUS_TREE_DEPTH_LIMIT`.

There are a few other subprojects that are worth mentioning:

- As described earlier, in the `intervalidus-examples` subproject there is a sample billing application that shows how
  Intervalidus structures can be used to support time-oriented logic like billing directly.

- Also in the `intervalidus-examples` subproject there is a sample rules-based application that shows how Intervalidus
  structures can be used to capture the outcomes of patients in a clinical trial. It uses a toy rules engine rules to
  interpret daily measurements, and Intervalidus to capture progress in a concise way.

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
import intervalidus.immutable.DataIn1D
import java.time.LocalDate.of as date

given Experimental = Experimental("requireDisjoint")

val plan1d = DataIn1D(
  Seq(
    intervalFrom(date(2024, 4, 1)) -> "Premium",
    intervalFrom(date(2024, 1, 1)) -> "Basic" // <-- wrong, throws an IllegalArgumentException: requirement failed: data must be disjoint
    // interval(date(2024, 1, 1), date(2024, 3, 31)) -> "Basic" // <-- right, does not throw
  )
)
```

Other experimental features that can be toggled are:

- **"printExperimental"** This feature simply prints a line when an experimental feature is/isn't being used. Useful if
  there is uncertainty around if the context parameter is being set and passed along correctly in the correct scope.

- **"noSearchTree"** Before adding support for higher-dimensional data, Intervalidus used a second reversed `TreeMap`
  for interval retrieval of 1D data. Because of performance issues in higher dimensions, this was replaced with the "box
  search tree" described above. Enabling this feature reverts Intervalidus to use the old `TreeMap` instead. This could
  actually help performance in some limited circumstances, e.g., if all the structures being used are one-dimensional.
  So it may be useful to toggle when micro-benchmarking the client app.

- **"bruteForceUpdate"** It was easy to specify all the cases directly for removing the intersection of an interval with
  all existing intervals in one dimension: there are only a few cases. In two dimensions it got more complicated, and
  even more so in three dimensions. There is a simpler brute force approach (code is about 10x shorter in 3D!) that
  eliminates all this complexity. Unfortunately it runs 2x - 5x slower in micro-benchmarks. It is unlikely this
  feature would be useful to anyone except an Intervalidus committer trying to close this performance gap -- it would be
  really nice if the shorter code was also the faster code!
