# Intervalidus

## In what intervals are your data valid?

A Scala library with zero dependencies for representing data as valid only in discrete intervals, one-, two-, and
three-dimensional. This seems to come up a lot in application design both in terms of effective dating and versioning
data.

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

You could use Intervalidus `DataIn1D` to represent the above as a one-dimensional structure like this:

```scala
import LocalDate.of as date

import intervalidus.DiscreteInterval1D.*
import intervalidus.mutable.DataIn1D

val plan1d = DataIn1D.of(intervalFrom(date(2024, 1, 1)) -> "Basic")
plan1d.set(intervalFrom(date(2024, 4, 1)) -> "Premium")
plan1d.remove(intervalFrom(date(2024, 6, 30).successor))
println(plan1d)
```

Which outputs a handy little Ascii Gantt of the valid values in the structure:

```
| 2024-01-01 .. 2024-03-31 | 2024-04-01 .. 2024-06-30 |
| Basic                    |
                           | Premium                  |
```

Since `DataIn1D` is a partial function, you can query individual valid values using that interface (many other query
methods exist as well). For example, using function application:

```scala 
plan1d(date(2024, 3, 15))
```

will return `Basic` because the user had the Basic tier on 3/15.

We could have just as easily used the immutable variant of `DataIn1D`, and the output would have been the same as
the above output:

```scala
import intervalidus.immutable.DataIn1D

val plan1d = DataIn1D
  .of(intervalFrom(date(2024, 1, 1)) -> "Basic")
  .set(intervalFrom(date(2024, 4, 1)) -> "Premium")
  .remove(intervalFrom(date(2024, 6, 30).successor))
println(plan1d)
```

On the other hand, you may want two dimensions of time: one to track what is effective and one to track when these facts
were known. For that, you could use Intervalidus `DataIn2D` to represent the above as a two-dimensional structure like
this:

```scala
import intervalidus.immutable.DataIn2D

val plan2d = DataIn2D
  .of((intervalFrom(date(2024, 1, 1)) x intervalFrom(date(2023, 12, 25))) -> "Basic")
  .set((intervalFrom(date(2024, 4, 1)) x intervalFrom(date(2024, 3, 15))) -> "Premium")
  .remove(intervalFrom(date(2024, 6, 30).successor) x intervalFrom(date(2024, 6, 28)))
println(plan2d)
```

Which results in the following (slightly less straightforward) output:

```
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

```scala
val futureEffectiveDate: DiscreteDomain1D[LocalDate] = date(2024, 8, 1)
List(date(2023, 12, 15), date(2024, 1, 15), date(2024, 5, 15), date(2024, 7, 15)).foreach: knownDate => 
  futureEffectiveDate x knownDate match
    case plan2d(tier) => println(s"On $knownDate, expected $tier tier on $futureEffectiveDate")
    case _            => println(s"On $knownDate, no expected tier on $futureEffectiveDate")
```

The result shows how what is known about this expected future effectivity changed over time:

```
On 2023-12-15, no expected tier on 2024-08-01
On 2024-01-15, expected Basic tier on 2024-08-01
On 2024-05-15, expected Premium tier on 2024-08-01
On 2024-07-15, no expected tier on 2024-08-01
```

The same methods are available in both mutable/immutable and one-/two-/three-dimensional forms (though parameter and
return types vary). See the [full API documentation](https://rremple.github.io/intervalidus/latest/api/intervalidus) for details on
these methods.

These query methods provide various data, difference, and Boolean results:

- `get` / `getOption` / `getAt` / `getAll` / `getIntersecting`
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
- `update` / `fill`
- `compress` / `compressAll` / `recompressAll`
- `filter`
- `map` / `mapValues`
- `flatMap`
- `applyDiffActions` / `syncWith`

## Using and Extending

There is nothing remarkable about `LocalDate` here. It is an example of something called a "discrete value" upon which a
"discrete domain" can be defined. Discrete values are finite with min/max values, they have an ordering, and they have 
methods for finding successors/predecessors. It is the "discrete domain" that is used to define "discrete intervals".
A discrete value is a type class, and there are implementations given for the following types:

- `Int`
- `Long`
- `LocalDate`
- `BigInteger`

But if you have your own type with these properties, you can certainly give an implementation of the type class for that
type and use it in the definition of one-, two-, or three-dimensional intervals. For example, these intervals use 
colors instead of dates or numbers:
```scala
import intervalidus.DiscreteInterval1D.*
import intervalidus.immutable.DataIn1D

enum Color:
  case Red, Yellow, Green, Cyan, Blue, Magenta

given DiscreteValue[Color] with
  override val minValue: Color = Color.values.head

  override val maxValue: Color = Color.values.last

  override def predecessorOf(x: Color): Option[Color] =
    if x == minValue then None else Some(Color.fromOrdinal(x.ordinal - 1))

  override def successorOf(x: Color): Option[Color] =
    if x == maxValue then None else Some(Color.fromOrdinal(x.ordinal + 1))

  override def compare(lhs: Color, rhs: Color): Int = lhs.ordinal.compareTo(rhs.ordinal)
  
  override def orderedHashOf(x: T): Double = x.ordinal

val color1d = DataIn1D
  .of(intervalTo(Color.Green) -> "Red, Yellow, Green")
  .set(intervalAt(Color.Cyan) -> "just Cyan")
  .set(intervalFrom(Color.Blue) -> "Blue, Magenta")

println(color1d)
```

This prints the following:
```
| -∞ .. Green        | Cyan .. Cyan       | Blue .. +∞         |
| Red, Yellow, Green |
                     | just Cyan          |
                                          | Blue, Magenta      |
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

- A mutable `TreeMap` ordered by the start of each interval. This allows for fast in-order retrieval in methods like
  `getAll`, and is essential for deterministic results in methods like `foldLeft` that use `getAll`.

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
  split. Intervalildus uses the ordered hashes defined on discrete domain components of intervals to approximate all
  discrete domain intervals as boxes in double space, and then manages valid data associated with these boxes in the box
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

```scala
import intervalidus.immutable.DataIn1D
import intervalidus.DiscreteInterval1D.*
import LocalDate.of as date

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
