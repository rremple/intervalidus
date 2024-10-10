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
for validity as well as representation.

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

Intervalidus provides composable data structures -- both one-dimensional and two-dimensional, both mutable and
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

- `get` / `getAll` / `getAt` / `getIntersecting`
- `domain`
- `foldLeft`
- `isEmpty`
- `intersects`
- `diffActionsFrom`

These methods return a new structure:

- `copy` / `toImmutable` / `toMutable`
- `zip` / `zipAll`
- `getByHorizontalIndex` / `getByVerticalIndex` (only available on 2D and 3D)
- `getByDepthIndex` (only available on 3D)
- `flip` (only available on 2D)
- `flipAboutHorizontal` / `flipAboutVertical` / `flipAboutDepth` /  (only available on 3D)

These mutation methods return a new structure when using immutable and `Unit` when using mutable:

- `remove`
- `replace` / `replaceByKey`
- `set` / `setIfNoConflict`
- `update`
- `filter`
- `map` / `mapValues`
- `flatMap`
- `applyDiffActions` / `syncWith`
- `compress` / `compressAll` / `recompressAll`

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
type and use it in the definition of one-dimensional or two-dimensional intervals. For example, these intervals use 
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

You can also extend through composition. For example `DataIn1DVersioned` mimics the `DataIn1D` API but uses an
underlying `DataIn2D` structure with an integer vertical dimension to create a versioned data structure. The "current"
version is tracked as internal state and methods accept version selection as a context parameter, with "current" as the
default version selection applied. Also, a notion of approval is supported by specifying a specific future version for 
anything unapproved. Similarly, there is a `DataIn2DVersioned` mimicking the `DataIn2D` API using an 
underlying `DataIn3D` structure.

## Software Structure

Below is the class diagram for the core bits of Intervalidus
(three-dimensional is not shown, but it is very similar to two-dimensional):
![core class diagram](/doc/intervalidus-core.svg)

As described above, `DataIn1DVersioned` leverages the core classes to provide specific functionality you might want when
versioning data (such as approval). Below is the class diagram for it:
![versioned class diagram](/doc/intervalidus-versioned.svg)

Lastly, the definitions and implementations of methods across mutable/immutable and one-/two-/three-dimensional 
variants have been made as generic as possible to avoid repetitive code/scaladoc (DRY). However, this can make it
harder to navigate to these methods. The following (rather unorthodox) diagram shows where to find each method in a
kind of Venn-like way, where overlaps indicate a definition (and documentation) is in the lower trait with the
implementation in the higher, inheriting trait/class
(three-dimensional is not shown, but it is very similar to two-dimensional).:
![trait stack diagram](/doc/intervalidus-trait-stack.svg)
