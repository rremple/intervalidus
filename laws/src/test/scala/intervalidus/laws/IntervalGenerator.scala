package intervalidus.laws

import intervalidus.*
import intervalidus.laws.DomainGenerator.*
import intervalidus.Interval.unbounded
import intervalidus.{DomainLike, Interval}
import org.scalacheck.Gen

// Generates intervals of any dimension
object IntervalGenerator:

  def gen[D <: NonEmptyTuple: DomainLike: GenDomainOps]: Gen[Interval[D]] = for
    start <- genStart[D]
    end <- genEnd[D](start)
  yield Interval(start, end)

  def genBounded[D <: NonEmptyTuple: DomainLike: GenDomainOps]: Gen[Interval[D]] = for
    start <- genBoundedStart[D]
    end <- genBoundedEnd[D](start)
  yield Interval(start, end)

  val minSizeDim = IndexedSeq(100, 10, 3, 2)
  val maxSizeDim = IndexedSeq(200, 30, 5, 3)

  def genNonIntersecting[D <: NonEmptyTuple: DomainLike: GenDomainOps]: Gen[Iterable[Interval[D]]] =
    val dimIndex = arity[D] - 1
    val minSize: Int = minSizeDim.applyOrElse(dimIndex, _ => 2)
    val maxSize: Int = maxSizeDim.applyOrElse(dimIndex, _ => 3)
    for
      size <- Gen.choose(minSize, maxSize)
      raw <- Gen.listOfN(size, gen[D])
      shards = Interval.uniqueIntervals(unbounded +: raw)
      subset <- Gen.someOf(shards)
      limitSize <- Gen.choose(100, 200)
    yield subset.take(limitSize)

  val compositionTable: Map[(SpatialRelation, SpatialRelation), Set[SpatialRelation]] =
    import SpatialRelation.*
    val universalRelation: Set[SpatialRelation] = SpatialRelation.values.toSet
    val transitiveEq: List[((SpatialRelation, SpatialRelation), Set[SpatialRelation])] =
      universalRelation.toList.flatMap(i => Set((EQ, i) -> Set(i), (i, EQ) -> Set(i))).distinct
    Map.from(
      transitiveEq ++ List(
        (DC, DC) -> universalRelation,
        (DC, EC) -> Set(DC, EC, PO, TPP, NTPP),
        (DC, PO) -> Set(DC, EC, PO, TPP, NTPP),
        (DC, TPP) -> Set(DC, EC, PO, TPP, NTPP),
        (DC, NTPP) -> Set(DC, EC, PO, TPP, NTPP),
        (DC, TPPi) -> Set(DC),
        (DC, NTPPi) -> Set(DC),
        (EC, DC) -> Set(DC, EC, PO, TPPi, NTPPi),
        (EC, EC) -> Set(DC, EC, PO, TPP, TPPi, EQ),
        (EC, PO) -> Set(DC, EC, PO, TPP, NTPP),
        (EC, TPP) -> Set(EC, PO, TPP, NTPP),
        (EC, NTPP) -> Set(PO, TPP, NTPP),
        (EC, TPPi) -> Set(DC, EC),
        (EC, NTPPi) -> Set(DC),
        (PO, DC) -> Set(DC, EC, PO, TPPi, NTPPi),
        (PO, EC) -> Set(DC, EC, PO, TPPi, NTPPi),
        (PO, PO) -> universalRelation,
        (PO, TPP) -> Set(PO, TPP, NTPP),
        (PO, NTPP) -> Set(PO, TPP, NTPP),
        (PO, TPPi) -> Set(DC, EC, PO, TPPi, NTPPi),
        (PO, NTPPi) -> Set(DC, EC, PO, TPPi, NTPPi),
        (TPP, DC) -> Set(DC),
        (TPP, EC) -> Set(DC, EC),
        (TPP, PO) -> Set(DC, EC, PO, TPP, NTPP),
        (TPP, TPP) -> Set(TPP, NTPP),
        (TPP, NTPP) -> Set(NTPP),
        (TPP, TPPi) -> Set(DC, EC, PO, TPP, TPPi, EQ),
        (TPP, NTPPi) -> Set(DC, EC, PO, TPPi, NTPPi),
        (NTPP, DC) -> Set(DC),
        (NTPP, EC) -> Set(DC),
        (NTPP, PO) -> Set(DC, EC, PO, TPP, NTPP),
        (NTPP, TPP) -> Set(NTPP),
        (NTPP, NTPP) -> Set(NTPP),
        (NTPP, TPPi) -> Set(DC, EC, PO, TPP, NTPP),
        (NTPP, NTPPi) -> universalRelation,
        (TPPi, DC) -> Set(DC, EC, PO, TPPi, NTPPi),
        (TPPi, EC) -> Set(EC, PO, TPPi, NTPPi),
        (TPPi, PO) -> Set(PO, TPPi, NTPPi),
        (TPPi, TPP) -> Set(PO, TPP, TPPi, EQ),
        (TPPi, NTPP) -> Set(PO, TPP, NTPP),
        (TPPi, TPPi) -> Set(TPPi, NTPPi),
        (TPPi, NTPPi) -> Set(NTPPi),
        (NTPPi, DC) -> Set(DC, EC, PO, TPPi, NTPPi),
        (NTPPi, EC) -> Set(PO, TPPi, NTPPi),
        (NTPPi, PO) -> Set(PO, TPPi, NTPPi),
        (NTPPi, TPP) -> Set(PO, TPPi, NTPPi),
        (NTPPi, NTPP) -> Set(PO, TPP, TPPi, NTPP, EQ, NTPPi),
        (NTPPi, TPPi) -> Set(NTPPi),
        (NTPPi, NTPPi) -> Set(NTPPi)
      )
    )

  /*
   * Generate a randomized target relation.
   */
  def genRelation: Gen[SpatialRelation] = Gen.oneOf(SpatialRelation.values.toIndexedSeq)

  /*
   * In higher dimensions, almost everything comes back DC, so when we hit DC, we construct new intervals based
   * on the randomized target relation (not 100% of the time, but often enough that it makes a better test).
   */
  def closerToTarget[D <: NonEmptyTuple: DomainLike](
    x: Interval[D],
    y: Interval[D],
    target: SpatialRelation
  ): (Interval[D], Interval[D]) =
    (x relationWith y, x gapWith y) match
      case (
            SpatialRelation.DC,
            Some(gap) // DC always has a gap, which we can use to construct the target relation...
          ) =>
        target match // ...but the gap _sometimes_ shares a border (in some dimensions) with x
          case SpatialRelation.DC    => (x, y) // we actually want DC, so no change
          case SpatialRelation.EQ    => (x, x)
          case SpatialRelation.EC    => (x, y joinedWith gap) // both x and y are connected to gap
          case SpatialRelation.PO    => (x joinedWith gap, y joinedWith gap) // gap as intersection
          case SpatialRelation.TPP   => (x, x joinedWith gap)
          case SpatialRelation.TPPi  => (x joinedWith gap, x)
          case SpatialRelation.NTPP  => (gap, x joinedWith y) // could be NTPP or TPP depending on gap
          case SpatialRelation.NTPPi => (x joinedWith y, gap) // could be NTPPi or TPPi depending on gap
      case _ => (x, y) // not DC, so no change
