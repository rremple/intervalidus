package intervalidus

import intervalidus.Domain.{HasDisplacementType, HasScalarType}

trait DomainAffineLikeTupleOps[D <: NonEmptyTuple]:

  // From domains

  def reflectedAboutFromDomain(using
    DomainAffineLike[D]
    // nothing to prove
  )(domain: D, pivot: D): D

  def scaledAboutFromDomain[S <: NonEmptyTuple](using
    DomainAffineLike[D],
    D HasScalarType S
  )(domain: D, center: D, scaledBy: S): D

  def displacedByFromDomain[S <: NonEmptyTuple](using
    DomainAffineLike[D],
    D HasDisplacementType S
  )(domain: D, offset: S): D

  def displacementToFromDomain[S <: NonEmptyTuple](using
    DomainAffineLike[D],
    D HasDisplacementType S
  )(domain: D, toDomain: D): Option[S]

  // From intervals

  def reflectedAboutFromInterval(using
    DomainAffineLike[D]
    // nothing to prove
  )(interval: Interval[D], pivot: D): Option[Interval[D]]

  def scaledAboutFromInterval[S <: NonEmptyTuple](using
    DomainAffineLike[D],
    D HasScalarType S
  )(interval: Interval[D], center: D, scaledBy: S): Option[Interval[D]]

  def displacedByFromInterval[S <: NonEmptyTuple](using
    DomainAffineLike[D],
    D HasDisplacementType S
  )(interval: Interval[D], offset: S): Option[Interval[D]]

  def paddedByFromInterval[S <: NonEmptyTuple](using
    DomainAffineLike[D],
    D HasDisplacementType S
  )(interval: Interval[D], offset: S): Option[Interval[D]]

  def dilatedByFromInterval(using
    DomainAffineLike[D]
  )(
    interval: Interval[D],
    element: Interval[D],
    elementCenter: D
  ): Option[Interval[D]]

  def erodedByFromInterval(using
    DomainAffineLike[D]
  )(
    interval: Interval[D],
    element: Interval[D],
    elementCenter: D
  ): Option[Interval[D]]

  def measureFromInterval[S <: NonEmptyTuple](using
    DomainAffineLike[D],
    D HasDisplacementType S
  )(interval: Interval[D]): Option[S]

/**
  * Use recursive decomposition of tuples to provide domain-like capabilities to tuples.
  *
  * @note
  *   We are using explicit `{ type Displacement = DispV; type Scalar = ScalarV }` structural refinements to extract
  *   path-dependent types into stable type parameters. This allows the compiler to safely unify types in inline methods
  *   that don't have access to the type paths. `DispV` and `ScalarV` become stable type alias that are independent of
  *   any variable paths.
  */
object DomainAffineLikeTupleOps:
  private type OneDimDomain[DV] = Domain1D[DV] *: EmptyTuple
  private type MultiDimDomain[DV, DomainTail <: NonEmptyTuple] = Domain1D[DV] *: DomainTail

  private type OneDimDisplacement[DispV] = DispV *: EmptyTuple
  private type MultiDimDisplacement[DispV, DispTail <: NonEmptyTuple] = DispV *: DispTail

  private type OneDimScalar[ScalarV] = ScalarV *: EmptyTuple
  private type MultiDimScalar[ScalarV, ScalarTail <: NonEmptyTuple] = ScalarV *: ScalarTail

  import DomainAffineLike.* // extension methods

  /**
    * Base case, for a one-dimensional domain (empty tail)
    */
  given OneDimOps[DV, DispV, ScalarV](using
    DomainAffineValueLike[DV] { type Displacement = DispV; type Scalar = ScalarV }
  ): DomainAffineLikeTupleOps[OneDimDomain[DV]] with

    private inline def headInterval(interval: Interval[OneDimDomain[DV]]): Interval1D[DV] =
      Interval1D(interval.start.head, interval.end.head)

    // From domains

    inline override def reflectedAboutFromDomain(using
      DomainAffineLike[OneDimDomain[DV]]
    )(domain: OneDimDomain[DV], pivot: OneDimDomain[DV]): OneDimDomain[DV] =
      domain.head.reflectedAbout(pivot.head).tupled

    inline override def scaledAboutFromDomain[S <: NonEmptyTuple](using
      DomainAffineLike[OneDimDomain[DV]],
      OneDimDomain[DV] HasScalarType S
    )(domain: OneDimDomain[DV], center: OneDimDomain[DV], scaledBy: S): OneDimDomain[DV] =
      val provenScaledBy = scaledBy.asInstanceOf[OneDimScalar[ScalarV]]
      domain.head.scaledAbout(center.head, provenScaledBy.head).tupled

    inline override def displacedByFromDomain[S <: NonEmptyTuple](using
      DomainAffineLike[OneDimDomain[DV]],
      OneDimDomain[DV] HasDisplacementType S
    )(domain: OneDimDomain[DV], offset: S): OneDimDomain[DV] =
      val provenOffset = offset.asInstanceOf[OneDimDisplacement[DispV]]
      domain.head.displacedBy(provenOffset.head).tupled

    inline override def displacementToFromDomain[S <: NonEmptyTuple](using
      DomainAffineLike[OneDimDomain[DV]],
      OneDimDomain[DV] HasDisplacementType S
    )(domain: OneDimDomain[DV], toDomain: OneDimDomain[DV]): Option[S] =
      def provenDisplacement(offset: DispV): S = (offset *: EmptyTuple).asInstanceOf[S]
      domain.head.displacementTo(toDomain.head).map(provenDisplacement)

    // From intervals

    inline override def reflectedAboutFromInterval(using
      DomainAffineLike[OneDimDomain[DV]]
    )(
      interval: Interval[OneDimDomain[DV]],
      pivot: OneDimDomain[DV]
    ): Option[Interval[OneDimDomain[DV]]] =
      headInterval(interval).reflectedAbout(pivot.head).map(_.tupled)

    inline override def scaledAboutFromInterval[S <: NonEmptyTuple](using
      DomainAffineLike[OneDimDomain[DV]],
      OneDimDomain[DV] HasScalarType S
    )(
      interval: Interval[OneDimDomain[DV]],
      center: OneDimDomain[DV],
      scaledBy: S
    ): Option[Interval[OneDimDomain[DV]]] =
      val provenScaledBy = scaledBy.asInstanceOf[OneDimScalar[ScalarV]]
      headInterval(interval).scaledAbout(center.head, provenScaledBy.head).map(_.tupled)

    inline override def displacedByFromInterval[S <: NonEmptyTuple](using
      DomainAffineLike[OneDimDomain[DV]],
      OneDimDomain[DV] HasDisplacementType S
    )(
      interval: Interval[OneDimDomain[DV]],
      offset: S
    ): Option[Interval[OneDimDomain[DV]]] =
      val provenOffset = offset.asInstanceOf[OneDimDisplacement[DispV]]
      headInterval(interval).displacedBy(provenOffset.head).map(_.tupled)

    inline override def paddedByFromInterval[S <: NonEmptyTuple](using
      DomainAffineLike[OneDimDomain[DV]],
      OneDimDomain[DV] HasDisplacementType S
    )(
      interval: Interval[OneDimDomain[DV]],
      offset: S
    ): Option[Interval[OneDimDomain[DV]]] =
      val provenOffset = offset.asInstanceOf[OneDimDisplacement[DispV]]
      headInterval(interval).paddedBy(provenOffset.head).map(_.tupled)

    inline def dilatedByFromInterval(using
      DomainAffineLike[OneDimDomain[DV]]
    )(
      interval: Interval[OneDimDomain[DV]],
      element: Interval[OneDimDomain[DV]],
      elementCenter: OneDimDomain[DV]
    ): Option[Interval[OneDimDomain[DV]]] =
      (headInterval(interval) ⊕ headInterval(element).withCenter(elementCenter.head)).map(_.tupled)

    inline def erodedByFromInterval(using
      DomainAffineLike[OneDimDomain[DV]]
    )(
      interval: Interval[OneDimDomain[DV]],
      element: Interval[OneDimDomain[DV]],
      elementCenter: OneDimDomain[DV]
    ): Option[Interval[OneDimDomain[DV]]] =
      (headInterval(interval) ⊖ headInterval(element).withCenter(elementCenter.head)).map(_.tupled)

    inline override def measureFromInterval[S <: NonEmptyTuple](using
      DomainAffineLike[OneDimDomain[DV]],
      OneDimDomain[DV] HasDisplacementType S
    )(
      interval: Interval[OneDimDomain[DV]]
    ): Option[S] =
      def provenDisplacement(offset: DispV): S = (offset *: EmptyTuple).asInstanceOf[S]
      headInterval(interval).measure.map(provenDisplacement)

  /**
    * Inductive case for a domain with two or more dimensions (non-empty tail)
    */
  given MultiDimOps[
    DV,
    DispV,
    ScalarV,
    DomainTail <: NonEmptyTuple: DomainAffineLike,
    DispTail <: NonEmptyTuple,
    ScalarTail <: NonEmptyTuple
  ](using
    DomainAffineValueLike[DV] { type Displacement = DispV; type Scalar = ScalarV },
    DomainTail HasDisplacementType DispTail,
    DomainTail HasScalarType ScalarTail
  )(using
    applyToTail: DomainAffineLikeTupleOps[DomainTail]
  ): DomainAffineLikeTupleOps[Domain1D[DV] *: DomainTail] with

    private inline def headInterval(interval: Interval[MultiDimDomain[DV, DomainTail]]): Interval1D[DV] =
      Interval1D(interval.start.head, interval.end.head)

    private inline def tailInterval(interval: Interval[MultiDimDomain[DV, DomainTail]]): Interval[DomainTail] =
      Interval(interval.start.tail, interval.end.tail)

    // From domains

    inline override def reflectedAboutFromDomain(using
      DomainAffineLike[MultiDimDomain[DV, DomainTail]]
    )(domain: MultiDimDomain[DV, DomainTail], pivot: MultiDimDomain[DV, DomainTail]): MultiDimDomain[DV, DomainTail] =
      val head = domain.head.reflectedAbout(pivot.head)
      val tail = applyToTail.reflectedAboutFromDomain(domain.tail, pivot.tail)
      tail withHead head

    inline override def scaledAboutFromDomain[S <: NonEmptyTuple](using
      DomainAffineLike[MultiDimDomain[DV, DomainTail]],
      MultiDimDomain[DV, DomainTail] HasScalarType S
    )(
      domain: MultiDimDomain[DV, DomainTail],
      center: MultiDimDomain[DV, DomainTail],
      scaledBy: S
    ): MultiDimDomain[DV, DomainTail] =
      val provenScaledBy = scaledBy.asInstanceOf[MultiDimScalar[ScalarV, ScalarTail]]
      val head = domain.head.scaledAbout(center.head, provenScaledBy.head)
      val tail = applyToTail.scaledAboutFromDomain(domain.tail, center.tail, provenScaledBy.tail)
      tail withHead head

    inline override def displacedByFromDomain[S <: NonEmptyTuple](using
      DomainAffineLike[MultiDimDomain[DV, DomainTail]],
      MultiDimDomain[DV, DomainTail] HasDisplacementType S
    )(domain: MultiDimDomain[DV, DomainTail], offset: S): MultiDimDomain[DV, DomainTail] =
      val provenOffset = offset.asInstanceOf[MultiDimDisplacement[DispV, DispTail]]
      val head = domain.head.displacedBy(provenOffset.head)
      val tail = applyToTail.displacedByFromDomain(domain.tail, provenOffset.tail)
      tail withHead head

    inline override def displacementToFromDomain[S <: NonEmptyTuple](using
      DomainAffineLike[MultiDimDomain[DV, DomainTail]],
      MultiDimDomain[DV, DomainTail] HasDisplacementType S
    )(
      domain: MultiDimDomain[DV, DomainTail],
      toDomain: MultiDimDomain[DV, DomainTail]
    ): Option[S] =
      def provenDisplacement(offset: DispV *: DispTail): S = offset.asInstanceOf[S]
      for
        head <- domain.head.displacementTo(toDomain.head)
        tail <- applyToTail.displacementToFromDomain(domain.tail, toDomain.tail)
      yield provenDisplacement(head *: tail)

    // From intervals

    inline override def reflectedAboutFromInterval(using
      DomainAffineLike[MultiDimDomain[DV, DomainTail]]
    )(
      interval: Interval[MultiDimDomain[DV, DomainTail]],
      pivot: MultiDimDomain[DV, DomainTail]
    ): Option[Interval[MultiDimDomain[DV, DomainTail]]] =
      for
        head <- headInterval(interval).reflectedAbout(pivot.head)
        tail <- applyToTail.reflectedAboutFromInterval(tailInterval(interval), pivot.tail)
      yield tail withHead head

    inline override def scaledAboutFromInterval[S <: NonEmptyTuple](using
      DomainAffineLike[MultiDimDomain[DV, DomainTail]],
      MultiDimDomain[DV, DomainTail] HasScalarType S
    )(
      interval: Interval[MultiDimDomain[DV, DomainTail]],
      center: MultiDimDomain[DV, DomainTail],
      scaledBy: S
    ): Option[Interval[MultiDimDomain[DV, DomainTail]]] =
      val provenScaledBy = scaledBy.asInstanceOf[MultiDimScalar[ScalarV, ScalarTail]]
      for
        head <- headInterval(interval).scaledAbout(center.head, provenScaledBy.head)
        tail <- applyToTail.scaledAboutFromInterval(tailInterval(interval), center.tail, provenScaledBy.tail)
      yield tail withHead head

    inline override def displacedByFromInterval[S <: NonEmptyTuple](using
      DomainAffineLike[MultiDimDomain[DV, DomainTail]],
      MultiDimDomain[DV, DomainTail] HasDisplacementType S
    )(
      interval: Interval[MultiDimDomain[DV, DomainTail]],
      offset: S
    ): Option[Interval[MultiDimDomain[DV, DomainTail]]] =
      val provenOffset = offset.asInstanceOf[MultiDimDisplacement[DispV, DispTail]]
      for
        head <- headInterval(interval).displacedBy(provenOffset.head)
        tail <- applyToTail.displacedByFromInterval(tailInterval(interval), provenOffset.tail)
      yield tail withHead head

    inline override def paddedByFromInterval[S <: NonEmptyTuple](using
      DomainAffineLike[MultiDimDomain[DV, DomainTail]],
      MultiDimDomain[DV, DomainTail] HasDisplacementType S
    )(
      interval: Interval[MultiDimDomain[DV, DomainTail]],
      offset: S
    ): Option[Interval[MultiDimDomain[DV, DomainTail]]] =
      val provenOffset = offset.asInstanceOf[MultiDimDisplacement[DispV, DispTail]]
      for
        head <- headInterval(interval).paddedBy(provenOffset.head)
        tail <- applyToTail.paddedByFromInterval(tailInterval(interval), provenOffset.tail)
      yield tail withHead head

    inline def dilatedByFromInterval(using
      DomainAffineLike[MultiDimDomain[DV, DomainTail]]
    )(
      interval: Interval[MultiDimDomain[DV, DomainTail]],
      element: Interval[MultiDimDomain[DV, DomainTail]],
      elementCenter: MultiDimDomain[DV, DomainTail]
    ): Option[Interval[MultiDimDomain[DV, DomainTail]]] =
      for
        head <- headInterval(interval) ⊕ headInterval(element).withCenter(elementCenter.head)
        tail <- applyToTail.dilatedByFromInterval(tailInterval(interval), tailInterval(element), elementCenter.tail)
      yield tail withHead head

    inline def erodedByFromInterval(using
      DomainAffineLike[MultiDimDomain[DV, DomainTail]]
    )(
      interval: Interval[MultiDimDomain[DV, DomainTail]],
      element: Interval[MultiDimDomain[DV, DomainTail]],
      elementCenter: MultiDimDomain[DV, DomainTail]
    ): Option[Interval[MultiDimDomain[DV, DomainTail]]] =
      for
        head <- headInterval(interval) ⊖ headInterval(element).withCenter(elementCenter.head)
        tail <- applyToTail.erodedByFromInterval(tailInterval(interval), tailInterval(element), elementCenter.tail)
      yield tail withHead head

    inline override def measureFromInterval[S <: NonEmptyTuple](using
      DomainAffineLike[MultiDimDomain[DV, DomainTail]],
      MultiDimDomain[DV, DomainTail] HasDisplacementType S
    )(
      interval: Interval[MultiDimDomain[DV, DomainTail]]
    ): Option[S] =
      def provenDisplacement(offset: DispV *: DispTail): S = offset.asInstanceOf[S]
      for
        head <- headInterval(interval).measure
        tail <- applyToTail.measureFromInterval(tailInterval(interval))
      yield provenDisplacement(head *: tail)
