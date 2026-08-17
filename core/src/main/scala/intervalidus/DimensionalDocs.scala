package intervalidus

/**
  * Documentation for all dimensional data, whether extended through inheritance or composition.
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam D
  *   the domain type -- a non-empty tuple that is DomainLike.
  *
  * @define configParam
  *   context parameter for configuration -- uses defaults if not given explicitly
  * @define dataValueType
  *   the type of the value managed as data.
  * @define intervalDomainType
  *   the domain type -- a non-empty tuple that is DomainLike.
  * @define immutableReturn
  *   a new, updated structure.
  * @define mutableAction
  *   Data are mutated in place.
  * @define returnedDataValueType
  *   the valid data value type of the returned structure.
  * @define returnedIntervalDomainType
  *   the valid data interval domain type of the returned structure.
  * @define intersectionDesc
  *   The intersection of this and a single interval. See [[https://en.wikipedia.org/wiki/Intersection_(set_theory)]].
  * @define intersectionParamInterval
  *   a single interval with which to intersect.
  * @define symmetricDifferenceDesc
  *   The "exclusive or" of this and that. That is, the portions of the that which are not in the domain of this and the
  *   portions of this which are not in the domain of that. See [[https://en.wikipedia.org/wiki/Symmetric_difference]].
  * @define symmetricDifferenceParamThat
  *   structure to combine.
  * @define mapDesc
  *   Applies a function to all valid data.
  * @define mapParamF
  *   the function to apply to each valid data element.
  * @define collectDesc
  *   Applies a partial function to all valid data on which it is defined.
  * @define collectParamPf
  *   the partial function to apply to each data element.
  * @define mapValuesDesc
  *   Applies a function to all valid data values.
  * @define mapValuesParamF
  *   the function to apply to the value part of each valid data element.
  * @define collectValuesDesc
  *   Applies a partial function to all valid data values on which it is defined.
  * @define collectValuesParamPf
  *   the partial function to apply to the value part of each valid data element.
  * @define mapIntervalsDesc
  *   Applies a function to all valid data intervals.
  * @define mapIntervalsParamF
  *   the function to apply to the interval part of each valid data element.
  * @define collectIntervalsDesc
  *   Applies a partial function to all valid data intervals on which it is defined.
  * @define collectIntervalsParamPf
  *   the partial function to apply to the interval part of each valid data element.
  * @define flatMapParamF
  *   the function to apply to each valid data element which results in a new structure.
  * @define filterParamP
  *   the predicate used to test elements.
  * @define setDesc
  *   Set new valid data. Replaces any data previously valid in this interval.
  * @define setParamData
  *   the valid data to set.
  * @define setManyDesc
  *   Set a collection of new valid data. Replaces any data previously valid in this interval.
  * @define setManyNote
  *   if intervals overlap, later items will update earlier ones, so order can matter.
  * @define setManyParamData
  *   collection of valid data to set.
  * @define setIfNoConflictDesc
  *   Set new valid data, but only if there are no data previously valid in this interval.
  * @define setIfNoConflictParamData
  *   the valid data to set.
  * @define updateDesc
  *   Update everything valid in the data's interval to have the data's value. No new intervals of validity are added as
  *   part of this operation. Data with overlaps are adjusted accordingly.
  * @define updateParamData
  *   the new value and interval existing data should take on.
  * @define removeDesc
  *   Remove valid values on the interval. If there are values valid on portions of the interval, those values have
  *   their intervals adjusted (e.g., shortened, shifted, split) accordingly.
  * @define removeParamInterval
  *   the interval where any valid values are removed.
  * @define removeByKeyDesc
  *   Remove the valid value with an interval starting at the key.
  * @define removeByKeyParamKey
  *   key of the data to be removed (the interval start).
  * @define removeManyDesc
  *   Remove data in all the intervals. If there are values valid on portions of any interval, those values have their
  *   intervals adjusted (e.g., shortened, shifted, split) accordingly.
  * @define removeManyParamIntervals
  *   the intervals where any valid values are removed.
  * @define differenceDesc
  *   The elements in this which are not in the domain of that. The values of that are ignored. See
  *   [[https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement]].
  * @define differenceParamThat
  *   structure defining shape to remove.
  * @define removeValueDesc
  *   Remove the value in all the intervals where it is valid.
  * @define removeValueParamValue
  *   the value that is removed.
  * @define compressDesc
  *   Compress out adjacent intervals with the same value.
  * @define compressParamValue
  *   value for which valid data are compressed.
  * @define compressAllDesc
  *   Compress out adjacent intervals with the same value for all values.
  * @define recompressAllDesc1
  *   Unlike in 1D, there is no unique compression in higher dimensions. For example, {[1..5], [1..2]} + {[1..2],
  *   [3..4]} could also be represented physically as {[1..2], [1..4]} + {[3..5], [1..2]}.
  * @define recompressAllDesc2
  *   First, this method decompresses data to use a unique arrangement of "atomic" intervals. In the above example, that
  *   would be the following "atomic" intervals: {[1..2], [1..2]} + {[3..5], [1..2]} + {[1..2], [3..4]}. Next, it
  *   recompresses the data, which results in a unique physical representation. It may be useful when comparing two
  *   structures to see if they are logically equivalent even if, physically, they differ in how they are compressed.
  * @define recompressAllParamOtherIntervals
  *   other intervals to be considered when decompressing the space. This is useful in testing equivalence of two
  *   structures where their starting intervals differ enough that they result in a different enough decompression that
  *   it results in different recompressions.
  * @define applyDiffActionsDesc
  *   Applies a sequence of diff actions to this structure.
  * @define applyDiffActionsParamDiffActions
  *   actions to be applied.
  * @define syncWithDesc
  *   Synchronizes this with another structure by getting and applying the applicable diff actions.
  * @define syncWithParamThat
  *   the structure with which this is synchronized.
  * @define fillDesc
  *   Adds a value as valid in portions of the interval where there aren't already valid values.
  * @define fillParamData
  *   value to make valid in any validity gaps found in the interval
  * @define mergeDesc
  *   Merges this structure with data from that structure. In intervals where both structures have valid values, the two
  *   values are merged (e.g., keep this data). In intervals where this does not have valid data but that does, the data
  *   are added (a fill operation).
  * @define mergeParamThat
  *   structure to merge with this one
  * @define mergeParamMergeValues
  *   function that merges values where both this and that have valid values, where the default merge operation is to
  *   give this data values priority and drop that data values
  * @define mergeManyDesc
  *   Merges this structure with a collection of other data. For each, in intervals where valid values already exists,
  *   the two values are merged (e.g., keep this data). In intervals where this does not have valid data, the data are
  *   added (a fill operation).
  * @define mergeManyParamThatData
  *   collection of other data to merge with this
  * @define mergeManyParamMergeValues
  *   function that merges values where both this and that data have valid values, where the default merge operation is
  *   to give this data values priority and drop that data values
  * @define replaceDesc
  *   Remove the old data and replace it with the new data. The new data value and interval can be different. Data that
  *   overlaps with the new data interval are adjusted accordingly.
  * @define replaceParamOldData
  *   the old data to be replaced.
  * @define replaceParamNewData
  *   the new data replacing the old data
  * @define replaceByKeyDesc
  *   Remove the old data and replace it with the new data. The new data value and interval can be different. Data that
  *   overlaps with the new data interval are adjusted accordingly.
  * @define replaceByKeyParamKey
  *   key of the old data to be replaced (the interval start).
  * @define replaceByKeyParamNewData
  *   the new data replacing the old data
  */
private[intervalidus] trait DimensionalDocs:
  /** Marker method to ensure trait interface bytecode generation for Scaladoc. */
  protected def dimensionalDocsMarker(): Unit = ()
