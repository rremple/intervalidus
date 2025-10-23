package intervalidus.json

/**
  * Common documentation for platform-specific implementations...
  *
  * @define inherited
  *   Sometimes you only want to process a slice of a very large JSON. Or maybe you want to process a small slice of a
  *   giant array, but without mapping the whole thing into memory at once. This special visitor allows you to filter
  *   out subsections of the source data, and "fold" over them, returning some consolidated results. It is very memory
  *   efficient, and does not add much overhead to processing. Paths to subdocuments can be expressed flexibly,
  *   including optional slicing constraints on array indexes.
  */
trait FilteredFoldingVisitorLike

/**
  * Common documentation for platform-specific implementations...
  *
  * WeePickle and uPickle test only differ in the monad type names.
  *
  * @tparam R
  *   monad for reading to a type (Reader/To)
  */
trait FilteredFoldingVisitorObjectLike[R[_]] extends FilteredFoldingVisitorLike:
  /**
    * $inherited
    * @param filterPath
    *   Path to the filtered data that will be folded.
    * @param zero
    *   An initial value of the result type B.
    * @param op
    *   A binary operator to accumulate filtered data into the result.
    * @tparam A
    *   The filtered data type (data referenced by the `filterPath`).
    * @tparam B
    *   The result type.
    * @return
    *   Structure to fold over any input to construct a `B`
    */
  def apply[A: R, B](filterPath: FilterPath, zero: B)(op: (B, A) => B): R[B]
