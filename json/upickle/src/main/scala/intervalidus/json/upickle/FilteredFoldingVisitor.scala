package intervalidus.json.upickle

import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, StringVisitor, Visitor}
import upickle.default.Reader
import intervalidus.json.{FilterPath, FilteredFoldingVisitorObjectLike, PathComponent}

/**
  * Sometimes you only want to process a slice of a very large JSON. Or maybe you want to process a small slice of a
  * giant array, but without mapping the whole thing into memory at once. This special visitor allows you to filter out
  * subsections of the source data, and "fold" over them, returning some consolidated results. It is very memory
  * efficient, and does not add much overhead to processing. Paths to subdocuments can be expressed flexibly, including
  * optional slicing constraints on array indexes.
  */
class FilteredFoldingVisitor[A, B] private (
  filterPath: FilterPath,
  delegate: Visitor[Any, A],
  zero: B,
  parentPath: FilterPath
)(op: (B, A) => B)
  extends NoOpFixedReader[B](zero):

  override def visitObject(length: Int, jsonableKeys: Boolean, index: Int): ObjVisitor[Any, B] =
    new ObjVisitor[Any, B] with FilterPath:
      private var key: Option[String] = None
      private var objResult: B = zero

      // subVisitor is called after visitKeyValue, so key should always be defined at this point
      override def path: List[PathComponent] = key.map(PathComponent.toObject) ++: parentPath.path

      override def visitKey(index: Int): Visitor[_, _] = StringVisitor

      override def visitKeyValue(s: Any): Unit =
        key = Some(s.toString)

      override def visitValue(v: Any, index: Int): Unit =
        key = None
        objResult = v.asInstanceOf[B]

      override def visitEnd(index: Int): B = objResult

      override def subVisitor: Visitor[Any, B] = comparePathTo(filterPath)(
        whenPrefix = new FilteredFoldingVisitor(filterPath, delegate, zero = objResult, parentPath = this)(op),
        whenMatches = delegate.map(op(objResult, _)),
        otherwise = NoOpFixedReader(objResult)
      )

  override def visitArray(length: Int, index: Int): ArrVisitor[Any, B] =
    new ArrVisitor[Any, B] with FilterPath:
      // must anticipate current element because subVisitor gets called before visitValue
      private var i: Int = 0
      private var arrResult: B = zero

      override def path: List[PathComponent] = PathComponent.toArrayIndex(i) :: parentPath.path

      override def visitValue(v: Any, index: Int): Unit =
        i += 1
        arrResult = v.asInstanceOf[B]

      override def visitEnd(index: Int): B = arrResult

      override def subVisitor: Visitor[Any, B] = comparePathTo(filterPath)(
        whenPrefix = new FilteredFoldingVisitor(filterPath, delegate, zero = arrResult, parentPath = this)(op),
        whenMatches = delegate.map(op(arrResult, _)),
        otherwise = NoOpFixedReader(arrResult)
      )

object FilteredFoldingVisitor extends FilteredFoldingVisitorObjectLike[Reader]:
  override def apply[A, B](filterPath: FilterPath, zero: B)(op: (B, A) => B)(using readerA: Reader[A]): Reader[B] =
    Reader.Delegate(new FilteredFoldingVisitor(filterPath, readerA, zero, FilterPath.RootPath)(op))

/**
  * Similar to [[NoOpVisitor]], but returns a fixed result rather than [[Unit]].
  *
  * @param fixedResult
  *   result returned by all visit methods.
  * @tparam B
  *   fixed result type.
  */
private class NoOpFixedReader[B](fixedResult: B) extends Reader.Delegate(NoOpVisitor.map(_ => fixedResult))
