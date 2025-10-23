package intervalidus.json.weepickle

import com.rallyhealth.weepickle.v1.core.{ArrVisitor, NoOpVisitor, ObjVisitor, StringVisitor, Visitor}
import com.rallyhealth.weepickle.v1.WeePickle.To
import intervalidus.json.{FilterPath, FilteredFoldingVisitorLike, FilteredFoldingVisitorObjectLike, PathComponent}

/**
  * $inherited
  */
class FilteredFoldingVisitor[A, B] private (
  filterPath: FilterPath,
  delegate: Visitor[Any, A],
  zero: B,
  parentPath: FilterPath
)(op: (B, A) => B)
  extends NoOpVisitor[B](zero)
  with FilteredFoldingVisitorLike:

  override def visitObject(length: Int): ObjVisitor[Any, B] =
    new ObjVisitor[Any, B] with FilterPath:
      private var key: Option[String] = None
      private var objResult: B = zero

      // subVisitor is called after visitKeyValue, so key should always be defined at this point
      override def path: List[PathComponent] = key.map(PathComponent.toObject) ++: parentPath.path

      override def visitKey(): Visitor[_, _] = StringVisitor

      override def visitKeyValue(s: Any): Unit =
        key = Some(s.toString)

      override def visitValue(v: Any): Unit =
        key = None
        objResult = v.asInstanceOf[B]

      override def visitEnd(): B = objResult

      override def subVisitor: Visitor[Any, B] = comparePathTo(filterPath)(
        whenPrefix = new FilteredFoldingVisitor(filterPath, delegate, zero = objResult, parentPath = this)(op),
        whenMatches = delegate.map(op(objResult, _)),
        otherwise = NoOpVisitor(objResult)
      )

  override def visitArray(length: Int): ArrVisitor[Any, B] =
    new ArrVisitor[Any, B] with FilterPath:
      // must anticipate current element because subVisitor gets called before visitValue
      private var i: Int = 0
      private var arrResult: B = zero

      override def path: List[PathComponent] = PathComponent.toArrayIndex(i) :: parentPath.path

      override def visitValue(v: Any): Unit =
        i += 1
        arrResult = v.asInstanceOf[B]

      override def visitEnd(): B = arrResult

      override def subVisitor: Visitor[Any, B] = comparePathTo(filterPath)(
        whenPrefix = new FilteredFoldingVisitor(filterPath, delegate, zero = arrResult, parentPath = this)(op),
        whenMatches = delegate.map(op(arrResult, _)),
        otherwise = NoOpVisitor(arrResult)
      )

object FilteredFoldingVisitor extends FilteredFoldingVisitorObjectLike[To]:
  override def apply[A, B](filterPath: FilterPath, zero: B)(op: (B, A) => B)(using toA: To[A]): To[B] =
    To.Delegate(new FilteredFoldingVisitor(filterPath, toA, zero, FilterPath.RootPath)(op))
