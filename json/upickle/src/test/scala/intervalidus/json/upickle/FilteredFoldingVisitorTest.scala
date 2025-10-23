package intervalidus.json.upickle

import ujson.StringParser
import upickle.core.NoOpVisitor
import upickle.default.{ReadWriter, Reader, Writer, macroRW}
import intervalidus.json.{FilterPath, FilteredFoldingVisitorTestBehavior, Hours, OfficeHours, OfficeOpen, ProviderName}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

given ReadWriter[Hours] = macroRW[Hours]
given ReadWriter[OfficeOpen] = macroRW[OfficeOpen]
given ReadWriter[OfficeHours] = macroRW[OfficeHours]
given ReadWriter[ProviderName] = macroRW[ProviderName]

class FilteredFoldingVisitorTest
  extends AnyFunSuite
  with Matchers
  with FilteredFoldingVisitorTestBehavior[Writer, Reader]:

  override def filteredFoldingVisitor[A: Reader, B](filterPath: FilterPath, zero: B)(op: (B, A) => B): Reader[B] =
    FilteredFoldingVisitor(filterPath, zero)(op)

  override def transform[T, S](t: T)(using writeT: Writer[T], readS: Reader[S]): S = writeT.transform(t, readS)

  override def testDoc[T](using folder: Reader[T]): T = StringParser.transform(testDoc, folder)

  override def rUnit: Reader[Unit] = Reader.Delegate(NoOpVisitor)

  testsFor(commonBehaviors("uPickle"))
