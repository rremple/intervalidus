package intervalidus.json.weepickle

import com.rallyhealth.weejson.v1.jackson.FromJson
import com.rallyhealth.weepickle.v1.core.NoOpVisitor
import com.rallyhealth.weepickle.v1.WeePickle.{From, FromTo, To, macroFromTo}
import intervalidus.json.{FilterPath, FilteredFoldingVisitorTestBehavior, Hours, OfficeHours, OfficeOpen, ProviderName}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

given FromTo[Hours] = macroFromTo[Hours]
given FromTo[OfficeOpen] = macroFromTo[OfficeOpen]
given FromTo[OfficeHours] = macroFromTo[OfficeHours]
given FromTo[ProviderName] = macroFromTo[ProviderName]

class FilteredFoldingVisitorTest extends AnyFunSuite with Matchers with FilteredFoldingVisitorTestBehavior[From, To]:

  override def filteredFoldingVisitor[A: To, B](filterPath: FilterPath, zero: B)(op: (B, A) => B): To[B] =
    FilteredFoldingVisitor(filterPath, zero)(op)

  override def transform[T, S](t: T)(using fromT: From[T], toS: To[S]): S = fromT.transform(t, toS)

  override def testDoc[T](using toT: To[T]): T = FromJson(testDoc).transform(toT)

  override def rUnit: To[Unit] = To.Delegate(NoOpVisitor)

  testsFor(commonBehaviors("WeePickle"))
