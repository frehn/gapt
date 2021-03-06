package at.logic.gapt.formats.tptp

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.execute.Success
import at.logic.gapt.language.hol._
import at.logic.gapt.language.lambda.types._
import at.logic.gapt.proofs.lk.base.FSequent

class TPTPHOLExporterTest extends SpecificationWithJUnit {
  "Export to TPTP thf" should {
    "handle atoms correctly" in {
      val x = HOLVar( "x", Ti -> To )
      val y = HOLVar( "y", To )
      val c = HOLConst( "c", Ti )

      val ax = HOLAtom( x, List( c ) )
      val ay = HOLAtom( y )

      println( TPTPHOLExporter( List( FSequent( Nil, List( ax, ay ) ) ) ) )

      println( TPTPHOLExporter( List( FSequent( List( ax ), Nil ),
        FSequent( Nil, List( ay ) ) ) ) )
      ok
    }
  }

}
