
package at.logic.gapt.proofs.shlk.algorithms

import at.logic.gapt.language.hol._
import at.logic.gapt.language.lambda.types._
import at.logic.gapt.proofs.lk.algorithms.solve
import at.logic.gapt.proofs.lk.base.FSequent
import org.junit.runner.RunWith
import org.specs2.execute.Success
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner

// Seems like this is testing auto-propositional for HOL... why is it here?
@RunWith( classOf[JUnitRunner] )
class BussGeneratorAutoPropTest extends SpecificationWithJUnit {
  "BussGeneratorAutoPropTest" should {
    "continue autopropositional" in {

      val a = HOLConst( "a", Ti )

      val Pc1 = HOLAtom( HOLConst( "Pc1", Ti -> To ), a :: Nil )
      val Pc2 = HOLAtom( HOLConst( "Pc2", Ti -> To ), a :: Nil )
      val Pd1 = HOLAtom( HOLConst( "Pd1", Ti -> To ), a :: Nil )
      val Pd2 = HOLAtom( HOLConst( "Pd2", Ti -> To ), a :: Nil )
      val Pc1_or_Pd1 = HOLOr( Pc1, Pd1 )
      val imp_Pc1_or_Pd1_Pc2 = HOLImp( Pc1_or_Pd1, Pc2 )
      val imp_Pc1_or_Pd1_Pd2 = HOLImp( Pc1_or_Pd1, Pd2 )
      val imp_Pc1_or_Pd1_Pc2__or__imp_Pc1_or_Pd1_Pd2 = HOLOr( imp_Pc1_or_Pd1_Pc2, imp_Pc1_or_Pd1_Pd2 )
      //      val fs = FSequent(imp_Pc1_or_Pd1_Pc2__or__imp_Pc1_or_Pd1_Pd2 :: Pc1_or_Pd1 :: Nil , Pc2::Pd2::Nil)
      val fs = BussTautology( 2 )
      val bussGen = solve.solvePropositional( fs )

      Success()
    }
  }
}

object BussTautology {
  def apply( n: Int ): FSequent = FSequent( Ant( n ), c( n ) :: d( n ) :: Nil )

  val a = HOLConst( "a", Ti )

  def c( i: Int ) = HOLAtom( HOLConst( "c_" + i, Ti -> To ), a :: Nil )
  def d( i: Int ) = HOLAtom( HOLConst( "d_" + i, Ti -> To ), a :: Nil )
  def F( i: Int ): HOLFormula = if ( i == 1 )
    HOLOr( c( 1 ), d( 1 ) )
  else
    HOLAnd( F( i - 1 ), HOLOr( c( i ), d( i ) ) )
  def A( i: Int ) = if ( i == 1 ) c( 1 )
  else HOLImp( F( i - 1 ), c( i ) )
  def B( i: Int ) = if ( i == 1 ) d( 1 )
  else HOLImp( F( i - 1 ), d( i ) )

  // the antecedens of the final sequent
  def Ant( i: Int ): List[HOLFormula] = if ( i == 0 ) Nil else HOLOr( A( i ), B( i ) ) :: Ant( i - 1 )
}
