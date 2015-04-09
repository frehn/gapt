
package at.logic.gapt.provers.basicProver

import at.logic.gapt.proofs.lk.algorithms.LKProver
import at.logic.gapt.provers.{ FailSafeProver, Prover }
import at.logic.gapt.language.hol.HOLFormula
import at.logic.gapt.proofs.lk.base.{ FSequent, LKProof }

class BasicProver extends Prover {

  // Uses our propositional prover to get LK proof and 
  // minisat for validity check

  def getLKProof( seq: FSequent ): Option[LKProof] =
    new LKProver( cleanStructuralRules = false ).getLKProof( seq )

  override def isValid( seq: FSequent ): Boolean =
    FailSafeProver.getProver().isValid( seq )

  override def isValid( f: HOLFormula ): Boolean = {
    FailSafeProver.getProver().isValid( f )
  }
}
