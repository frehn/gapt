/**
 * Terms extraction
 *
 * Returns a list with the terms used to instantiate each weakly quantified
 * formula.
 * Implemented for the cut-introduction algorithm.
 * NOTE: This algorithm was developed for prenex formulas only.
 * This means that when we hava a binary rule such as A ^ B, it is never the
 * case that A or B have instantiated terms. Therefore, we don't worry about it
 * and don't look them up on the hashmap (they won't be there anyway...)
 */

package at.logic.algorithms.cutIntroduction

import at.logic.calculi.lk.base._
import at.logic.calculi.lk.propositionalRules._
import at.logic.calculi.lk.quantificationRules._
import at.logic.calculi.lk.definitionRules._
import at.logic.language.fol._
import at.logic.calculi.occurrences._
import scala.collection.mutable._
import at.logic.calculi.lk.base.types._
import at.logic.algorithms.lk._

class TermsExtractionException(msg: String) extends Exception(msg)

object TermsExtraction {

  // If all the quantified formulas have only one quantifier, each list of
  // the list will have only one element
  def apply(proof: LKProof) : Map[FOLFormula, List[List[FOLTerm]]] = {
    val es = proof.root
    val formulas = es.antecedent ++ es.succedent
    // Check if all formulas are prenex.
    if( formulas.forall(f => f.formula.isPrenex) ) {
      val map = extractTerms(proof)
      map
    }
    else throw new TermsExtractionException("ERROR: Trying to extract the terms of a proof with non-prenex formulas: " + es.toString)
  }
  
  private def extractTerms(proof: LKProof) : Map[FOLFormula, List[List[FOLTerm]]] = proof match {

    /* AXIOM */
    case Axiom(s) => new HashMap[FOLFormula, List[List[FOLTerm]]] 

    /* WEAKENING RULES */
    case WeakeningLeftRule(up, _, pf) =>
      extractTerms(up)
    case WeakeningRightRule(up, _, pf) =>
      extractTerms(up)

    /* CONTRACTION RULES */
    case ContractionLeftRule(up, _, aux1, aux2, prin) =>
      extractTerms(up)

    case ContractionRightRule(up, _, aux1, aux2, prin) =>
      extractTerms(up)

    /* RIGHT CONJUNCTION RULE */
    case AndRightRule(up1, up2, _, aux1, aux2, _) =>
      val map1 = extractTerms(up1)
      val map2 = extractTerms(up2)
      map1 ++ map2

    /* LEFT CONJUNCTION RULES */
    case AndLeft1Rule(up, _, aux, prin) =>
      extractTerms(up)
    case AndLeft2Rule(up, _, aux, prin) =>
      extractTerms(up)

    /* LEFT DISJUNCTION RULE */
    case OrLeftRule(up1, up2, _, aux1, aux2, _) =>
      val map1 = extractTerms(up1)
      val map2 = extractTerms(up2)
      map1 ++ map2

    /* RIGHT DISJUNCTION RULES */
    case OrRight1Rule(up, _, aux, prin) =>
      extractTerms(up)
    case OrRight2Rule(up, _, aux, prin) =>
      extractTerms(up)

    /* LEFT IMPLICATION RULE */
    case ImpLeftRule(up1, up2, _, aux1, aux2, _) =>
      val map1 = extractTerms(up1)
      val map2 = extractTerms(up2)
      map1 ++ map2

    /* RIGHT IMPLICATION RULE */
    case ImpRightRule(up, _, aux1, aux2, _) =>
      extractTerms(up)

    /* NEGATION RULES */
    case NegLeftRule(up, _, aux, _) =>
      extractTerms(up)
    case NegRightRule(up, _, aux, _) =>
      extractTerms(up)

    /* WEAK QUANTIFIER RULES */
    // This is when the HashMap is filled.
    case ForallLeftRule(up, _, aux, prin, term) =>
      val map = extractTerms(up)
      val f = prin.formula.asInstanceOf[FOLFormula]
      val a = aux.formula.asInstanceOf[FOLFormula]
      val newterms = if (map.contains(a) ) {
        map -= a // side effect!!
        val terms = map(a)
        // Append the new terms to every list in terms
        terms.map(lst => term.asInstanceOf[FOLTerm] :: lst)
      }
      else {
        val folterm = term.asInstanceOf[FOLTerm]
        ((folterm::Nil)::Nil)
      }

      if(map.contains(f) ) {
        val t = map(f)
        map += (f -> (t ++ newterms) )
      }
      else map += (f -> newterms)

    case ExistsRightRule(up, _, aux, prin, term) =>
      val map = extractTerms(up)
      val f = prin.formula.asInstanceOf[FOLFormula]
      val a = aux.formula.asInstanceOf[FOLFormula]
      val newterms = if (map.contains(a) ) {
        map -= a // side effect!!
        val terms = map(a)
        // Append the new terms to every list in terms
        terms.map(lst => term.asInstanceOf[FOLTerm] :: lst)
      }
      else {
        val folterm = term.asInstanceOf[FOLTerm]
        ((folterm::Nil)::Nil)
      }

      if(map.contains(f) ) {
        val t = map(f)
        map += (f -> (t ++ newterms) )
      }
      else map += (f -> newterms)

    /* CUT RULE */
    case CutRule(up1, up2, _, a1, a2) => 
      val map1 = extractTerms(up1)
      val map2 = extractTerms(up2)
      map1 ++ map2


    /* STRONG QUANTIFIER RULES */
    // Should not occur
    case ExistsLeftRule(_, _, _, _, _) | ForallRightRule(_, _, _, _, _) =>
      throw new TermsExtractionException("ERROR: Found strong quantifier while extracting terms.")

    /* Any other rule... fail */
    case _ => throw new TermsExtractionException("ERROR: Unexpected rule while extracting the term set.")
  }
}

