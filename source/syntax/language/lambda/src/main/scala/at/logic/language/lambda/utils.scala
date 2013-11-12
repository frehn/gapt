/*
 * Simple functions that operate on lambda-expressions
 *
 */

package at.logic.language.lambda

import at.logic.language.lambda.symbols.getRenaming

object freeVariables {
  def apply(e: LambdaExpression) : List[Var] = getFreeVariables(e, List())
  
  private def getFreeVariables(e: LambdaExpression, bound: List[Var]) : List[Var] = e match {
    case v : Var =>
      if (!bound.contains(v)) List(v)
      else List()
    case Const(_,_) => List()
    case App(exp, arg) => getFreeVariables(exp, bound) ++ getFreeVariables(arg, bound)
    case Abs(v, exp) => getFreeVariables(exp, v :: bound)
  }
}

// get a new variable/constant (similar to the current and) different from all 
// variables/constants in the blackList, returns this variable if this variable 
// is not in the blackList
object rename {
  def apply(v: Var, blackList: List[Var]) : Var = new Var(getRenaming(v.sym, blackList.map(v => v.sym)), v.exptype)

  def apply(c: Const, blackList: List[Const]) : Const = new Const(getRenaming(c.sym, blackList.map(c => c.sym)), c.exptype)
}
