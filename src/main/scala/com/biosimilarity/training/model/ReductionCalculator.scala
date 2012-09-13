/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.biosimilarity.training.model

import scala.collection.Map
import scala.collection.mutable.HashMap

trait ReductionCalculator extends Swaps {
  self: Language with FreeVariableCalculator with SubstitutionCalculator with UUIDOps =>

//  def Fresh(nominals:Set[Nominal]) :Nominal =
//  {
//    StringVariable(getUUID().toString)
//  }

  def SwapMap(formals:List[Nominal], actuals:List[Expression]):Map[Nominal,Expression] = {
    val results = new HashMap[Nominal,Expression]
    for (i <- 0 to formals.length-1)
    {
      results(formals(i)) = actuals(i)
    }
    results
  }

  def ReduceToNormal( expr : Expression ) : Expression = {
    Reduce( expr ) match {
      case v : Value[_] => v
      case abs : Abstraction => abs
      case rn => ReduceToNormal( rn )
    }
  }

  def Reduce( expr : Expression ) : Expression = {
    expr match {
      case Application( op, actls ) => {
          val normalOp = ReduceToNormal( op )
          normalOp match {
            case abs : Abstraction => {
              Reduction( abs, actls )
            }
          }
      }
      case _ => {
         expr
      }
    }
  }

  def Reduction(abstraction: Abstraction, expr: List[Expression]):Expression= {
    val xLength = abstraction.formals.length
    val nLength = expr.length
    if (xLength == nLength){
      Substitution(abstraction.body,SwapMap(abstraction.formals,expr))
    }
    else if (xLength > nLength)
    {
      val swapped =
	Substitution(abstraction.body,SwapMap(abstraction.formals.take(nLength),expr))
      Abstraction(abstraction.formals.drop(nLength),swapped)
    }
    else
    {
      throw new Exception("arity mismatch")
    }
  }
}
