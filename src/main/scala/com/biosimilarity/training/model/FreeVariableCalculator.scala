/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.biosimilarity.training.model

import com.biosimilarity.training.model._
import scala.collection.immutable.HashSet

trait FreeVariableCalculator {
self: Language =>

  def FreeVariables(x:Expression):Set[Nominal]= {
    x match {
      case Mention(n) => new HashSet[Nominal]++List(n)
      case Abstraction(fmls, body) => FreeVariables(body) -- fmls
      case Application(head, exprs) => {
          exprs.flatMap((x)=> FreeVariables(x)).toSet ++ FreeVariables(head)
        }
      case _ => new HashSet[Nominal]
    }
  }
}
