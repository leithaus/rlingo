/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.biosimilarity.training.model

import scala.collection.Map
import scala.collection.mutable.HashMap

trait SubstitutionCalculator extends Swaps {
  self: Language with FreeVariableCalculator with UUIDOps =>

    implicit def nominalToExpression( n : Nominal ) = Mention( n )
    implicit def exprToNominal( e : Expression ) = {
      e match {
	case Mention( n ) => n
	case _ => Transcription( e )
      }
    }

  def Fresh(nominals:Set[Nominal]) :Nominal =
  {
    StringVariable(getUUID().toString)
  }

  def Substitution(
    expr:Expression,
    swap:Map[Nominal,Expression]
  ):Expression= {
    expr match {
      case Mention(x) => {
        swap.getOrElse(x, expr)
      }
      case Abstraction(fmls, body) => {
        val keys = swap.keySet
        val nonCollisions = keys -- fmls.toSet
        if (nonCollisions == keys){
          Abstraction(fmls,Substitution(body, swap))
        }
        else
          {
            val collisions = keys & fmls.toSet
            val fV = FreeVariables(expr) ++ collisions
            val nSwap = new HashMap[Nominal,Expression]()
            for( n <- keys & fmls.toSet ) {
              nSwap( Fresh(fV) ) = n
            }
            val nBody = Substitution(body,nSwap)
            Abstraction(
	      fmls.map(
		{
		  (n)=> {
		    exprToNominal( nSwap.getOrElse( n, Mention( n ) ) )
		  }
		}
	      ),
	      Substitution( nBody, swap )
	    )
          }
        }
      case Application(head, exprs) => {
          Application(Substitution(head,swap), exprs.map((expr)=>Substitution(expr,swap)))
        }
      case _ => expr
    }
  }


}
