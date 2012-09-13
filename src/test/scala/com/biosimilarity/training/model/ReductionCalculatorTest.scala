/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.biosimilarity.training.model

import org.junit._
import Assert._
import scala.collection.mutable.HashMap

class ReductionCalculatorTest {

  object calc extends ReductionCalculator with Language with FreeVariableCalculator with SubstitutionCalculator with UUIDOps {
    //type Swap = HashMap[Nominal,Nominal]
  }
  import calc._

  @Before
  def setUp: Unit = {
  }

  @After
  def tearDown: Unit = {
  }

  @Test
  def ApplicationIdentityToValue = {
    val identity = Abstraction(List(StringVariable("x")), Mention(StringVariable("x")))
    val app = Application(identity,List(IntegerLiteral(1)))
    val result = calc.Reduce(app)
    assertEquals(IntegerLiteral(1), result)
  }

//  @Test
//  def LambdaXSubstitutionWithCollisions = {
//    val swap:Swap = new HashMap()
//    swap(StringVariable("x")) = StringVariable("y")
//    val expr = Abstraction(List(StringVariable("x")), Mention(StringVariable("x")))
//    val result = calc.Substitution(expr, swap)
//    val expectedResult = Abstraction(List(StringVariable("x")), Mention(StringVariable("x")))
//    assertFalse(result.equals(expectedResult))
//  }
}
