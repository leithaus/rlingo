// -*- mode: Scala;-*- 
// Filename:    REPL.scala 
// Authors:     lgm                                                    
// Creation:    Thu May  8 10:18:48 2008 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.training.model

//import Absyn._
//import Eval._
//import Compile._

import scala.collection.immutable.HashMap

import java.net.URI
import java.util.UUID
import java.io.StringReader

class REPL
extends CoreLanguageProcessing {
  // parsing
  class ConcreteRLambdaParser()
  extends RLambdaParser

  def parser() = new ConcreteRLambdaParser()
  def clientRequestParseTree (str : String) = {
    val prsr = parser()
    prsr.parseAll( prsr.termXform, new StringReader( str ) )
  }
  def read (str : String) = clientRequestParseTree(str)  

  // compilation      
  
  // evaluation  

  // printing
  def showClientRequestParseTree (str : String) =
    clientRequestParseTree(str).toString    
}
