// -*- mode: Scala;-*- 
// Filename:    rlambda.scala 
// Authors:     lgm                                                    
// Creation:    Sun Feb 13 20:20:23 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.training.model

import com.biosimilarity.lift.model.zipper._

import scala.xml._
import scala.util.parsing.combinator._
import scala.collection.SeqProxy

import java.net.URI

trait CoreLanguageForms {
  trait URLConcreteSyntax
  extends JavaTokenParsers {
    def URL : Parser[Any] =
      "<"~URLScheme~":"~URLPath~">"
    def URLScheme : Parser[Any] = 
      ident
    def URLPath : Parser[Any] =
      "/"~URLLocation~URLRelativePath
    def URLLocation : Parser[Any] =
      "/"~URLRsrcLocation
    def URLRsrcLocation : Parser[Any] =
      NetLocation~opt( ":"~port )
    def NetLocation : Parser[Any] =
      repsep( DNSElement, "." )
    def DNSElement : Parser[Any] =
      ident
    def port : Parser[Any] =
      wholeNumber
    def URLRelativePath : Parser[Any] =
      "/"~repsep( ident, "/" )

  }
    // def URLJ : Parser[URI] =
//       "<"~URLScheme~":"~URLPath~">" ^^ {
// 	case "<"~scheme~":"~( netloc, relpath )~">" => {
// 	  netloc match {
// 	    case ( host, Some( p ) ) => {
// 	      new URI( scheme, 
// 	    }
// 	    case ( host, None ) => {
// 	    }
// 	  }
// 	}
//       }
//     def URLSchemeJ : Parser[String] = 
//       ident.toString
//     def URLPathJ : Parser[( String, List[String] )] =
//       "/"~URLLocation~URLRelativePath ^^ {
// 	case "/"~loc~path => {
// 	  ( loc, path )
// 	}
//       }
//     def URLLLocationJ : Parser[String] =
//       "/"~URLRsrcLocation ^^ {
// 	case "/"~( h, p ) => {
// 	  h + p.getOrElse( "" )
// 	}
//       }
//     def URLRsrcLocationJ : Parser[( String, Option[Int] )] =
//       NetLocation~opt( ":"~port ) ^^ {
	
//       }
//     def NetLocationJ : Parser[Any] =
//       repsep( DNSElement, "." )
//     def DNSElementJ : Parser[Any] =
//       ident
//     def portJ : Parser[Any] =
//       wholeNumber
//     def URLRelativePathJ : Parser[Any] =
//       "/"~repsep( ident, "/" )
//  }

  trait RLambdaConcreteSyntax
   extends URLConcreteSyntax {
    def term : Parser[Any] = 
      application | abstraction | mention | value
    def application : Parser[Any] =
      term1~rep1sep( term1, " " )
    def abstraction : Parser[Any] =
      "lambda"~repsep( mention, "," )~"."~term1      
    def mention : Parser[Any] =
      ident | quotation | URL
    def value : Parser[Any] =
      (
	"*"~mention
	| stringLiteral
	| floatingPointNumber
	| "true"
	| "false"
	| "niv"
      )
    def quotation : Parser[Any] =
      "@"~"<"~term~">"
     def term1 : Parser[Any] =
      abstraction | value | mention | "("~term~")"
  }
}

trait CoreLanguageProcessing
extends CoreLanguageForms {
  trait RLambdaParser
  extends RLambdaConcreteSyntax
  with Expressions
  with Nominals {
    type Nominal = Name
    type Term = Expression

    def termXform : Parser[Expression] = 
      applicationXform | abstractionXform | mentionXform | valueXform
    def applicationXform : Parser[Application] =
      term1Xform~rep1sep( term1Xform, " " ) ^^ {
	case hd~args => Application( hd, args )
      }
    def abstractionXform : Parser[Abstraction] =
      "lambda"~repsep( mentionXform, "," )~"."~term1Xform ^^ {
	case "lambda"~fmls~"."~body =>
	  Abstraction( fmls.map( _.reference ), body )
      }
    def mentionXform : Parser[Mention] =
      (
	ident ^^ ( x => Mention( StringVariable( x ) ) )
	| quotationXform ^^ ( x => Mention( x ) )
	//| URL ^^ ( x => Mention( URLVariable( x ) ) )
      )
    def valueXform : Parser[Expression] =
      (
	"*"~mentionXform ^^ { case "*"~Mention( v ) => Dereference( v ) }
	| stringLiteral ^^ ( x => StringLiteral( x ) )
 	| floatingPointNumber ^^ ( x => DoubleLiteral( x.toDouble ) )
 	| wholeNumber ^^ ( x => IntegerLiteral( x.toInt ) )
 	| "true" ^^ ( x => BooleanLiteral( true ) )
 	| "false" ^^ ( x => BooleanLiteral( false ) )
 	| "bot" ^^ ( x => BottomLiteral )
      )
    def quotationXform : Parser[Nominal] =
      "@"~"<"~termXform~">" ^^ {
	case "@"~"<"~term~">" => Transcription( term )
      }
    def term1Xform : Parser[Expression] =
      (
	abstractionXform
	| valueXform
	| mentionXform
	| "("~termXform~")" ^^ {
	  case "("~term~")" => term
	}
      )
  }
}

object CLF extends CoreLanguageProcessing {
  object RLP extends RLambdaParser
}
