/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.biosimilarity.training.model
import java.util.UUID

// -*- mode: Scala;-*-
// Filename:    UUIDOps.scala
// Authors:     lgm
// Creation:    Tue Aug 10 00:45:45 2010
// Copyright:   Not supplied
// Description:
// ------------------------------------------------------------------------

trait UUIDOps {
  def getUUID(): UUID = UUID.randomUUID()
  def getUUID(uuid: String) = UUID.fromString(uuid)
}

