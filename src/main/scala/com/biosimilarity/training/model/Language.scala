/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.biosimilarity.training.model

trait Language extends Expressions with Nominals
{
  type Nominal = Name
  type Term = Expression
}

