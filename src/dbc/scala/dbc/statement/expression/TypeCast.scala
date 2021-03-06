/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.dbc
package statement
package expression;


case class TypeCast (
  expression: Expression,
  castType: DataType
) extends Expression {
  
  /** A SQL-99 compliant string representation of the relation sub-
    * statement. This only has a meaning inside another statement. */
  def sqlInnerString: String = {
    "CAST (" + expression.sqlInnerString + " AS " + castType.sqlString + ")";
  }
  
  /** The expression that will be casted. */
  //def expression: Expression;
  
  /** The type to which to cast. */
  //def castType: scala.dbc.datatype.DataType;
}
