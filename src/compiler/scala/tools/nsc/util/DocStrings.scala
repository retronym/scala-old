/* NSC -- new Scala compiler
 * Copyright 2006-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id: ClassPath.scala 20028 2009-12-07 11:49:19Z cunei $

package scala.tools.nsc
package util

import Chars._
import scala.collection.mutable.{HashMap, ListBuffer, StringBuilder}

/** Utilitity methods for doc comment strings
 */
object DocStrings {

  /** Returns index of string `str` following `start` skipping longest
   *  sequence of whitespace characters characters (but no newlines)
   */
  def skipWhitespace(str: String, start: Int): Int =
    if (start < str.length && isWhitespace(str charAt start)) skipWhitespace(str, start + 1)
    else start

  /** Returns index of string `str` following `start` skipping 
   *  sequence of identifier characters.
   */
  def skipIdent(str: String, start: Int): Int =
    if (start < str.length && isIdentifierPart(str charAt start)) skipIdent(str, start + 1)
    else start

  /** Returns index of string `str` after `start` skipping longest
   *  sequence of space and tab characters, possibly also containing
   *  a single `*' character.
   *  @pre  start == str.length || str(start) == `\n'
   */
  def skipLineLead(str: String, start: Int): Int =
    if (start == str.length) start
    else {
      val idx = skipWhitespace(str, start + 1)
      if (idx < str.length && (str charAt idx) == '*') skipWhitespace(str, idx + 1)
      else idx
    }

  /** Skips to next occurrence of `\n' following index `start`.
   */
  def skipToEol(str: String, start: Int): Int =
    if (start < str.length && (str charAt start) != '\n') skipToEol(str, start + 1)
    else start

  /** Returns first index following `start` and starting a line (i.e. after skipLineLead)
   *  which satisfies predicate `p'.
   */
  def findNext(str: String, start: Int)(p: Int => Boolean): Int = {
    val idx = skipLineLead(str, skipToEol(str, start))
    if (idx < str.length && !p(idx)) findNext(str, idx)(p)
    else idx
  }

  /** Return first index following `start` and starting a line (i.e. after skipLineLead)
   *  which satisfies predicate `p'.
   */
  def findAll(str: String, start: Int)(p: Int => Boolean): List[Int] = {
    val idx = findNext(str, start)(p)
    if (idx == str.length) List()
    else idx :: findAll(str, idx)(p)
  }

  /** Produces a string index, which is a list of ``sections'', i.e
   *  pairs of start/end positions of all tagged sections in the string.
   *  Every section starts with a `@' and extends to the next `@', or
   *  to the end of the comment string, but excluding the final two
   *  charcters which terminate the comment.
   */
  def tagIndex(str: String, p: Int => Boolean = (idx => true)): List[(Int, Int)] =
    findAll(str, 0) (idx => str(idx) == '@' && p(idx)) match {
      case List() => List()
      case idxs => idxs zip (idxs.tail ::: List(str.length - 2))
    }

  /** Does interval `iv` start with given `tag`?
   */
  def startsWithTag(str: String, section: (Int, Int), tag: String): Boolean = 
    startsWithTag(str, section._1, tag)

  def startsWithTag(str: String, start: Int, tag: String): Boolean =
    str.startsWith(tag, start) && !isIdentifierPart(str charAt (start + tag.length))


  /** The first start tag of a list of tag intervals,
   *  or the end of the whole comment string - 2 if list is empty
   */
  def startTag(str: String, sections: List[(Int, Int)]) = sections match {
    case List() => str.length - 2
    case (start, _) :: _ => start
  }

  /** A map from parameter names to start/end indices describing all parameter
   *  sections in `str` tagged with `tag`, where `sections` is the index of `str`.
   */
  def paramDocs(str: String, tag: String, sections: List[(Int, Int)]): Map[String, (Int, Int)] =
    Map() ++ {
      for (section <- sections if startsWithTag(str, section, tag)) yield {
        val start = skipWhitespace(str, section._1 + tag.length)
        str.substring(start, skipIdent(str, start)) -> section
      }
    }

  /** Optionally start and end index of return section in `str`, or `None`
   *  if `str` does not have a @return.
   */
  def returnDoc(str: String, sections: List[(Int, Int)]): Option[(Int, Int)] = 
    sections find (startsWithTag(str, _, "@return"))
    
  /** Extracts variable name from a string, stripping any pair of surrounding braces */
  def variableName(str: String): String = 
    if (str.length >= 2 && (str charAt 0) == '{' && (str charAt (str.length - 1)) == '}')
      str.substring(1, str.length - 1)
    else
      str

  /** Returns index following variable, or start index if no variable was recognized
   */
  def skipVariable(str: String, start: Int): Int = {
    var idx = start
    if (idx < str.length && (str charAt idx) == '{') {
      do idx += 1
      while (idx < str.length && (str charAt idx) != '}')
      if (idx < str.length) idx + 1 else start
    } else {
      while (idx < str.length && isVarPart(str charAt idx))
        idx += 1
      idx
    }
  }
}
