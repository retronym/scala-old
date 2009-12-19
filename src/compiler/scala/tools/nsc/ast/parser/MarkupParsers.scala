/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Burak Emir
 */
// $Id$

package scala.tools.nsc
package ast.parser

import scala.collection.mutable
import mutable.{ Buffer, ArrayBuffer, ListBuffer, HashMap }
import scala.util.control.ControlException
import scala.tools.nsc.util.{Position,NoPosition,SourceFile,CharArrayReader}
import scala.xml.{ Text, TextBuffer }
import scala.xml.Utility.{ isNameStart, isNameChar, isSpace }
import util.Chars.{ SU, LF }
import scala.annotation.switch

// XXX/Note: many/most of the functions in here are almost direct cut and pastes
// from another file - scala.xml.parsing.MarkupParser, it looks like.
// (It was like that when I got here.) They used to be commented "[Duplicate]" but
// since approximately all of them were, I snipped it as noise.  As far as I can
// tell this wasn't for any particularly good reason, but slightly different
// compiler and library parser interfaces meant it would take some setup.
//
// I rewrote most of these, but not as yet the library versions: so if you are
// tempted to touch any of these, please be aware of that situation and try not
// to let it get any worse.  -- paulp

/** This trait ...
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
trait MarkupParsers 
{
  self: Parsers =>
  
  case object MissingEndTagException extends RuntimeException with ControlException {
    override def getMessage = "start tag was here: "
  }

  case object ConfusedAboutBracesException extends RuntimeException with ControlException {
    override def getMessage = " I encountered a '}' where I didn't expect one, maybe this tag isn't closed <"
  }

  case object TruncatedXML extends RuntimeException with ControlException {
    override def getMessage = "input ended while parsing XML"
  }

  import global._

  class MarkupParser(parser: UnitParser, final val preserveWS: Boolean) extends scala.xml.parsing.MarkupParserCommon {

    import Tokens.{ EMPTY, LBRACE, RBRACE }
    
    type PositionType = Position
    val eof = false
    
    def xHandleError(that: Char, msg: String) =
      if (ch == SU) throw TruncatedXML
      else reportSyntaxError(msg)

    var input : CharArrayReader = _

    import parser.{ symbXMLBuilder => handle, o2p, r2p }

    def curOffset : Int = input.charOffset - 1
    var tmppos : Position = NoPosition
    def ch = input.ch
    /** this method assign the next character to ch and advances in input */
    def nextch = { val result = input.ch; input.nextChar(); result } 

    var xEmbeddedBlock = false
    
    /** Execute body with a variable saved and restored after execution */
    def saving[A,B](getter: A, setter: (A) => Unit)(body: => B): B = {
      val saved = getter
      try body
      finally setter(saved)
    }

    private var debugLastStartElement = new mutable.Stack[(Int, String)]
    private def debugLastPos = debugLastStartElement.top._1
    private def debugLastElem = debugLastStartElement.top._2
    private def unreachable = Predef.error("Cannot be reached.")
    
    private def errorBraces() = {
      reportSyntaxError("in XML content, please use '}}' to express '}'")
      throw ConfusedAboutBracesException
    }
    private def errorNoEnd(tag: String) = {
      reportSyntaxError("expected closing tag of " + tag)
      throw MissingEndTagException
    }
    private def errorAndResult[T](msg: String, x: T): T = {
      reportSyntaxError(msg)
      x
    }

    /** checks whether next character starts a Scala block, if yes, skip it.
     * @return true if next character starts a scala block 
     */
    def xCheckEmbeddedBlock: Boolean = {
      // attentions, side-effect, used in xText
      xEmbeddedBlock = (ch == '{') && { nextch; (ch != '{') }
      xEmbeddedBlock
    }

    /** parse attribute and add it to listmap
     *  [41] Attributes   ::= { S Name Eq AttValue }
     *       AttValue     ::= `'` { _  } `'`
     *                      | `"` { _ } `"`
     *                      | `{` scalablock `}`
     */
    def xAttributes = {
      val aMap = new HashMap[String, Tree]()
      
      while (isNameStart(ch)) {
        val start = curOffset
        val key = xName
        xEQ
        val delim = ch
        val mid = curOffset
        val value: Tree = ch match {
          case '"' | '\'' =>
            nextch
            val tmp = xAttributeValue(delim)
            nextch
            
            try handle.parseAttribute(r2p(start, mid, curOffset), tmp)
            catch {
              case e: RuntimeException => 
                errorAndResult("error parsing attribute value", parser.errorTermTree)
            }
          
          case '{'  =>
            nextch
            xEmbeddedExpr
          case SU =>
            throw TruncatedXML
          case _ =>
            errorAndResult("' or \" delimited attribute value or '{' scala-expr '}' expected", Literal(Constant("<syntax-error>")))
        }
        // well-formedness constraint: unique attribute names
        if (aMap contains key)
          reportSyntaxError("attribute %s may only be defined once" format key)

        aMap(key) = value
        if (ch != '/' && ch != '>')
          xSpace
      }
      aMap
    }

    /** attribute value, terminated by either ' or ". value may not contain <.
     *  @param endch either ' or "
     */
    def xAttributeValue(endCh: Char): String = {
      val buf = new StringBuilder      
      while (ch != endCh) {
        // well-formedness constraint
        if (ch == '<') return errorAndResult("'<' not allowed in attrib value", "")
        else if (ch == SU) throw TruncatedXML
        else buf append nextch
      }
      // @todo: normalize attribute value
      buf.toString
    }

    /** parse a start or empty tag.
     *  [40] STag         ::= '<' Name { S Attribute } [S] 
     *  [44] EmptyElemTag ::= '<' Name { S Attribute } [S] 
     */
    def xTag: (String, mutable.Map[String, Tree]) = {
      val elemName = xName
      xSpaceOpt
       
      (elemName, xAttributes)
    }

    /** [42]  '<' xmlEndTag ::=  '<' '/' Name S? '>'
     */
    def xEndTag(startName: String) {
      xToken('/')
      if (xName != startName)
        errorNoEnd(startName)

      xSpaceOpt
      xToken('>')
    }
    
    /** Create a non-destructive lookahead reader and see if the head
     *  of the input would match the given String.  If yes, return true
     *  and drop the entire String from input; if no, return false
     *  and leave input unchanged.
     */
    private def peek(lookingFor: String): Boolean = {
      val la = input.lookaheadReader
      for (c <- lookingFor) {
        la.nextChar()
        if (la.ch != c)
          return false
      }
      // drop the chars from the real reader (all lookahead + orig)
      (0 to lookingFor.length) foreach (_ => nextch)
      true
    }
    
    /** Take characters from input stream until given String "until"
     *  is seen.  Once seen, the accumulated characters are passed
     *  along with the current Position to the supplied handler function.
     */
    private def xTakeUntil[T](
      handler: (Position, String) => T,
      positioner: () => Position,
      until: String): T =
    {
      val sb = new StringBuilder
      val head = until charAt 0
      val rest = until drop 1
      
      while (true) {
        if (ch == head && peek(rest))
          return handler(positioner(), sb.toString)
        else if (ch == SU)
          throw TruncatedXML
        
        sb append ch
        nextch
      }
      unreachable
    }
      
    /** '<! CharData ::= [CDATA[ ( {char} - {char}"]]>"{char} ) ']]>'
     *
     * see [15]
     */
    def xCharData: Tree = {
      val start = curOffset
      "[CDATA[" foreach xToken
      val mid = curOffset
      xTakeUntil(handle.charData, () => r2p(start, mid, curOffset), "]]>")
    }

    def xUnparsed: Tree = {
      val start = curOffset
      xTakeUntil(handle.unparsed, () => r2p(start, start, curOffset), "</xml:unparsed>")
    }

    /** CharRef ::= "&#" '0'..'9' {'0'..'9'} ";"
     *            | "&#x" '0'..'9'|'A'..'F'|'a'..'f' { hexdigit } ";"
     *
     * see [66]
     */
    def xCharRef: String = {
      val hex = (ch == 'x') && { nextch; true }
      val base = if (hex) 16 else 10
      var i = 0
      while (ch != ';') { 
        (ch: @switch) match {
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
            i = i * base + ch.asDigit
          case 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
             | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' =>
            if (!hex)
              reportSyntaxError("hex char not allowed in decimal char ref\n"
                           +"Did you mean to write &#x ?");
            else
              i = i * base + ch.asDigit
          case SU =>
            throw TruncatedXML
          case _ =>
            reportSyntaxError("character '"+ch+"' not allowed in char ref")
        }
        nextch
      }
      i.toChar.toString
    }

    /** Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
     *
     * see [15]
     */
    def xComment: Tree = {
      val start = curOffset - 2   // Rewinding to include "<!"
      "--" foreach xToken      
      xTakeUntil(handle.comment, () => r2p(start, start, curOffset), "-->")
    }

    def appendText(pos: Position, ts: Buffer[Tree], txt: String) {
      val toAppend =
        if (preserveWS) Seq(txt)
        else TextBuffer.fromString(txt).toText map (_.text)
      
      toAppend foreach (t => ts append handle.text(pos, t))
    }

    /** adds entity/character to ts as side-effect 
     *  @precond ch == '&amp;'
     */
    def content_AMP(ts: ArrayBuffer[Tree]) {
      nextch
      val toAppend = ch match {
        case '#' => // CharacterRef
          nextch
          val theChar = handle.text(tmppos, xCharRef)
          xToken(';')
          theChar
        case _ =>   // EntityRef
          val n = xName
          xToken(';')
          handle.entityRef(tmppos, n)
      }
      
      ts append toAppend
    }

    /**
     *  @precond ch == '{'
     *  @postcond: xEmbeddedBlock == false!
     */
    def content_BRACE(p: Position, ts: ArrayBuffer[Tree]): Unit =
      if (xCheckEmbeddedBlock) ts append xEmbeddedExpr
      else appendText(p, ts, xText)

    /** Returns true if it encounters an end tag (without consuming it),
     *  appends trees to ts as side-effect.
     *
     *  @param ts ...
     *  @return   ...
     */
    private def content_LT(ts: ArrayBuffer[Tree]): Boolean = {
      if (ch == '/')
        return true   // end tag
      
      val toAppend = ch match {
        case '!'    => nextch ; if (ch =='[') xCharData else xComment // CDATA or Comment
        case '?'    => nextch ; xProcInstr                            // PI
        case _      => element                                        // child node
      }
      
      ts append toAppend
      false
    }

    def content: Buffer[Tree] = {
      val ts = new ArrayBuffer[Tree]
      while (true) {
        if (xEmbeddedBlock)
          ts append xEmbeddedExpr
        else {
          tmppos = o2p(curOffset)
          ch match {
            // end tag, cdata, comment, pi or child node
            case '<'  => nextch ; if (content_LT(ts)) return ts
            // either the character '{' or an embedded scala block }
            case '{'  => content_BRACE(tmppos, ts)  // }
            // EntityRef or CharRef 
            case '&'  => content_AMP(ts)
            case SU   => return ts
            // text content - here xEmbeddedBlock might be true
            case _    => appendText(tmppos, ts, xText)
          }
        }
      }
      unreachable
    }

    /** '<' element ::= xmlTag1 '>'  { xmlExpr | '{' simpleExpr '}' } ETag
     *                | xmlTag1 '/' '>'
     */
    def element: Tree = {
      val start = curOffset
      val (qname, attrMap) = xTag
      if (ch == '/') { // empty element
        "/>" foreach xToken
        handle.element(r2p(start, start, curOffset), qname, attrMap, new ListBuffer[Tree])
      }
      else { // handle content
        xToken('>')
        if (qname == "xml:unparsed")
          return xUnparsed

        debugLastStartElement.push((start, qname))
        val ts = content
        xEndTag(qname)
        debugLastStartElement.pop
        val pos = r2p(start, start, curOffset)
        qname match {
          case "xml:group" => handle.group(pos, ts)
          case _ => handle.element(pos, qname, attrMap, ts)
        }
      }
    }

    /** actually, Name ::= (Letter | '_' | ':') (NameChar)*  but starting with ':' cannot happen
     *  Name ::= (Letter | '_') (NameChar)*
     *
     *  see  [5] of XML 1.0 specification
     *
     *  pre-condition:  ch != ':' // assured by definition of XMLSTART token
     *  post-condition: name does neither start, nor end in ':'
     */
    def xName: String = {
      if (ch == SU) throw TruncatedXML
      else if (!isNameStart(ch))
        return errorAndResult("name expected, but char '%s' cannot start a name" format ch, "")
      
      val buf = new StringBuilder

      do buf append nextch
      while (isNameChar(ch))
      
      if (buf.last == ':') {
        reportSyntaxError( "name cannot end in ':'" )
        buf setLength (buf.length - 1)
      }
      buf.toString.intern
    }

    /** '<?' ProcInstr ::= Name [S ({Char} - ({Char}'>?' {Char})]'?>'
     *
     * see [15]
     */
    // <?xml2 version="1.0" encoding="UTF-8" standalone="yes"?>
    def xProcInstr: Tree = {
      val n = xName
      xSpaceOpt
      xTakeUntil(handle.procInstr(_: Position, n, _:String), () => tmppos, "?>")
    }

    /** parse character data.
     *  precondition: xEmbeddedBlock == false (we are not in a scala block)
     */
    def xText: String = {
      assert(!xEmbeddedBlock, "internal error: encountered embedded block")
      val buf = new StringBuilder
      def done = buf.toString
      
      while (ch != SU) {
        if (ch == '}') {
          if (charComingAfter(nextch) == '}') nextch
          else errorBraces
        }
        
        buf append ch
        nextch
        if (xCheckEmbeddedBlock || ch == '<' ||  ch == '&')
          return done
      }
      done
    }
    
    /** Some try/catch/finally logic used by xLiteral and xLiteralPattern.  */
    private def xLiteralCommon(f: () => Tree, ifTruncated: Exception => Unit): Tree =
      try f()
      catch {
        case ex: RuntimeException => 
          ex match {
            case c @ TruncatedXML =>
              ifTruncated(c)
            case c @ (MissingEndTagException | ConfusedAboutBracesException) =>
              parser.syntaxError(debugLastPos, c.getMessage + debugLastElem + ">")
            case _: ArrayIndexOutOfBoundsException =>
              parser.syntaxError(debugLastPos, "missing end tag in XML literal for <%s>" format debugLastElem)
            case _ => throw ex
          }
          EmptyTree
      }
      finally parser.in resume Tokens.XMLSTART
      
    /** Use a lookahead parser to run speculative body, and return the first char afterward. */
    private def charComingAfter(body: => Unit): Char = {
      input = input.lookaheadReader
      body
      val res = ch
      input = parser.in
      res
    }

    /** xLiteral = element { element }
     *  @return Scala representation of this xml literal
     */
    def xLiteral: Tree = xLiteralCommon(
      () => {
        input = parser.in
        handle.isPattern = false
      
        val ts = new ArrayBuffer[Tree]
        val start = curOffset
        tmppos = o2p(curOffset)    // Iuli: added this line, as it seems content_LT uses tmppos when creating trees
        content_LT(ts)
      
        // parse more XML ?        
        if (charComingAfter(xSpaceOpt) == '<') {
          xSpaceOpt
          while (ch == '<') {
            nextch
            ts append element
            xSpaceOpt
          }
          handle.makeXMLseq(r2p(start, start, curOffset), ts)
        }
        else {
          assert(ts.length == 1)
          ts(0)
        }
      },
      ex => parser.incompleteInputError(ex.getMessage)
    )

    /** @see xmlPattern. resynchronizes after successful parse 
     *  @return this xml pattern
     */
    def xLiteralPattern: Tree = xLiteralCommon(
      () => {
        input = parser.in
        saving[Boolean, Tree](handle.isPattern, handle.isPattern = _) {
          handle.isPattern = true
          val tree = xPattern
          xSpaceOpt
          tree
        }
      },
      ex => parser.syntaxError(curOffset, ex.getMessage)
    )

    def escapeToScala[A](op: => A, kind: String) = {
      xEmbeddedBlock = false
      val res = saving[List[Int], A](parser.in.sepRegions, parser.in.sepRegions = _) {
        parser.in resume LBRACE
        op
      }
      if (parser.in.token != RBRACE)
        reportSyntaxError(" expected end of Scala "+kind)
      
      res
    }

    def xEmbeddedExpr: Tree = escapeToScala(parser.block(), "block")

    /** xScalaPatterns  ::= patterns
     */
    def xScalaPatterns: List[Tree] = escapeToScala(parser.patterns(true), "pattern")

    def reportSyntaxError(pos: Int, str: String) = parser.syntaxError(pos, str)
    def reportSyntaxError(str: String) = {
      reportSyntaxError(curOffset, "in XML literal: " + str)
      nextch
    }

    /** '<' xPattern  ::= Name [S] { xmlPattern | '{' pattern3 '}' } ETag
     *                  | Name [S] '/' '>'
     */
    def xPattern: Tree = {      
      var start = curOffset
      val qname = xName
      debugLastStartElement.push((start, qname))
      xSpaceOpt

      val ts = new ArrayBuffer[Tree]
      val isEmptyTag = (ch == '/') && { nextch ; true }
      xToken('>')

      if (!isEmptyTag) {        
        // recurses until it hits a termination condition, then returns
        def doPattern: Boolean = {
          val start1 = curOffset
          if (xEmbeddedBlock) ts ++= xScalaPatterns
          else ch match {
            case '<'  => // tag
              nextch
              if (ch != '/') ts append xPattern   // child
              else return false                   // terminate
              
            case '{'  => // embedded Scala patterns
              while (ch == '{') {
                nextch
                ts ++= xScalaPatterns
              }
              assert(!xEmbeddedBlock, "problem with embedded block")
              
            case SU   =>
              throw TruncatedXML
              
            case _    => // text
              appendText(r2p(start1, start1, curOffset), ts, xText)
              // here xEmbeddedBlock might be true:
              // if (xEmbeddedBlock) throw new ApplicationError("after:"+text); // assert
          }
          true
        }
        
        while (doPattern) { }  // call until false
        xEndTag(qname)
        debugLastStartElement.pop
      }
      
      handle.makeXMLpat(r2p(start, start, curOffset), qname, ts)
    }
  } /* class MarkupParser */
}
