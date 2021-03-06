import java.io.StringReader
import org.xml.sax.InputSource

import scala.testing.SUnit.Assert
import scala.xml.{Node, NodeSeq, Elem, Text, XML}

object Test extends AnyRef with Assert {

  /** returns true if exception was thrown */
  def catcher(att: Function1[Unit, scala.xml.MetaData]): Boolean = {
    var ex = false
    try {
      att.apply({})
    } catch {
      case scala.xml.MalformedAttributeException(msg) =>
        println(msg)
        ex = true
    }
    ex
  }

  def main(args: Array[String]) {

  //val e:  scala.xml.MetaData         = null; //Node.NoAttributes;
  //val sc: scala.xml.NamespaceBinding = null;

    // ------------------------------------------ tests for class NodeSeq

    /**
    println("checking wellformed attributes");
    {
    import scala.xml.{ UnprefixedAttribute, Null }
    assertTrue(catcher {x:Unit => new UnprefixedAttribute("key", "<", Null)});   // < illegal
    assertTrue(catcher(x:Unit => new UnprefixedAttribute("key", "&", Null)));   // & illegal
    assertTrue(catcher(x:Unit => new UnprefixedAttribute("key", "a&a", Null))); // & illegal
    assertTrue(catcher(x:Unit => new UnprefixedAttribute("key", "a&a;&", Null))); // 2nd &

    assertFalse(catcher(x:Unit => new UnprefixedAttribute("key", "a&a; &lt;&lt;", Null)));
    }
*/

/*
checking wellformed attributes
< not allowed in attribute value
passed ok
malformed entity reference in attribute value [&]
passed ok
malformed entity reference in attribute value [a&a]
passed ok
malformed entity reference in attribute value [a&a;&]
passed ok
passed ok
*/

  println("NodeSeq")

    val p = <foo>
              <bar gt='ga' value="3"/>
              <baz bazValue="8"/>
              <bar value="5" gi='go'/>
            </foo>;
  
    val pelems_1 = for( val x <- p \ "bar"; val y <- p \ "baz" ) yield {
      Text(x.attributes("value").toString + y.attributes("bazValue").toString+ "!")
    };
    val pelems_2 = new NodeSeq { val theSeq = List(Text("38!"),Text("58!")) };
    assertSameElements(pelems_1, pelems_2)

    assertEquals(p \\ "@bazValue", Text("8"))

    val books = 
    <bks>
      <book><title>Blabla</title></book>
      <book><title>Blubabla</title></book>
      <book><title>Baaaaaaalabla</title></book>
    </bks>;

  val reviews = 
    <reviews>
      <entry><title>Blabla</title>
      <remarks>
        Hallo Welt.
      </remarks>
    </entry>
      <entry><title>Blubabla</title>
      <remarks>
        Hello Blu
      </remarks>
  </entry>
      <entry><title>Blubabla</title>
      <remarks>
        rem 2
      </remarks>
  </entry>
    </reviews>;

  println( new scala.xml.PrettyPrinter(80, 5).formatNodes (
    for (t <- books \\ "title";
         r <- reviews \\ "entry"
         if r \ "title" == t) yield
          <result>
    { t }
    { r \ "remarks" }
    </result>
  ));

  // example
  println( 
    for (t @ <book><title>Blabla</title></book> <- new NodeSeq { val theSeq = books.child }.toList)
    yield t
  );
val phoneBook =  
  <phonebook>
      <descr>
        This is the <b>phonebook</b> of the 
        <a href="http://acme.org">ACME</a> corporation.
      </descr>
      <entry>
        <name>John</name> 
        <phone where="work">  +41 21 693 68 67</phone>
        <phone where="mobile">+41 79 602 23 23</phone>
      </entry>
    </phonebook>;


val addrBook =  
  <addrbook>
      <descr>
        This is the <b>addressbook</b> of the 
        <a href="http://acme.org">ACME</a> corporation.
      </descr>
      <entry>
        <name>John</name> 
        <street> Elm Street</street>
        <city>Dolphin City</city>
      </entry>
    </addrbook>;

  println( new scala.xml.PrettyPrinter(80, 5).formatNodes (
    for (t <- addrBook \\ "entry";
         r <- phoneBook \\ "entry"
         if t \ "name" == r \ "name") yield
          <result>
    { t.child }
    { r \ "phone" }
    </result>
  ));

  
  /* namespaces */
   // begin tmp
  println("namespaces")
  val cuckoo = <cuckoo xmlns="http://cuckoo.com">
    <foo/>
    <bar/>
  </cuckoo>;
  assertEquals(cuckoo.namespace, "http://cuckoo.com")
  for (n <- cuckoo \ "_" ) {
    //println("n = "+n);
    //println("n.prefix = "+n.prefix);
    //.println("n.scope = "+n.scope);
    assertEquals( n.namespace, "http://cuckoo.com")
  }

  println("validation - elements")
  val vtor = new scala.xml.dtd.ElementValidator();
  {
    import scala.xml.dtd.ELEMENTS
    import scala.xml.dtd.ContentModel._
    vtor.setContentModel(
	  ELEMENTS( 
	    Sequ(
		  Letter(ElemName("bar")), 
		  Star(Letter(ElemName("baz"))) )));

  }
  assertEquals(vtor( <foo><bar/><baz/><baz/></foo> ), true);
  {
    import scala.xml.dtd.MIXED
    import scala.xml.dtd.ContentModel._
    
    vtor.setContentModel(
      MIXED(
        Alt(Letter(ElemName("bar")), 
            Letter(ElemName("baz")), 
            Letter(ElemName("bal")))));
  }

  assertEquals(vtor(<foo><bar/><baz/><baz/></foo> ), true)
  assertEquals(vtor(<foo>ab<bar/>cd<baz/>ed<baz/>gh</foo> ), true)
  assertEquals(vtor(<foo> <ugha/> <bugha/> </foo> ), false)

  println("validation - attributes")
  vtor.setContentModel(null)
  vtor.setMetaData(List())
  assertEquals(vtor( <foo bar="hello"/> ), false)
  
  { 
    import scala.xml.dtd._ 
    vtor setMetaData List(AttrDecl("bar", "CDATA", IMPLIED))
  }
  assertEquals(vtor(<foo href="http://foo.com" bar="hello"/>), false)
  assertEquals(vtor(<foo bar="hello"/>), true)

  { 
    import scala.xml.dtd._
    vtor.setMetaData(List(AttrDecl("bar","CDATA",REQUIRED)))
  }
  assertEquals( vtor( <foo href="http://foo.com" /> ), false )
  assertEquals( vtor( <foo bar="http://foo.com" /> ), true )
  
  }
}
