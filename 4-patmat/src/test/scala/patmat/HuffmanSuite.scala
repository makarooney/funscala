package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times char count") {
    val testCharList = string2Chars("hello, world")
    val bla = times(testCharList)
    bla.foreach(pair => println(pair))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton Tree") {
    new TestTrees {
      val isSingleton = singleton(List(Leaf('a',2)))
      assert(isSingleton == true)
      assert(singleton(List(t1)) == true)

      val isNotSingleton = singleton(List(t1,t2))
      assert(isNotSingleton == false)
    }
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine a list that's already a singleton") {
    val singletonLeafList = List(Leaf('e', 1))
    val result = until(singleton, combine) (singletonLeafList)
    assert (result === singletonLeafList)
  }

  test("combine leaf list until there's only one code tree") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val result : List[CodeTree] = until(singleton, combine) (leaflist)
    assert(result.length == 1)
  }

  test("generate code tree for 'hello world'") {
    val testCharList = string2Chars("hello, world")
    val tree = createCodeTree(testCharList)
  }

  test("Print french code secret") {
    println(decodedSecret.toString())
  }

  test("decode and encode a single character: a") {
    new TestTrees {
      assert(decode(t2, encode(t2)("a".toList)) === "a".toList)
    }
  }

  test("decode and encode a single character: b") {
    new TestTrees {
      assert(decode(t2, encode(t2)("b".toList)) === "b".toList)
    }
  }

  test("decode and encode single character: d") {
    new TestTrees {
      assert(decode(t2, encode(t2)("d".toList)) === "d".toList)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t2, encode(t2)("ab".toList)) === "ab".toList)
    }
  }

  test("encode french code and compare with given french code") {
    val decodedSecret = decode(frenchCode, secret)
    assert(decode(frenchCode, encode(frenchCode)(decodedSecret)) === decodedSecret)
    assert(encode(frenchCode)("huffmanestcool".toList) === secret)
  }

  test("code tree -> code table, encode character b") {
    new TestTrees {
      val codetable = convert(t2)
      val charDTree = encode(t2)("b".toList)
      println(charDTree)
      val charDTable = codeBits(codetable)('b')
      println(charDTable)
      assert(charDTree === charDTable)
    }
  }

  test("Quick encode french code and compare with given french code") {
    val decodedSecret = decode(frenchCode, secret)
    println("French code: " + secret)
    println("Quick encode: " + quickEncode(frenchCode)(decodedSecret))
    assert(decode(frenchCode, quickEncode(frenchCode)(decodedSecret)) === decodedSecret)
    assert(quickEncode(frenchCode)("huffmanestcool".toList) === secret)
  }

}
