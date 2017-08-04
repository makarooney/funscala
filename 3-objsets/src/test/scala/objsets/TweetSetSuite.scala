package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set3c = set3.incl(c)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)

    val setJerno = ((new Empty).incl(d)).incl(c)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)

    }
  }

  test("calling mostRetweeted on an empty set should throw an exception") {
    new TestSets {
      intercept[NoSuchElementException] {
        set1.mostRetweeted
      }
    }
  }

  test("most retweeted only one element") {
    new TestSets {
      assert(set2.mostRetweeted.user === "a")
    }
  }

  test("most retweetedtwo of a kind only returls first?") {
    new TestSets {
      assert(set5.mostRetweeted.user === "a")
    }
  }

  test("most retweeted no ambiguity only 1 is largest") {
    new TestSets {
      assert(set3c.mostRetweeted.user === "a")
    }
  }

  test("descending: empty list") {
    new TestSets {
      val testEmpty = set1.descendingByRetweet
      assert(testEmpty.isEmpty)
      //assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  test("descending: max count is 9") {
    new TestSets {
      val trends = setJerno.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "d")
      assert(trends.head.retweets == 9)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
      assert(trends.head.retweets == 20)
    }
  }

  }
