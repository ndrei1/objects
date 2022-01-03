package objsets

class TweetSetSuite extends munit.FunSuite :
  trait TestSets:
    val set1 = Empty()
    val set2 = set1.incl(Tweet("a", "a body", 20))
    val set3 = set2.incl(Tweet("b", "b body", 20))
    val c = Tweet("c", "c body", 7)
    val d = Tweet("d", "d body", 9)
    val s = Tweet("a", "a body2", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val set6 = set5.incl(s)
    val allTweets = TweetReader.allTweets

  def asSet(tweets: TweetSet): Set[Tweet] =
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets :
      assertEquals(size(set1.filter(tw => tw.user == "a")), 0)
  }

  test("filter: on empty set with retweets") {
    new TestSets :
      assertEquals(size(set1.filter(tw => tw.retweets == 1)), 0)
  }

  test("filter: on empty set with names") {
    new TestSets :
      assertEquals(size(set6.filter(tw => tw.user == "a")), 2)
  }

  test("filter: a on set5") {
    new TestSets :
      assertEquals(size(set6.filter(tw => tw.user == "b")), 1)
  }

  test("filter: 20 on set5") {
    new TestSets :
      assertEquals(size(set5.filter(tw => tw.retweets == 20)), 2)
  }

  test("filter: 7 on set5") {
    new TestSets :
      assertEquals(size(set5.filter(tw => tw.retweets == 7)), 1)
  }

  test("union: with empty set1") {
    new TestSets :
      assertEquals(size(set5.union(set1)), 4)
  }

  test("union: set4c and set4d") {
    new TestSets :
      assertEquals(size(set4c.union(set4d)), 4)
  }

  test("union: set4c and emtySet") {
    new TestSets :
      assertEquals(size(set4c.union(set1)), 3)
  }

  test("union: with empty set2") {
    new TestSets :
      assertEquals(size(set1.union(set5)), 4)
  }

  test("descending: on set5") {
    new TestSets :
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "b")
  }

  test("descending: emtySet") {
    new TestSets :
      intercept[NoSuchElementException]{
        val trends = set1.descendingByRetweet
        trends.head.user == "a"
      }
  }

  test("descending: on allTweets") {
    new TestSets :
      val trends = allTweets.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "mashable")
  }

  import scala.concurrent.duration.*

  override val munitTimeout = 10.seconds
