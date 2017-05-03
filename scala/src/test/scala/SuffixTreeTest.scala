import org.scalatest.{FlatSpec, Matchers}

class SuffixTreeTest extends FlatSpec with Matchers {

  "A Suffix Tree" should "take into account repeated letters" in {
    val tree = SuffixTree.build("abbc")
    tree.find("abc") shouldBe empty
  }

  "A Suffix Tree" should "find all substrings" in {
    val tree = SuffixTree.build("abbc")
    tree.find("a") shouldBe List(0)
    tree.find("b") shouldBe List(1, 2)
    tree.find("ab") shouldBe List(0)
    tree.find("abb") shouldBe List(0)
    tree.find("abbc") shouldBe List(0)
    tree.find("bbc") shouldBe List(1)
    tree.find("bc") shouldBe List(2)
    tree.find("c") shouldBe List(3)
    tree.find("bb") shouldBe List(1)
  }

  "A Suffix Tree" should "find repeated substrings" in {
    val tree = SuffixTree.build("abbcdizbbcdi")
    tree.find("bbcdi") shouldBe List(1, 7)
    tree.find("abbcdi") shouldBe List(0)
  }
}
