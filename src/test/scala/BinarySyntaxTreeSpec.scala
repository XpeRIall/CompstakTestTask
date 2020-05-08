import org.scalatest._
class BinarySyntaxTreeSpec extends FlatSpec {
  val testData = Seq(33, 12, 45, 5, 20, 39, 50, 4, 6, 19, 22, 38, 40, 49, 51)

  /* test case
	*           33
	*     12          45
	*   5    20     39   50
	*  4 6 19 22  38 40 49 51
	*/
  val tree: BinarySyntaxTree[Int] = BinarySyntaxTree(testData: _*)

  "BST" should "have expected depth" in {
    assert(tree.depth === 4)
  }

  it should "have values inserted in correct order" in {
    val result = for {
      min <- tree.min
      max <- tree.max
    } yield (min, max)

    val predicate = Option(testData.min, testData.max)
    assert(result === predicate)
  }

  it should "find minimal value" in {
    assert(tree.min.get === 4)
  }

  it should "find maximum value" in {
    assert(tree.max.get === 51)
  }

}
