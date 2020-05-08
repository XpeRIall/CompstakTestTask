object Main extends App {
  val keys = Seq(33, 12, 45, 5, 20, 39, 50)
  val tree = BinarySyntaxTree[Int](keys: _*)
  print(tree.depth)
}
