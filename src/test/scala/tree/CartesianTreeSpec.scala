package tree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class CartesianTreeSpec extends AnyFlatSpec with Matchers {
  "CartesianTree" should "create CartesianTree with one element" in {
    CartesianTree(123)
  }

  "add" should "add elements to tree" in {
    Seq.fill(100)(Random.nextInt).foldLeft(CartesianTree(0): Tree[Int])(_.add(_))
  }

  "delete" should "delete elements from non empty tree" in {
    val randomSeq = Seq.fill(100)(Random.nextInt)
    val nonEmptyTree = randomSeq.foldLeft(CartesianTree(0): Tree[Int])(_.add(_))
    randomSeq.init.foldLeft(nonEmptyTree.delete(0)) { (treeOption: Option[Tree[Int]], el: Int) =>
      treeOption.flatMap(_.delete(el))
    } match {
      case None => fail
      case _    => ()
    }
  }

  "delete" should "return None on tree with one element" in {
    CartesianTree(0).delete(0) match {
      case None => ()
      case _    => fail
    }
  }

  "foldLeft" should "calculate function on tree" in {
    val seq = List(0, 1, 2, 3, 4, 8, -10)
    val tree = seq.foldLeft(CartesianTree(seq.head): Tree[Int])(_.add(_))
    tree.foldLeft("Love") { (acc, _: Node[Int]) => acc } { (seqEl: Node[Int], seq) =>
      def withNode(seq: Seq[Node[Int]], nodeOption: Option[Node[Int]]) = nodeOption match {
        case None => seq
        case Some(node) =>
          if (Random.nextBoolean) seq.appended(node) else seq.prepended(node)
      }
      withNode(withNode(seq, seqEl.left), seqEl.right)
    } shouldEqual "Love"
  }

  "breadthFirstSearch" should "calculate function on tree" in {
    val seq = List(0, 1, 2, 3, 4, 8, -10)
    val tree = seq.foldLeft(CartesianTree(seq.head): Tree[Int])(_.add(_))
    tree.breadthFirstSearch("Love") { (acc, _) => acc } shouldEqual "Love"
  }

  "depthFirstSearch" should "calculate function on tree" in {
    val seq = List(0, 1, 2, 3, 4, 8, -10)
    val tree = seq.foldLeft(CartesianTree(seq.head): Tree[Int])(_.add(_))
    tree.depthFirstSearch("Love") { (acc, _) => acc } shouldEqual "Love"
  }

  "size" should "return elements count in tree" in {
    val randomSeq = Seq.fill(100)(Random.nextInt)
    val tree = randomSeq.foldLeft(CartesianTree(0): Tree[Int])(_.add(_))
    tree.size shouldEqual 101
  }

  "min with breadthFirstSearch" should "return min element" in {
    val seq = List(0, 1, 2, 3, 4, 8, -10)
    val tree = seq.foldLeft(CartesianTree(seq.head): Tree[Int])(_.add(_))
    tree.min(tree.breadthFirstSearch) shouldEqual -10
  }

  "min with depthFirstSearch" should "return min element" in {
    val seq = List(0, 1, 2, 3, 4, 8, -10)
    val tree = seq.foldLeft(CartesianTree(seq.head): Tree[Int])(_.add(_))
    tree.min(tree.depthFirstSearch) shouldEqual -10
  }

  "max with breadthFirstSearch" should "return max element" in {
    val seq = List(0, 1, 2, 3, 4, 8, -10)
    val tree = seq.foldLeft(CartesianTree(seq.head): Tree[Int])(_.add(_))
    tree.max(tree.breadthFirstSearch) shouldEqual 8
  }

  "max with depthFirstSearch" should "return max element" in {
    val seq = List(0, 1, 2, 3, 4, 8, -10)
    val tree = seq.foldLeft(CartesianTree(seq.head): Tree[Int])(_.add(_))
    tree.max(tree.depthFirstSearch) shouldEqual 8
  }
}
