package tree

import scala.annotation.tailrec
import scala.math.Ordering.Implicits._

private object RandomValueGen {
  private val rand = new scala.util.Random
  def next(): Int = rand.nextInt()
}

private sealed trait WalkDirection
private object WalkDirection {
  object Right extends WalkDirection
  object Left extends WalkDirection
}

case class CartesianTreeNode[A](
  value: A,
  left: Option[CartesianTreeNode[A]],
  right: Option[CartesianTreeNode[A]]
)(implicit val valueOrdering: Ordering[A])
  extends Node[A] {
  private val randomValue = RandomValueGen.next()
}

object CartesianTreeNode {
  @tailrec
  private def splitWalkDown[A: Ordering](
    nodeOption: Option[CartesianTreeNode[A]],
    isNodeInLeft: CartesianTreeNode[A] => Boolean,
    acc: List[(CartesianTreeNode[A], WalkDirection)] = List.empty
  ): (List[(CartesianTreeNode[A], WalkDirection)], (Option[CartesianTreeNode[A]], Option[CartesianTreeNode[A]])) =
    nodeOption match {
      case None => (acc, (None, None))
      case Some(node) =>
        if (isNodeInLeft(node)) {
          val newAcc = (node, WalkDirection.Right) +: acc
          splitWalkDown(node.right, isNodeInLeft, newAcc)
        } else {
          val newAcc = (node, WalkDirection.Left) +: acc
          splitWalkDown(node.left, isNodeInLeft, newAcc)
        }
    }

  @tailrec
  private def splitWalkUp[A: Ordering](
    nodeStack: List[(CartesianTreeNode[A], WalkDirection)],
    splitPart: (Option[CartesianTreeNode[A]], Option[CartesianTreeNode[A]])
  ): (Option[CartesianTreeNode[A]], Option[CartesianTreeNode[A]]) =
    if (nodeStack.isEmpty)
      splitPart
    else {
      val (currentNode, walkDirection) = nodeStack.head
      val newSplitPart = walkDirection match {
        case WalkDirection.Left =>
          (splitPart._1, Some(currentNode.copy(left = splitPart._2)))
        case WalkDirection.Right =>
          (Some(currentNode.copy(right = splitPart._1)), splitPart._2)
      }
      splitWalkUp(nodeStack.tail, newSplitPart)
    }

  def split[A: Ordering](
    node: Option[CartesianTreeNode[A]],
    del: A,
    delNodeInLeft: Boolean = false
  ): (Option[CartesianTreeNode[A]], Option[CartesianTreeNode[A]]) = {
    val isNodeInLeft: CartesianTreeNode[A] => Boolean = if (delNodeInLeft) _.value <= del else _.value < del
    val (nodeStack, splitPart) = splitWalkDown(node, isNodeInLeft)
    splitWalkUp(nodeStack, splitPart)
  }

  def splitInThree[A: Ordering](
    node: Option[CartesianTreeNode[A]],
    key: A
  ): (Option[CartesianTreeNode[A]], Option[CartesianTreeNode[A]], Option[CartesianTreeNode[A]]) = {
    val (less, greaterOrEq) = CartesianTreeNode.split(node, key)
    val (eq, greater) = CartesianTreeNode.split(greaterOrEq, key, delNodeInLeft = true)
    (less, eq, greater)
  }

  @tailrec
  private def mergeWalkDown[A: Ordering](
    leftOption: Option[CartesianTreeNode[A]],
    rightOption: Option[CartesianTreeNode[A]],
    acc: List[(CartesianTreeNode[A], WalkDirection)] = List.empty
  ): (List[(CartesianTreeNode[A], WalkDirection)], Option[CartesianTreeNode[A]]) =
    (leftOption, rightOption) match {
      case (Some(left), Some(right)) =>
        if (left.randomValue > right.randomValue) {
          val newAcc = (left, WalkDirection.Right) +: acc
          val newLeftOption = left.right
          mergeWalkDown(newLeftOption, rightOption, newAcc)
        } else {
          val newAcc = (right, WalkDirection.Left) +: acc
          val newRightOption = right.left
          mergeWalkDown(leftOption, newRightOption, newAcc)
        }
      case (left, None)  => (acc, left)
      case (None, right) => (acc, right)
    }

  @tailrec
  private def mergeWalkUp[A: Ordering](
    nodeStack: List[(CartesianTreeNode[A], WalkDirection)],
    mergedPartOption: Option[CartesianTreeNode[A]]
  ): Option[CartesianTreeNode[A]] =
    mergedPartOption match {
      case _ if nodeStack.isEmpty => mergedPartOption
      case None =>
        mergeWalkUp(nodeStack.tail, Some(nodeStack.head._1))
      case Some(mergedPart) =>
        val (currentNode, walkDirection) = nodeStack.head
        val newMergedPart = walkDirection match {
          case WalkDirection.Left =>
            currentNode.copy(left = Some(mergedPart))
          case WalkDirection.Right =>
            currentNode.copy(right = Some(mergedPart))
        }
        mergeWalkUp(nodeStack.tail, Some(newMergedPart))
    }

  def merge[A: Ordering](
    leftOption: Option[CartesianTreeNode[A]],
    rightOption: Option[CartesianTreeNode[A]]
  ): Option[CartesianTreeNode[A]] = {
    val (nodeStack, mergedPart) = mergeWalkDown(leftOption, rightOption)
    mergeWalkUp(nodeStack, mergedPart)
  }
}

class CartesianTree[A: Ordering] private (private val root: CartesianTreeNode[A]) extends Tree[A] {
  override def add(key: A): Tree[A] = {
    val (less, _, greater) = CartesianTreeNode.splitInThree(Some(root), key)
    val keyNode = CartesianTreeNode(key, None, None)
    new CartesianTree(CartesianTreeNode.merge(less, CartesianTreeNode.merge(Some(keyNode), greater)).getOrElse(keyNode))
  }

  override def delete(key: A): Option[Tree[A]] = {
    val (less, _, greater) = CartesianTreeNode.splitInThree(Some(root), key)
    CartesianTreeNode.merge(less, greater).map(new CartesianTree[A](_))
  }

  override def foldLeft[B, C](
    acc: B
  )(op: (B, C) => B)(processSeq: (C, Seq[C]) => Seq[C])(implicit nodeWrapper: NodeWrapper[A, C]): B = {
    @tailrec
    def walk(acc: B, seq: Seq[C]): B = {
      if (seq.isEmpty)
        acc
      else {
        val newSeq = processSeq(seq.head, seq.tail)
        val newAcc = op(acc, seq.head)
        walk(newAcc, newSeq)
      }
    }
    walk(acc, IndexedSeq(nodeWrapper.wrapNode(root)))
  }

  private def foldLeftWithNodes[B](acc: B)(op: (B, A) => B)(
    processSeq: (Node[A], Seq[Node[A]]) => Seq[Node[A]]
  ): B = foldLeft[B, Node[A]](acc) { (acc: B, node: Node[A]) =>
    op(acc, node.value)
  }(processSeq)

  override def breadthFirstSearch[B](acc: B)(op: (B, A) => B): B =
    foldLeftWithNodes(acc)(op) { (node: Node[A], seq: Seq[Node[A]]) =>
      val leftAdded = node.left.fold(seq)(seq.appended)
      val rightAdded = node.right.fold(leftAdded)(leftAdded.appended)
      rightAdded
    }

  override def depthFirstSearch[B](acc: B)(op: (B, A) => B): B =
    foldLeftWithNodes(acc)(op) { (node: Node[A], seq: Seq[Node[A]]) =>
      val leftAdded = node.left.fold(seq)(seq.prepended)
      val rightAdded = node.right.fold(leftAdded)(leftAdded.prepended)
      rightAdded
    }

  private def findByOrder(searchFunction: A => ((A, A) => A) => A, compareFunction: (A, A) => Boolean): A =
    searchFunction(root.value)((acc: A, el: A) => if (compareFunction(acc, el)) el else acc)

  override def max(searchFunction: A => ((A, A) => A) => A): A = findByOrder(searchFunction, _ < _)
  override def min(searchFunction: A => ((A, A) => A) => A): A = findByOrder(searchFunction, _ > _)

  override def size: Int = depthFirstSearch(0) { (acc: Int, _: A) => acc + 1 }

  override def print(): Unit = {
    def formatEl(el: Option[Node[A]]): String = el match {
      case Some(node) => node.value.toString
      case None       => "()"
    }

    val treeHeight = foldLeft(1)((acc: Int, seqEl: (Node[A], Int)) => math.max(acc, seqEl._2)) {
      (seqEl: (Node[A], Int), seq: Seq[(Node[A], Int)]) =>
        val (node, h) = seqEl
        val withNextNode = (s: Seq[(Node[A], Int)], n: Node[A]) => s.appended((n, h + 1))
        val leftAdded = node.left.fold(seq)(withNextNode(seq, _))
        val rightAdded = node.right.fold(leftAdded)(withNextNode(leftAdded, _))
        rightAdded
    } { node: Node[A] => (node, 1) }

    implicit val printNodeWrapper: NodeWrapper[A, (Option[Node[A]], Int)] = (node: Node[A]) => (Some(node), 1)

    Console.print(
      foldLeft(("", 1)) { (acc: (String, Int), seqEl: (Option[Node[A]], Int)) =>
        if (acc._2 < seqEl._2)
          (s"${acc._1}\n${formatEl(seqEl._1)} ", seqEl._2)
        else
          (s"${acc._1}${formatEl(seqEl._1)} ", seqEl._2)
      } { (seqEl: (Option[Node[A]], Int), seq: Seq[(Option[Node[A]], Int)]) =>
        val h = seqEl._2
        val withNextNode = (s: Seq[(Option[Node[A]], Int)], n: Option[Node[A]]) => s.appended((n, h + 1))
        seqEl match {
          case (Some(node), _) if h < treeHeight =>
            withNextNode(withNextNode(seq, node.left), node.right)
          case (None, _) if h < treeHeight =>
            withNextNode(withNextNode(seq, None), None)
          case _ =>
            seq
        }
      }._1
    )
  }
}

object CartesianTree {
  def apply[A: Ordering](rootKey: A): CartesianTree[A] = new CartesianTree(CartesianTreeNode(rootKey, None, None))
}
