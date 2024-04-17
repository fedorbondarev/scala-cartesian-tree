package tree

trait Node[A] {
  def value: A
  def left: Option[Node[A]]
  def right: Option[Node[A]]
}

trait NodeWrapper[A, C] {
  def wrapNode(node: Node[A]): C
}
object NodeWrapper {
  implicit def defaultNodeWrapper[A]: NodeWrapper[A, Node[A]] = (node: Node[A]) => node
}

trait Tree[A] {

  def add(key: A): Tree[A]

  def delete(key: A): Option[Tree[A]]

  def foldLeft[B, C](acc: B)(op: (B, C) => B)(processSeq: (C, Seq[C]) => Seq[C])(implicit
    nodeWrapper: NodeWrapper[A, C]
  ): B

  def breadthFirstSearch[B](acc: B)(op: (B, A) => B): B

  def depthFirstSearch[B](acc: B)(op: (B, A) => B): B

  def max(searchFunction: A => ((A, A) => A) => A): A

  def min(searchFunction: A => ((A, A) => A) => A): A

  def size: Int

  def print(): Unit
}
