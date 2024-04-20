package tree

import cats.Monad
import cats.effect.IO
import cats.effect.std.Random
import cats.effect.unsafe.implicits.global
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxFlatMapOps}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CartesianTreeSpec extends AnyFlatSpec with Matchers {
  val ioRand: IO[Random[IO]] = Random.scalaUtilRandom[IO]
  def ioRandomInt: IO[Int] = ioRand >>= { rnd => rnd.nextInt }
  def ioRandomIntSeq(size: Int): IO[Seq[Int]] = {
    Monad[IO].tailRecM[(Int, Seq[Int]), Seq[Int]]((size, Seq.empty)) {
      case (0, acc)    => Right(acc).pure[IO]
      case (size, acc) => ioRandomInt.map { n => Left((size - 1, n +: acc)) }
    }
  }

  val simpleSampleSeq: Seq[Int] = List(0, 1, 2, 3, 4, 8, -10)
  val simpleSampleSeqMax: Int = simpleSampleSeq.max
  val simpleSampleSeqMin: Int = simpleSampleSeq.min

  "CartesianTree" should "create CartesianTree with one element" in {
    (ioRand >>= { implicit rnd: Random[IO] =>
      CartesianTree[IO, Int](123)
    }).unsafeRunSync()
  }

  "add" should "add elements to tree" in {
    (ioRand >>= { implicit rnd =>
      for {
        seq <- ioRandomIntSeq(100)
        _ <- seq.foldLeft[IO[Tree[IO, Int]]](CartesianTree[IO, Int](0))({ (ioAcc, el) =>
          for {
            acc <- ioAcc
            res <- acc.add(el)
          } yield res
        })
      } yield ()
    }).unsafeRunSync()
  }

  "delete" should "delete elements from non empty tree" in {
    (ioRand >>= { implicit rnd: Random[IO] =>
      (for {
        randomSeq <- ioRandomIntSeq(100)
        nonEmptyTree <- randomSeq.foldLeft[IO[Tree[IO, Int]]](CartesianTree[IO, Int](0)) { (ioAcc, el) =>
          ioAcc >>= (_.add(el))
        }
      } yield randomSeq.init.foldLeft[Option[Tree[IO, Int]]](nonEmptyTree.delete(0)) {
        (treeOption: Option[Tree[IO, Int]], el: Int) =>
          treeOption >>= (_.delete(el))
      }).map {
        case None => fail
        case _    => ()
      }
    }).unsafeRunSync()
  }

  "delete" should "return None on tree with one element" in {
    (ioRand >>= { implicit ioRnd =>
      CartesianTree[IO, Int](0).map(_.delete(0) match {
        case None => ()
        case _    => fail
      })
    }).unsafeRunSync()
  }

  "foldLeft" should "calculate function on tree" in {
    val seq = simpleSampleSeq
    (ioRand >>= { implicit rnd: Random[IO] =>
      for {
        del <- rnd.elementOf(seq)
        tree <- seq.foldLeft[IO[Tree[IO, Int]]](CartesianTree[IO, Int](seq.head))((acc, n) => acc >>= (_.add(n)))
      } yield tree.foldLeft("Love") { (acc, _: Node[Int]) => acc } { (seqEl: Node[Int], seq) =>
        def withNode(seq: Seq[Node[Int]], nodeOption: Option[Node[Int]]) = nodeOption match {
          case None => seq
          case Some(node) =>
            if (node.value >= del) seq.appended(node) else seq.prepended(node)
        }
        withNode(withNode(seq, seqEl.left), seqEl.right)
      }
    }).unsafeRunSync() shouldEqual "Love"
  }

  "breadthFirstSearch" should "calculate function on tree" in {
    val seq = simpleSampleSeq
    (ioRand >>= { implicit rnd: Random[IO] =>
      for {
        tree <- seq.foldLeft[IO[Tree[IO, Int]]](CartesianTree[IO, Int](seq.head))((acc, v) => acc >>= (_.add(v)))
      } yield tree.breadthFirstSearch("Love") { (acc, _) => acc }
    }).unsafeRunSync() shouldEqual "Love"
  }

  "depthFirstSearch" should "calculate function on tree" in {
    val seq = simpleSampleSeq
    (ioRand >>= { implicit rnd: Random[IO] =>
      for {
        tree <- seq.foldLeft[IO[Tree[IO, Int]]](CartesianTree[IO, Int](seq.head))((acc, v) => acc >>= (_.add(v)))
      } yield tree.depthFirstSearch("Love") { (acc, _) => acc }
    }).unsafeRunSync() shouldEqual "Love"
  }

  "size" should "return elements count in tree" in {
    val seq = Seq.range(0, 101)
    (ioRand >>= { implicit rnd: Random[IO] =>
      for {
        tree <- seq.foldLeft[IO[Tree[IO, Int]]](CartesianTree[IO, Int](seq.head))((acc, v) => acc >>= (_.add(v)))
      } yield tree.size
    }).unsafeRunSync() shouldEqual 101
  }

  "min with breadthFirstSearch" should "return min element" in {
    val seq = simpleSampleSeq
    (ioRand >>= { implicit rnd: Random[IO] =>
      for {
        tree <- seq.foldLeft[IO[Tree[IO, Int]]](CartesianTree[IO, Int](seq.head))((acc, v) => acc >>= (_.add(v)))
      } yield tree.min(tree.breadthFirstSearch)
    }).unsafeRunSync() shouldEqual simpleSampleSeqMin
  }

  "min with depthFirstSearch" should "return min element" in {
    val seq = simpleSampleSeq
    (ioRand >>= { implicit rnd: Random[IO] =>
      for {
        tree <- seq.foldLeft[IO[Tree[IO, Int]]](CartesianTree[IO, Int](seq.head))((acc, v) => acc >>= (_.add(v)))
      } yield tree.min(tree.depthFirstSearch)
    }).unsafeRunSync() shouldEqual simpleSampleSeqMin
  }

  "max with breadthFirstSearch" should "return max element" in {
    val seq = simpleSampleSeq
    (ioRand >>= { implicit rnd: Random[IO] =>
      for {
        tree <- seq.foldLeft[IO[Tree[IO, Int]]](CartesianTree[IO, Int](seq.head))((acc, v) => acc >>= (_.add(v)))
      } yield tree.max(tree.breadthFirstSearch)
    }).unsafeRunSync() shouldEqual simpleSampleSeqMax
  }

  "max with depthFirstSearch" should "return max element" in {
    val seq = simpleSampleSeq
    (ioRand >>= { implicit rnd: Random[IO] =>
      for {
        tree <- seq.foldLeft[IO[Tree[IO, Int]]](CartesianTree[IO, Int](seq.head))((acc, v) => acc >>= (_.add(v)))
      } yield tree.max(tree.depthFirstSearch)
    }).unsafeRunSync() shouldEqual simpleSampleSeqMax
  }
}
