/**
  * Created by hangyu.lin on 22/09/2017.
  */
object TreePractice {

  def main(args: Array[String]): Unit = {
    println("asda")

    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    println(map(tree)(_ + 1))
    println(maximun(tree))
  }

  sealed trait Tree[+A] {
    def size: Int
    def depth: Int
  }
  case class Leaf[A](value: A) extends Tree[A] {
    override def size: Int = 1
    override def depth: Int = 1
  }
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
    override def size: Int = left.size + right.size
    override def depth: Int = left.depth max right.depth
  }

  def maximun(tree: Tree[Int]): Int = {
    fold(tree, 0)(_ max _)
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case leaf: Leaf[A] => Leaf(f(leaf.value))
      case branch: Branch[A] => Branch(map(branch.left)(f), map(branch.right)(f))
    }
  }

  def fold[A, B](tree: Tree[A], z: B)(f: (A, B) => B): B = {
    tree match {
      case leaf: Leaf[A] => f(leaf.value, z)
      case branch: Branch[A] => fold(branch.left, fold(branch.right, z)(f))(f)
    }
  }



}
