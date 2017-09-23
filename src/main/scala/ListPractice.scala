
object ListPractice {

  def main(args: Array[String]): Unit = {

    println("aaa")

    println(foldRight(List(1,2,3), Nil: List[Int])(_::_))
    println(length(List(1,2,3)))
    println(foldLeft(List(1,2,3), 0)((a,b) => a + b))
    println(reverse(List(1,2,3)))

    println(append(List(1,2,3), List(4,5,6)))
    println(append2(List(1,2,3), List(4,5,6)))

    println(flatten(List(List(1,2), List(3,4), List(5, 6, 7))))

    println(addOne(List(1,2,3), List()))
    println(map(List(1,2,3))(_ + 1))
    println(filter(List(1,2,3,4,5,6,7))(_ % 2 == 0))

    println(flatMap(List(1,2,3))(i => List(i, i)))
    println(zipWith(List(1,2,3, 8), List(1, 2, 3, 5))(_ + _))

    println(hasSubsequence(List(1,2,3,4,5), List()))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case x :: xs => f(x, foldRight(xs, z)(f))
    }
  }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case x :: xs => foldLeft(xs, f(z, x))(f)
    }
  }

  def length[A](as: List[A]): Int = {
    foldRight[A, Int](as, 0)((_, b) => b + 1)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((a, b) => b :: a)
  }

  def append[A](as: List[A], bs: List[A]) = {
    foldLeft(reverse(as), bs)((a, b) => b :: a)
  }

  def append2[A](as: List[A], bs: List[A]) = {
    foldRight(as, bs)((a, b) => a :: b)
  }

  def flatten[A](as: List[List[A]]): List[A] = {
    foldLeft(as, Nil: List[A])((a, b) => append(a, b))
  }

  def addOne(as: List[Int], bs: List[Int]): List[Int] = {
    as match {
      case Nil => reverse(bs)
      case x :: xs => addOne(xs, x + 1 :: bs)
    }
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    as match {
      case Nil => Nil
      case x :: xs => f(x) :: map(xs)(f)
    }
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    as match {
      case Nil => Nil
      case x :: xs => append(f(x), flatMap(xs)(f))
    }
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as){ x =>
      if(f(x)) List(x)
      else List()
    }
  }

  def zipWith[A, B](as: List[A], bs: List[A])(f: (A, A) => B): List[B] = {
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (x1 :: xs1, x2 :: xs2) => f(x1, x2) :: zipWith(xs1, xs2)(f)
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startAtSubsequence(sup2: List[A]): Boolean = {
      val list = zipWith(sup2, sub)((a, b) => a == b)
      list.take(sub.length).forall(a => a)
    }
    sup match {
      case Nil => false
      case _ => startAtSubsequence(sup) || hasSubsequence(sup.tail, sub)
    }
  }




}
