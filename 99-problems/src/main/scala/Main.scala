import scala.collection.immutable.Nil

object Main extends App {

  override def main(arg: Array[String]) = {
    println("Problem 1.1: " + last1(List(1, 1, 2, 3, 5, 8)));
    println("Problem 1.2: " + last2(List(1, 1, 2, 3, 5, 8)));
    println("Problem 2: " + penultimate(List(1, 1, 2, 3, 5, 8)));
    println("Problem 3: " + nth(2, List(1, 1, 2, 3, 5, 8)));
    println("Problem 4.1: " + length1(List(1, 1, 2, 3, 5, 8)));
    println("Problem 4.2: " + length2(List(1, 1, 2, 3, 5, 8)));
    println("Problem 4.3: " + length3(List(1, 1, 2, 3, 5, 8)));
    println("Problem 5.1: " + reverse1(List(1, 1, 2, 3, 5, 8)));
    println("Problem 5.2: " + reverse2(List(1, 1, 2, 3, 5, 8)));
    println("Problem 5.3: " + reverse3(List(1, 1, 2, 3, 5, 8)));
    println("Problem 6.1: " + isPalindrome1(List(1, 2, 3, 2, 1)));
    println("Problem 6.2: " + isPalindrome2(List(1, 2, 3, 2, 1)));
    println("Problem 6.3: " + isPalindrome3(List(1, 2, 3, 2, 1)));
    println("Problem 7: " + flatten(List(List(1, 1), 2, List(3, List(5, 8)))));
    println("Problem 8: " + compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)));
    println("Problem 9: " + pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)));
    println("Problem 10: " + encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)));
    println("Problem 11: " + encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)));
    println("Problem 12: " + decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))));
    println("Problem 13: " + encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)));
    println("Problem 14: " + duplicate(List('a, 'b, 'c, 'c, 'd)));
    println("Problem 15: " + duplicateN(3, List('a, 'b, 'c, 'c, 'd)));
    println("Problem 16.1: " + drop1(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)));
    println("Problem 16.2: " + drop2(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)));
    println("Problem 17.1: " + split1(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)));
    println("Problem 17.2: " + split2(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)));
    println("Problem 18.1: " + slice1(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)));
    println("Problem 18.2: " + slice2(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)));
    println("Problem 19.1: " + rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)));
    println("Problem 19.2: " + rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)));
    println("Problem 20.1: " + removeAt1(1, List('a, 'b, 'c, 'd)));
    println("Problem 20.2: " + removeAt2(1, List('a, 'b, 'c, 'd)));
  }

  def last1[A](in: List[A]): A = in.last
  def last2[A](in: List[A]): A = in match {
    case item :: Nil => item
    case _ :: tail => last2(tail)
    case _ => throw new NoSuchElementException
  }
  def penultimate[A](in: List[A]): A = in match {
    case item :: _ :: Nil => item
    case _ :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException
  }
  def nth[A](k: Int, in: List[A]): A = {
    def nthR[A](pos: Int, in: List[A]): A = (pos, in) match {
      case (0, item :: _) => item
      case (n, _ :: tail) => nthR(n - 1, tail)
      case (_, Nil) => throw new NoSuchElementException
    }
    nthR(k, in)
  }
  def length1[A](in: List[A]): Int = in.length
  def length2[A](in: List[A]): Int = in match {
    case Nil => 0
    case _ :: tail => length2(tail) + 1
  }
  def length3[A](in: List[A]): Int = in.foldLeft(0)((l, _) => l + 1)
  def reverse1[A](in: List[A]): List[A] = in.reverse
  def reverse2[A](in: List[A]): List[A] = in match {
    case Nil => Nil
    case item :: tail => reverse2(tail) ::: List(item)
  }
  def reverse3[A](in: List[A]): List[A] = in.foldLeft(List[A]()) { (a, b) => b :: a }
  def isPalindrome1[A](in: List[A]): Boolean = {
    def isPalindromeR[A](in: List[A], a: Int, b: Int): Boolean = {
      if (a <= b) (in(a) == in(b)) && isPalindromeR(in, a + 1, b - 1)
      else true
    }
    isPalindromeR(in, 0, in.length - 1)
  }
  def isPalindrome2[A](in: List[A]): Boolean = in == in.reverse
  def isPalindrome3[A](in: List[A]): Boolean = {
    def isPalindromeR[A](in: List[A], offset: Int): Boolean = {
      if (offset <= in.length / 2) (in(offset) == in(in.length - 1 - offset)) && isPalindromeR(in, offset + 1)
      else true
    }
    isPalindromeR(in, 0)
  }
  def flatten(in: List[Any]): List[Any] = in flatMap {
    case item: List[_] => flatten(item)
    case item => List(item)
  }
  def compress[A](in: List[A]): List[A] = {
    def compressR[A](in: List[A], prev: A): List[A] = in match {
      case item :: tail => {
        if (item == prev) compressR(tail, item)
        else item :: compressR(tail, item)
      }
      case Nil => Nil
    }
    in(0) :: compressR(in, in(0))
  }
  def pack[A](in: List[A]): List[List[A]] = {
    if (in == Nil) Nil
    else {
      def sppanned: (List[A], List[A]) = in.span { _ == in.head }
      List(sppanned._1) ::: pack(sppanned._2)
    }
  }
  def encode[A](in: List[A]): List[(A, Int)] = pack(in) map { item => (item.head, item.length) }
  def encodeModified(in: List[Any]): List[Any] = pack(in) map { item =>
    if (item.length == 1) item.head
    else (item.head, item.length)
  }
  def decode[A](in: List[(Int, A)]): List[A] = in flatMap { item => List.make(item._1, item._2) }
  def encodeDirect(in: List[Any]): List[(Any, Int)] = {
    if (in == Nil) Nil
    else {
      def sppanned: (List[Any], List[Any]) = in.span { _ == in.head }
      List((sppanned._1.head, sppanned._1.length)) ::: encodeDirect(sppanned._2)
    }
  }
  def duplicate[A](in: List[A]): List[A] = in flatMap { el => List(el, el) }
  def duplicateN[A](n: Int, in: List[A]): List[A] = in flatMap { List.make(n, _) }
  def drop1[A](n: Int, in: List[A]): List[A] = in.zipWithIndex filter { v => (v._2 + 1) % n != 0 } map { _._1 }
  def drop2[A](n: Int, in: List[A]): List[A] = {
    def dropR[A](n: Int, c: Int, in: List[A]): List[A] = in match {
      case Nil => Nil
      case item :: tail => if (c % n == 0) dropR(n, c + 1, tail) else List(item) ::: dropR(n, c + 1, tail)
    }
    dropR(n, 1, in)
  }
  def split1[A](n: Int, in: List[A]): (List[A], List[A]) = in.splitAt(n)
  def split2[A](n: Int, in: List[A]): (List[A], List[A]) = {
    def splitR[A](n: Int, c: Int, in: (List[A], List[A])): (List[A], List[A]) = in match {
      case (item, Nil) => (item, Nil)
      case (first, second :: tail) => if (c < n) splitR(n, c + 1, (first ::: List(second), tail)) else (first, second :: tail)
    }
    splitR(n, 0, (Nil, in))
  }
  def slice1[A](start: Int, finish: Int, in: List[A]): List[A] = in.slice(start, finish)
  def slice2[A](start: Int, finish: Int, in: List[A]): List[A] = {
    in.slice(start, finish)
    def sliceR[A](n: Int, start: Int, finish: Int, in: List[A]): List[A] = in match {
      case Nil => Nil
      case item :: tail if (n < start) => sliceR(n + 1, start, finish, tail)
      case item :: tail if (n >= start && n < finish) => item :: sliceR(n + 1, start, finish, tail)
      case item :: tail => Nil
    }
    sliceR(0, start, finish, in);
  }
  def rotate[A](n: Int, in: List[A]): List[A] = {
    def rotateR[A](i: Int, in: List[A]): List[A] = (i, in) match {
      case (_, Nil) => Nil
      case (position, item :: tail) if (position > 0) => rotateR(position - 1, tail ::: List(item))
      case (position, item :: tail) if (position < 0) => rotateR(in.length + position - 1, tail ::: List(item))
      case (position, list) => list
    }
    rotateR(n % in.length, in)
  }
  def removeAt1[A](n: Int, in: List[A]): (List[A], A) = in.splitAt(n) match {
    case (_, Nil) => throw new NoSuchElementException
    case (Nil, _) => throw new NoSuchElementException
    case (list, item :: tail) => (list ::: tail, item)
  }
  def removeAt2[A](n: Int, in: List[A]): (List[A], A) = {
    def removeAtR[A](i: Int, in: List[A]): (List[A], A) = (i, in) match {
      case (_, Nil) => throw new NoSuchElementException
      case (0, item :: tail) => (tail, item)
      case (position, item :: tail) => {
        var (l, e) = removeAtR(position - 1, tail)
        (item :: l, e)
      }
    }
    removeAtR(n, in)
  }
}