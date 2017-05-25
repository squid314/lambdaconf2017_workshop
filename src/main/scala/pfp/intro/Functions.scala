package pfp.intro

import scala.language.higherKinds
import scalaz._, Scalaz._

object Functions {

  /** Defining a function */

  val func: String => Int = (s: String) => s.length
  val func_alt: String => Int = _.length

  val func2: (Int, Int) => Int = (i1: Int, i2: Int) => i1 + i2
  val func2_alt: (Int, Int) => Int = _ + _ 

  /** Using functions */
  val list = List("hello", "world", "!")
  list.map(func)
  list.map(func_alt)
  list.map(_.length)
  
  /** Function composition */

  // f(x) = x + 1
  val f = (x: Int) => x + 1
  // g(x) = "x"
  val g = (x: Int) => x.toString

  // h(x) = g(f(x))
  val h: Int => String = g compose f
  val h_alt: Int => String = f andThen g
  // operators from ScalaZ
  val hz = g <<< f
  val hz_alt = f >>> g

  /** Higher Ordered Functions */
  val hof1: String => (Int => Int) =
    (str: String) => ((i: Int) => i + str.length)
  val hof2: (String => Int) => Int =
    (f: String => Int) => f("blablalbal")

  val hof1Val: Int => Int = hof1("hello")
  val hof2Val: Int = hof2(_.length)

  /** Curry */
  val threeArgs: (Int, String, Double) => Long =
    (i, s, d) => i.toLong + s.length.toLong + d.toLong
  val curried: Int => String => Double => Long = threeArgs.curried
  val oneArgApplied: String => Double => Long = curried(10)

  /** Parital application */
  val pa1: (String, Double) => Long = threeArgs(10, _:String, _:Double)
  val pa2: (Int, Double) => Long = threeArgs(_:Int, "hi", _:Double)

  /** Functions vs methods */
  class Foo(val str: String) {
    val func: Int => String = _.toString

    def method(i: Int): String = i.toString
  }

  val foo = new Foo("hello")
  foo.func(10)
  foo.method(10)

  /** do they compose */
  val temp: String => Int = _.toInt
  val a: String => String = temp andThen foo.func
  val a1: String => String = temp andThen foo.method
  val a2: Int => Int = foo.func andThen temp
  val a3: Int => Int = foo.method _ andThen temp

  /** methods can be polymorphic */
  def bar[A, B, C](a: A, b: B, make: (A, B) => C): C =
    make(a, b)

  bar[Int, Int, Int](10, 20, _ + _)
  bar[String, Int, Double]("hello", 20, (s, i) => (s.length + i).toDouble)

  /** higher kinded types */
  def buz[F[_], A](f: F[A], tr: F[A] => A): A = tr(f)
  val buzres1: Int = buz[Option, Int](10.some, _.get)
  val buzres2: String = buz[Option, String]("hello".some, _.get)

  val buz1: Either[String,Int] = ???
  buz[({type L[a] = Either[String, a]})#L, Int](buz1, _.right.get)
  buz[Either[String, ?], Int](buz1, _.right.get)
}
