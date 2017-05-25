package pfp.typeclasses

object Exercise {
 
  /** Typeclasses */
  // 1. Implement Equalz[A] typeclass that has method def eq(a1: A, a2: A): Boolean
  trait Equalz[A] { def equ(a1: A, a2: A): Boolean }

  // 2. Write EqualzOps & Equalz companion object with apply method
  object Equalz {
    def apply[A : Equalz]: Equalz[A] = implicitly[Equalz[A]]

    implicit class EqualzOps[A : Equalz](a: A) { def equ(a2: A): Boolean = Equalz[A].equ(a,a2) }
  }

  // 3. Write instance for String and User
  case class User(login: String)
  implicit val StringEqualz: Equalz[String] = new Equalz[String] { def equ(a1:String,a2:String) = a1 == a2 }
  import Equalz._
  implicit val UserEqualz: Equalz[User] = new Equalz[User] { def equ(u1:User,u2:User) = (u1.login).equ(u2.login) }

  // 4. if A & B have instance for Equalz, can u write generic instance for Tuple (A, B)
  // because the types inside of the tuple are not known, we can't use an implicit val and must use an implicit def to generate the value
  implicit def Tuple2Equalz[A : Equalz, B : Equalz]: Equalz[(A, B)] =
    new Equalz[(A, B)] { def equ(t1: (A,B), t2: (A,B)) = t1._1.equ(t2._1) && t1._2.equ(t2._2) }
  // btw, we could do the same thing if we only had one type parameter we cared about
//  implicit def Tuple2FirstEqualz[A : Equalz, B]: Equalz[(A,B)] =
//    new Equalz[(A, B)] { def equ(t1: (A,B), t2: (A,B)) = t1._1.equ(t2._1) /*&& t1._2.equ(t2._2)*/ }
  // well... that's not working

  def _main(args: Array[String]) {
    val weirdo1: (String, User) = ("blah", User("asdf"))
    val weirdo2: (String, User) = ("blah", User("asdf"))
    val weirdo3: (String, User) = ("asdf", User("asdf"))
    val weirdo4: (String, User) = ("asdf", User("blah"))
    println(weirdo1 equ weirdo2)
    println(weirdo1 equ weirdo3)
    println(weirdo1 equ weirdo4)
    println(weirdo2 equ weirdo3)
    println(weirdo2 equ weirdo4)
    println(weirdo3 equ weirdo4)
  }
}
