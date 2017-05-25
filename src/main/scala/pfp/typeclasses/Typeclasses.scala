package pfp.intro

object Typeclasses {


  class User(val firstName: String, val lastName: String)

  def hello[A](a: A)(implicit sh: Show[A]): String = "hi " + sh.show(a)

  trait Show[A] { def show(a: A): String }
  // in Haskell terminology, type A is now constrained by the type class Show
  def hello1[A : Show](a: A): String = "hi " + implicitly[Show[A]].show(a)

  object Show {
    def apply[A : Show]: Show[A] = implicitly[Show[A]]

    implicit class ShowOps[A : Show](a: A) { def show: String = Show[A].show(a) }
  }
  def hello2[A : Show](a: A): String = "hi " + Show[A].show(a)
  def hello3[A : Show](a: A): String = {
    import Show._ // pull in that ShowOps class
    "hi " + a.show // compiler automagically wraps A with ShowOps and then we can call a.show
  }

  def _main(args: Array[String]) {
    implicit val UserShow: Show[User] = new Show[User] {
      def show(user: User) = user.firstName + " " + user.lastName
    }
    val user: User = new User("John", "The Camera Man")
    println(hello(user))
    println(hello1(user))
    println(hello2(user))
    println(hello3(user))
  }
}

