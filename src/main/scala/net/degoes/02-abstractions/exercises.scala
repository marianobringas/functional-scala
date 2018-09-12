// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.abstractions

import scalaz._
import Scalaz._

object algebra {
  //
  // Semigroups: First (always returns the left),
  //             Right (always returns the right),
  //             Min,
  //             Max,
  //             Constant (always return the same value)
  //


  //
  // EXERCISE 1
  //
  // Define a semigroup for `NotEmpty` below.
  //
  case class NotEmpty[+A](head: A, tail: Option[NotEmpty[A]])
  implicit def NotEmptySemigroup[A]: Semigroup[NotEmpty[A]] =
   new Semigroup[NotEmpty[A]] {
     override def append(f1: NotEmpty[A], f2: => NotEmpty[A]): NotEmpty[A] = {
       NotEmpty(f1.head, f1.tail match {
         case Some(t) => Some(append(t, f2))
         case None => Some(f2)
       })
     }
   }
  val example1 = NotEmpty(1, None) |+| NotEmpty(2, None)

  //
  // EXERCISE 2
  //
  // Design a permission system for securing some resource, together with a
  // monoid for the permission data structure.
  //

  type Email = String
  type Resource = String

  sealed trait Capability
  case object Read extends Capability
  case object Write extends Capability

  case class Permission(value: Map[Email, Map[Resource, Set[Capability]]])
  object Permission {
    def apply() = {
      new Permission(Map.empty)
    }
  }
  implicit val MonoidPermission: Monoid[Permission] = new Monoid[Permission] {
    override def append(f1: Permission, f2: => Permission): Permission = {
      Permission(f1.value |+| f2.value)
    }

    override def zero: Permission = Permission(Map.empty)
  }
  val example2 = mzero[Permission] |+| Permission()

  //
  // EXERCISE 3
  //
  // Define an instance of `Semigroup` for `(A, B)` when both `A` and
  // `B` form semigroups.
  //
  implicit def SemigroupTuple2[A: Semigroup, B: Semigroup]:
    Semigroup[(A, B)] = new Semigroup[(A, B)] {
    override def append(f1: (A, B), f2: => (A, B)): (A, B) = {
      (f1._1 |+| f2._1, f1._2 |+| f2._2)
    }
  }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Monoid` for `NotEmpty` for any type `A`.
  //
  implicit def MonoidNotEmpty[A]: Monoid[NotEmpty[A]] = ???
}

object functor {
  //
  // EXERCISE 1
  //
  // Define an instance of `Functor` for `BTree`.
  //
  sealed trait BTree[+A]
  case class Leaf[A](a: A) extends BTree[A]
  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]
  implicit val BTreeFunctor: Functor[BTree] =
    new Functor[BTree] {
      def map[A, B](fa: BTree[A])(f: A => B): BTree[B] =
        fa match {
          case Leaf(a) => Leaf(f(a))
          case Fork(l, r) => Fork(map(l)(f), map(r)(f))
        }
    }

  //
  // EXERCISE 2
  //
  // Define an instance of `Functor` for `Nothing`.
  //
  implicit val NothingFunctor: Functor[Nothing] = new Functor[Nothing] {
    override def map[A, B](fa: Nothing)(f: A => B): Nothing = fa
  }

  //
  // EXERCISE 3
  //
  // Define an instance of `Functor` for Parser[E, ?].
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)])
  implicit def ParserFunctor[E]: Functor[Parser[E, ?]] = new Functor[Parser[E, ?]] {
    override def map[A, B](fa: Parser[E, A])(f: A => B): Parser[E, B] = {
      Parser(input => fa.run(input) match {
        case Left(e) => Left(e)
        case Right((s, a)) => Right((s, f(a)))
      })
    }
  }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Functor` for the following data type.
  //
  case class DataType[A](f: A => A)
  implicit val DataTypeFunctor: Functor[DataType] = ???

  //
  // EXERCISE 5
  //
  // Define an instance of `Functor` for `FunctorProduct`.
  //
  case class FunctorProduct[F[_], G[_], A](l: F[A], r: G[A])
  implicit def FunctorProductFunctor[F[_]: Functor, G[_]: Functor]:
    Functor[FunctorProduct[F, G, ?]] = new Functor[FunctorProduct[F,G,?]] {

    override def map[A, B](fa: FunctorProduct[F, G, A])(f: A => B): FunctorProduct[F, G, B] = {
      FunctorProduct(fa.l.map(f), fa.r.map(f))
    }

  }

  //
  // EXERCISE 6
  //
  // Define an instance of `Functor` for `FunctorSum`.
  //
  case class FunctorSum[F[_], G[_], A](run: Either[F[A], G[A]])
  implicit def FunctorSumFunctor[F[_]: Functor, G[_]: Functor]:
    Functor[FunctorSum[F, G, ?]] = new Functor[FunctorSum[F,G,?]] {
    override def map[A, B](fa: FunctorSum[F, G, A])(f: A => B): FunctorSum[F, G, B] = {
      FunctorSum(fa.run match {
        case Left(a) => Left(a.map(f))
        case Right(a) => Right(a.map(f))
      })
    }
  }

  //
  // EXERCISE 7
  //
  // Define an instance of `Functor` for `FunctorNest`.
  //
  case class FunctorNest[F[_], G[_], A](run: F[G[A]])
  implicit def FunctorNestFunctor[F[_]: Functor, G[_]: Functor]:
    Functor[FunctorNest[F, G, ?]] = new Functor[FunctorNest[F,G,?]] {
    override def map[A, B](fa: FunctorNest[F, G, A])(f: A => B): FunctorNest[F, G, B] = {
      FunctorNest(fa.run.map(_.map(f)))
    }
  }

  def zipOption[A,B](l: Option[A], r: Option[B]): Option[(A,B)] = (l, r) match {
    case (Some(a), Some(b)) => Some((a, b))
    case _ => None
  }

  def zipWith[A,B,C](l: Option[A], r: Option[B])(f: ((A, B)) => C): Option[C] =
    zipOption(l, r).map(f)

  def zipList1[A, B](l: List[A], r: List[B]): List[(A, B)] =
    (l, r) match {
      case (a :: as, bs) => zipList1(as, bs) ++ bs.map(b => (a,b))
      case (Nil, bs) => Nil
    }

  def zipList2[A, B](l: List[A], r: List[B]): List[(A, B)] =
    (l, r) match {
      case (a :: as, b :: bs) => (a, b) :: zipList2(as, bs)
      case _ => Nil
    }

  //
  // EXERCISE 8
  //
  // Define `Applicative` for `Option`.
  //
  implicit val OptionApplicative: Applicative[Option] =
    new Applicative[Option] {
      def point[A](a: => A): Option[A] = Some(a)

      def ap[A, B](fa: => Option[A])(f: => Option[A => B]): Option[B] =
        (fa, f) match {
          case (Some(a), Some(f)) => Some(f(a))
          case _ => None
        }
    }

  //
  // EXERCISE 9
  //
  // Implement `zip` in terms of the applicative composition using `|@|`.
  //
  val example1 = (Option(3) |@| Option(5))((_, _))
  val example2 = zip(Option(3), Option("foo")) : Option[(Int, String)]
  def zip[F[_]: Applicative, A, B](l: F[A], r: F[B]): F[(A, B)] =
    (l |@| r)((_, _))
  def ap2[F[_]: Applicative, A, B](fa: F[A], fab: F[A => B]): F[B] =
    zip(fa, fab).map { case (a,f) => f(a) }

  //
  // EXERCISE 10
  //
  // Define an instance of `Applicative` for `Parser[E, ?]`.
  //
  implicit def ApplicativeParser[E]: Applicative[Parser[E, ?]] =
    new Applicative[Parser[E, ?]] {
      def point[A](a: => A): Parser[E,A] = Parser(input => Right((input, a)))

      def ap[A, B](fa: => Parser[E,A])(
        f: => Parser[E, A => B]): Parser[E,B] = Parser(input => f.run(input) match {
        case Left(e) => Left(e)
        case Right((s, f)) => fa.run(s) match {
          case Left(e) => Left(e)
          case Right((s2, a)) => Right((s2, f(a)))
        }
      })
    }

  //
  // EXERCISE 11
  //
  // Define an instance of `Monad` for `BTree`.
  //
  implicit val MonadBTree: Monad[BTree] = new Monad[BTree] {

    override def point[A](a: => A): BTree[A] = Leaf(a)

    override def bind[A, B](fa: BTree[A])(f: A => BTree[B]): BTree[B] = fa match {
      case Leaf(a) => f(a)
      case Fork(l, r) => Fork(bind(l)(f), bind(r)(f))
    }

  }

  implicit val OptionMonad: Monad[Option] = new Monad[Option] {
    override def bind[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
      case Some(a) => f(a)
      case None => None
    }

    override def point[A](a: => A): Option[A] = Some(a)
  }

  implicit val ListMonad: Monad[List] = new Monad[List] {
    override def bind[A, B](fa: List[A])(f: A => List[B]): List[B] = fa match {
      case a :: as => f(a) ++ bind(as)(f)
      case Nil => Nil
    }

    override def point[A](a: => A): List[A] = List(a)
  }

  //
  // EXERCISE 12
  //
  // Define an instance of `Monad` for `Parser[E, ?]`.
  //
  implicit def MonadParser[E]: Monad[Parser[E, ?]] = new Monad[Parser[E, ?]] {
    override def point[A](a: => A): Parser[E, A] = Parser(input => Right((input, a)))

    override def bind[A,B](fa: Parser[E,A])(f: A => Parser[E,B]): Parser[E,B] = {
      Parser(input => fa.run(input) match {
        case Left(e) => Left(e)
        case Right((s, a)) => f(a).run(s)
      })
    }
  }
}

object foldable {
  //
  // EXERCISE 1
  //
  // Define an instance of `Foldable` for `BTree`.
  //
  sealed trait BTree[+A]
  case class Leaf[A](a: A) extends BTree[A]
  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]

  implicit val FoldableBTree: Foldable[BTree] =
    new Foldable[BTree] {
      def foldMap[A, B](fa: BTree[A])(f: A => B)(implicit F: scalaz.Monoid[B]): B =
        fa match {
          case Leaf(a) => f(a)
          case Fork(l, r) => foldMap(l)(f) |+| foldMap(r)(f)
        }

      def foldRight[A, B](fa: BTree[A],z: => B)(f: (A, => B) => B): B =
        fa match {
          case Leaf(a) => f(a, z)
          case Fork(l, r) =>
            foldRight(l, foldRight(r,z)(f))(f)

        }
    }

  //
  // EXERCISE 2
  //
  // Try to define an instance of `Foldable` for `A => ?`.
  //
  implicit def FunctionFoldable[A]: Foldable[A => ?] = ???

  //
  // EXERCISE 3
  //
  // Define an instance of `Traverse` for `BTree`.
  //
  implicit val TraverseBTree: Traverse[BTree] = new Traverse[BTree] {
    override def traverseImpl[G[_], A, B](fa: BTree[A])(f: A => G[B])(implicit F: Applicative[G]): G[BTree[B]] = {
      fa match {
        case Leaf(a) => f(a).map[BTree[B]](Leaf(_))
        case Fork(l, r) => (traverseImpl(l)(f) |@| traverseImpl(r)(f))(Fork(_, _))
      }
    }
  }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Traverse` for `Parser[E, ?]`.
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)])
  implicit def TraverseParser[E]: Traverse[Parser[E, ?]] = ???
}

object optics {

  sealed trait Country
  object Country {
    val usa: Prism[Country, Unit] = Prism[Country, Unit]({
      case USA => Some(Unit)
      case _ => None
    }, _ => USA)

    val uk: Prism[Country, Unit] = Prism[Country, Unit]({
      case UK => Some(Unit)
      case _ => None
    }, _ => UK)

    val poland: Prism[Country, Unit] = Prism[Country, Unit]({
      case Poland => Some(Unit)
      case _ => None
    }, _ => Poland)

    def value(c: Country): Prism[Country, Unit] = Prism({
      case `c` => Some(())
      case _ => None
    }, _ => c)
  }
  case object USA extends Country
  case object UK extends Country
  case object Poland extends Country

  case class Org(name: String,
                 address: Address,
                 site: Site)

  object Org {
    val site: Lens[Org, Site] =
      Lens[Org, Site](_.site, l => _.copy(site = l))
  }

  case class Site(manager: Employee,
                  address: Address,
                  employees: Set[Employee])

  object Site {
    val manager: Lens[Site, Employee] =
      Lens[Site, Employee](_.manager, m => _.copy(manager = m))
  }

  case class Address(number: String,
                     street: String,
                     postalCode:
                     String,
                     country: Country)

  case class Employee(name: String,
                      dob: java.time.Instant,
                      salary: BigDecimal,
                      address: Address)

  object Employee {
    val salary: Lens[Employee, BigDecimal] =
      Lens[Employee, BigDecimal](_.salary, s => _.copy(salary = s))
  }


  lazy val org: Org = ???

  lazy val org2 = org.copy(site = org.site.copy(
                                   manager = org.site.manager.copy(
                                     salary = org.site.manager.salary * 0.95)))

  lazy val org2_lenses: Org =
    (Org.site >>> Site.manager >>> Employee.salary).update(_ * 0.95)(org)

  // S - Super structure
  // A - Sub structure
  // An Optic S A allows you to focus in on a sub structure A
  // inside a super structure S,
  // for purposes of accessing or modifying the substructure
  //
  // Lenses focus in on a term in a Product type
  // Prisms focus in on a term in a Sum type

  // type Optics[S, A]
  //
  // case class Lens[S, A] (
  //    get: S => A
  //    set: A => (S => S)
  //)
  //


  final case class Lens[S, A] (get: S => A, set: A => (S => S)) {
    self =>

    def >>> [B](that: Lens[A, B]): Lens[S, B] = Lens[S, B](get = (s: S) => that.get(self.get(s)),
                                                     set = (b: B) => (s: S) => self.set(that.set(b)(self.get(s)))(s))

    def update(f: A => A): S => S =
      (s: S) => self.set(f(self.get(s)))(s)
  }

  final case class Prism[S, A] (get: S => Option[A], set: A => S) {
    self =>

    def >>> [B](that: Prism[A, B]): Prism[S, B] = Prism[S, B](
      get = (s: S) => self.get(s).flatMap(that.get),
      set = self.set.compose(that.set)
    )

    def select(implicit ev: Unit =:= A): S = set(ev(()))
  }

  def _Left[A,B]: Prism[Either[A, B], A] = Prism[Either[A, B], A]({
    case Left(a) => Some(a)
    case _ => None
  }, Left(_))

  def _Right[A,B]: Prism[Either[A, B], B] = Prism[Either[A, B], B]({
    case Right(b) => Some(b)
    case _ => None
  }, Right(_))

}
