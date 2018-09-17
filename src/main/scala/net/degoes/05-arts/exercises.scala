// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.arts

import scalaz.{MonadReader, _}
import Scalaz._
import scalaz.zio._
import scalaz.zio._

object exercises {


  // Logging using monad transformers while using final tagless

  case class LoggingT[F[_], A](
                                run: List[String] => F[(List[String], A)]) { self =>
    def map[B](f: A => B)(implicit F: Functor[F]): LoggingT[F, B] =
      LoggingT[F, B](log => self.run(log).map(t => (t._1, f(t._2))))

    def flatMap[B](f: A => LoggingT[F, B])(implicit F: Monad[F]): LoggingT[F, B] =
      LoggingT[F, B](log => self.run(log).flatMap(t => f(t._2).run(t._1)))

    def eval(implicit F: Functor[F]): F[(List[String], A)] = run(Nil).map(t => (t._1.reverse, t._2))
  }
  object LoggingT {
    def point[F[_]: Applicative, A](a: => A): LoggingT[F, A] = LoggingT(log => Applicative[F].point((log, a)))

    def lift[F[_], A](fa: F[A])(implicit F: Functor[F]): LoggingT[F, A] =
      LoggingT(log => fa.map(a => (log, a)))

    implicit def LoggingTMonad[F[_]: Monad]: Monad[LoggingT[F, ?]] =
      ???

    def log[F[_]: Applicative](line: String): LoggingT[F, Unit] =
      LoggingT(log => Applicative[F].point((line :: log, ())))
  }

  trait Console[F[_]] {
    def putStrLn(s: String): F[Unit]
    def getStrLn: F[String]
  }

  object Console {

    def apply[F[_]](implicit F: Console[F]): Console[F] = F

    implicit val ConsoleIO: Console[IO[Throwable, ?]] = new
        Console[IO[Throwable, ?]] {
      override def putStrLn(s: String): IO[Throwable, Unit] = IO.sync(println(s))

      override def getStrLn: IO[Throwable, String] = IO.sync(scala.io.StdIn.readLine())
    }

    def putStrLn[F[_]: Console](s: String) = Console[F].putStrLn(s)

    def getStrLn[F[_]: Console] = Console[F].getStrLn

  }

  def getName[F[_]: Console: Monad]: F[String] =
    (for {
      _ <- LoggingT.lift[F, Unit](Console.putStrLn("Hello, what is your name?"))
      _ <- LoggingT.log[F]("My log line")
      n <- LoggingT.lift[F, String](Console.getStrLn)
      _ <- LoggingT.lift[F, Unit](Console.putStrLn("Good to meet you, " + n + "!"))
    } yield n).eval.flatMap {
      case (log, name) =>
        val lines = log.mkString("\n")

        Console.putStrLn(s"Logging: \n${lines}") *> (Monad[F].point(name))
    }

  // First-order Abstract Syntax example

  object FCAS {

    /**
      * let i = 0
      * in while (i < 10) { i = i + 1 }
      */
    sealed trait Expr[A]
    case class IntLit(value: Int) extends Expr[Int]
    case class Add(l: Expr[Int], r: Expr[Int]) extends Expr[Int]
    case class Let[A, B](name: String, value: Expr[A], body: Expr[B]) extends Expr[B]
    case class Value[A](name: String) extends Expr[A]
    case class UpdateVariable[A](name: String, value: Expr[A]) extends Expr[A]
    case class LessThan(left: Expr[Int], right: Expr[Int]) extends Expr[Boolean]
    case class While[A](condition: Expr[Boolean], body: Expr[A]) extends Expr[Unit]

    case class IState(value: Map[String, Any]) {
      def addVariable(name: String, v: Any): IState =
        copy(value = value + (name -> v))

      def removeVariable(name: String): IState =
        copy(value = value - name)
    }

    def interpret[A0](expr: Expr[A0], ref: Ref[IState]): IO[String, A0] = {
      def interpret0[A](expr: Expr[A]): IO[String, A] =
        expr match {
          case IntLit(value)    => IO.now(value)
          case Add(left, right) => interpret0(left).seqWith(interpret0(right))(_ + _)
          case Let(name, value, body) =>
            for {
              v <- interpret0(value)
              _ <- ref.update(_.addVariable(name, v))
              b <- interpret0(body)
              _ <- ref.update(_.removeVariable(name))
            } yield b

          case Value(name) =>
            for {
              s <- ref.get
              v <- IO.fromOption(s.value.get(name)).leftMap(_ => "Unreferenced variable: " + name)
            } yield v.asInstanceOf[A]

          case UpdateVariable(name, value) =>
            for {
              v <- interpret0(value)
              _ <- ref.update(_.addVariable(name, v))
            } yield v

          case LessThan(left, right) =>
            interpret0(left).seqWith(interpret0(right))(_ < _)

          case While(condition, body) =>
            (for {
              b <- interpret0(condition)
              _ <- if (b) interpret0(body) else IO.unit
            } yield b).repeat(Schedule.doWhile[Boolean](identity)).void
        }

      interpret0[A0](expr)
    }

    val program: Expr[Unit] =
      Let("i", IntLit(0),
        While(LessThan(Value("isdfsdf"), IntLit(10)),
          UpdateVariable("sfdsdfi", Add(Value("isdfsdf"), IntLit(1)))
        )
      )
  }

  // Higher-Order Abstract Syntax example

  object HOAS {

    /**
      * let i = 0
      * in while (i < 10) { i = i + 1 }
      */
    trait Expr[F[_]] {
      def intLit(value: Int): F[Int]
      def add(l: F[Int], r: F[Int]): F[Int]
      // Uses scala lambdas in input type
      def let[A, B](name: Symbol, value: F[A], body: F[A] => F[B]): F[B]
      def updateVar[A](name: Symbol, value: F[A]): F[A]
      def lessThan(l: F[Int], Right: F[Int]): F[Boolean]
      def while0[A](condition: F[Boolean], body: F[A]): F[Unit]
    }
    object Expr {
      def apply[F[_]](implicit F: Expr[F]): Expr[F] = F
    }

    implicit class IntExprSyntax[F[_]](left: F[Int]) {
      def + (right: F[Int])(implicit F: Expr[F]): F[Int] = F.add(left, right)
      def < (right: F[Int])(implicit F: Expr[F]): F[Boolean] = F.lessThan(left, right)
    }
    def int[F[_]: Expr](i: Int): F[Int] = Expr[F].intLit(i)
    def let[F[_]: Expr, A, B](name: Symbol, value: F[A])(body: F[A] => F[B]): F[B] =
      Expr[F].let(name, value, body)
    def while0[F[_]: Expr, A](condition: F[Boolean])(body: F[A]): F[Unit] =
      Expr[F].while0(condition, body)

    case class IState(value: Map[Symbol, Any]) {
      def addVariable(name: Symbol, v: Any): IState =
        copy(value = value + (name -> v))

      def removeVariable(name: Symbol): IState =
        copy(value = value - name)
    }

    // Look how in this program, 'i is defined in let, but than becomes a real
    // scala variable in the places where you want to use it.
    //
    // Variables in the DSL, have now become variables in scala as well. This
    // doesn't hold for the updateVar yet. This can be worked around, but if
    def program[F[_]: Expr]: F[Unit] =
      let('i, int(0))(i =>
        while0(i < int(10))(
          Expr[F].updateVar('i, i + int(1))
        )
      )

  }

  object SelectableFunctors {

    sealed trait Parser[+E, +A] {
      self =>

      def || [B](that: Parser[E1 >: E, B]): Parser[E1, Either[A, B]] = Alternative(self, that)

      def * : Parser[E, List[A]] = Repeat(self)

      def ~ [B](that: Parser[E1 >: E, B]): Parser[E1, (A, B)] = Zip(self, that)

      def <~ [B](that: Parser[E1 >: E, B]): Parser[E1, (A, B)] = (self ~ that).map(_._1)

      def ~> [B](that: Parser[E1 >: E, B]): Parser[E1, (A, B)] = (self ~ that).map(_._2)
    }
    object Parser {
      def fail[E](e: E): Parser[E, Nothing] = Fail(e)

      def char[E](e: E): Parser[E, Char] = Character(e)
    }
    case class Fail[E](error: E) extends Parser[E, Nothing]
    case class Succeed[A](value: A) extends Parser[Nothing, A]
    case class Character[E](error: E) extends Parser[E, Char]
    case class Repeat[E, A](value: Parser[E, A]) extends Parser[E, List[A]]
    case class Alternative[E, A, B](left: Parser[E, A], right: Parser[E, B]) extends Parser[E, Either[A, B]]
    case class Literal[E](error: E, value: String) extends Parser[E, String]
    case class Zip[E,A,B](left: Parser[E, A], right: Parser[E, B]) extends Parser[E, (A, B)]
    case class Map[E, A0, A](value: Parser[E, A0], f: A0 => A) extends Parser[E, A]

    implicit def ApplicativeParser[E] = Applicative[Parser[E, ?]] = new Applicative[Parser[E, ?]] {
      def point[A](a: => A): Parser[E, A] = Succeed(a)
      def ap[A, B](fa: => Parser[E,A])(f: => Parser[E, A => B]): Parser[E, B] =
        Map[E, (A => B, A), B](Zip(f, fa), t => t._1(t._2))
    }

  }

  object ReaderExample {
    trait MonadReader[R, F[_]] {
      def read: F[R]
    }

    trait HasEnv1[R] {
      def env1: Lens[R, LowDslEnv1]
    }
    case class LowDslEnv1()
    def myLowDsl1[R: HasEnv1, F[_]: MonadReader[R, ?]]: F[Unit] = ???

    trait HasEnv2[R] {
      def env2: Lens[R, LowDslEnv2]
    }
    case class LowDslEnv2()
    def myLowDsl2[R: HasEnv2, F[_]: MonadReader[R, ?]]: F[Unit] = ???

    case class GlobalEnvironment(env1: LowDslEnv1, env2: LowDslEnv2)
    object GlobalEnvironment {
      implicit val GlobalHasEnv1: HasEnv1[GlobalEnvironment] = ???
      implicit val GlobalHasEnv2: HasEnv2[GlobalEnvironment] = ???
    }
    def myProgram[F[_]: HasEnv1: HasEnv2]: F[Unit] = ???

  }



}
