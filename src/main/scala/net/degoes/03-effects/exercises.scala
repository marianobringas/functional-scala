// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.effects

import java.io.IOException

import net.degoes.effects.zio_background.Program.{ReadLine, Return, WriteLine}
import scalaz.zio._
import scalaz.zio.console._

import scala.concurrent.duration._

object zio_background {

  sealed trait Program[A] { self =>
    final def map[B](f: A => B): Program[B] =
      self match {
        case Program.ReadLine(next) => Program.ReadLine(input => next(input).map(f))
        case Program.WriteLine(line, next) => Program.WriteLine(line, next.map(f))
        case Program.Return(value) => Program.Return(() => f(value()))
      }

    final def flatMap[B](f: A => Program[B]): Program[B] =
      self match {
        case Program.ReadLine(next) => Program.ReadLine(input => next(input).flatMap(f))
        case Program.WriteLine(line, next) => Program.WriteLine(line, next.flatMap(f))
        case Program.Return(value) => f(value())
      }

    final def *> [B](that: Program[B]): Program[B] =
      self.flatMap( _ => that)

    final def <* [B](that: Program[B]): Program[A] =
      self.flatMap(a => that.map(_ => a))
  }

  object Program {
    final case class ReadLine[A](next: String => Program[A]) extends Program[A]
    final case class WriteLine[A](line: String, next: Program[A]) extends Program[A]
    final case class Return[A](value: () => A) extends Program[A]

    def readLine: Program[String] = ReadLine(point)
    def writeLine(line: String): Program[Unit] = WriteLine(line, Return(() => ()))
    def point[A](a: A): Program[A] = Return(() => a)
  }

//  sealed trait Program[A] { self =>
//    import Program.{Chain, Return}
//
//    final def map[B](f: A => B): Program[B] =
//      Chain(self, f.andThen(Return(_)))
//
//    final def flatMap[B](f: A => Program[B]): Program[B] =
//      Chain(self, f)
//  }
//  object Program {
//    final case class ReadLine[A](next: String => Program[A]) extends Program[A]
//    final case class WriteLine[A](line: String, next: Program[A]) extends Program[A]
//    final case class Chain[A0, A](previous: Program[A0], next: A0 => Program[A]) extends Program[A]
//    final case class Return[A](value: A) extends Program[A]
//
//    def readLine: Program[String] = ReadLine(Return(_))
//    def writeLine(line: String): Program[Unit] = WriteLine(line, Return(()))
//    def point[A](a: A): Program[A] = Return(a)
//  }

  import Program.{readLine, writeLine, point}

  val yourName1: Program[Unit] =
    writeLine("What is your name?").flatMap(_ =>
      readLine.flatMap(name =>
        writeLine("Hello, " + name + ", good to meet you!").flatMap(_ =>
          point(())
        )
      )
    )

  //
  // EXERCISE 1
  //
  // Rewrite `program1` to use a for comprehension.
  //
  val yourName2: Program[Unit] = for {
    _    <- writeLine("What is your name?")
    name <- readLine
    _    <- writeLine("Hello, " + name + ", good to meet you!")
  } yield ()

  //
  // EXERCISE 2
  //
  // Rewrite `yourName2` using the helper function `getName`, which shows how
  // to create larger programs from smaller programs.
  //
  def yourName3: Program[Unit] = for {
    name <- getName
    _    <- writeLine("Hello, " + name + ", good to meet you!")
  } yield ()

  def getName: Program[String] =
    writeLine("What is your name?").flatMap(_ => readLine)

  //
  // EXERCISE 3
  //
  // Implement the following effectful procedure, which interprets
  // `Program[A]` into `A`. You can use this procedure to "run" programs.
  //
  def interpret[A](program: Program[A]): A = program match {
    case WriteLine(line, next) =>
      println(line)
      interpret(next)
    case ReadLine(next) =>
      interpret(next(scala.io.StdIn.readLine()))
    case Return(value) =>
      value()
  }

  //
  // EXERCISE 4
  //
  // Implement the following function, which shows how to write a combinator
  // that operates on programs.
  //
  def sequence[A](programs: List[Program[A]]): Program[List[A]] = programs match {
    case Nil => Program.point(Nil)
    case head :: tail =>
      for {
        a  <- head
        as <- sequence(tail)
      } yield a :: as
  }

  //
  // EXERCISE 5
  //
  // Translate the following procedural program into a purely functional program
  // using `Program` and a for comprehension.
  //
  def ageExplainer1(): Unit = {
    println("What is your age?")
    scala.util.Try(scala.io.StdIn.readLine().toInt).toOption match {
      case Some(age) =>
        if (age < 12) println("You are a kid")
        else if (age < 20) println("You are a teenager")
        else if (age < 30) println("You are a grownup")
        else if (age < 50) println("You are an adult")
        else if (age < 80) println("You are a mature adult")
        else if (age < 100) println("You are elderly")
        else println("You are probably lying.")
      case None =>
        println("That's not an age, try again")

        ageExplainer1()
    }

  }

  def ageExplainer2: Program[Int] = for {
    _     <- writeLine("What is your age?")
    input <- readLine
    age   <- scala.util.Try(input.toInt).toOption match {
      case Some(age) =>
        (if (age < 12) writeLine("You are a kid")
         else if (age < 20) writeLine("You are a teenager")
         else if (age < 30) writeLine("You are a grownup")
         else if (age < 50) writeLine("You are an adult")
         else if (age < 80) writeLine("You are a mature adult")
         else if (age < 100) writeLine("You are elderly")
         else writeLine("You are probably lying.")) *> point(age)
      case None =>
        writeLine("That's not an age, try again") *> ageExplainer2
    }
  } yield age
}

object zio_type {
  type ??? = Nothing

  //
  // EXERCISE 1
  //
  // Write the type of `IO` values that can fail with an `Exception`, or
  // may produce an `A`.
  //
  type Exceptional[A] = IO[Exception, A]

  //
  // EXERCISE 2
  //
  // Write the type of `IO` values that can fail with a `Throwable`, or
  // may produce an `A`.
  //
  type Task[A] = IO[Throwable, A]

  //
  // EXERCISE 3
  //
  // Write the type of `IO` values that cannot fail, but may produce an `A.`
  //
  type Infallible[A] = IO[Nothing, A]

  //
  // EXERCISE 4
  //
  // Write the type of `IO` values that cannot produce a value, but may fail
  // with an `E`.
  //
  // Looping forever without producing a value having effects that might fail. i.e. Webserver
  //
  type Unproductive[E] = IO[E, Nothing]

  //
  // EXERCISE 5
  //
  // Write the type of `IO` values that cannot fail or produce a value.
  //
  type Unending = IO[Nothing, Nothing]
}

object zio_values {
  //
  // EXERCISE 1
  //
  // Using the `IO.now` method, lift the integer `2` into a strictly-evaluated
  // `IO`.
  //
  val ioInteger: IO[Nothing, Int] = IO.now(2)

  //
  // EXERCISE 2
  //
  // Using the `IO.point` method, lift the string "Functional Scala" into a
  // lazily-evaluated `IO`.
  //
  val ioString: IO[Nothing, String] = IO.point("Functional Scala")

  //
  // EXERCISE 3
  //
  // Using the `IO.fail` method to lift the string "Bad Input" into a failed
  // `IO`.
  //
  val failedInput: IO[String, Nothing] = IO.fail("Bad Input")
}

object zio_composition {
  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }
  //
  // EXERCISE 1
  //
  // Map the `IO[Nothing, Int]` into an `IO[Nothing, String]` by converting the
  // integer into its string rendering using the `map` method of the `IO`
  // object.
  //
  (IO.point(42).map(_.toString) : IO[Nothing, String])

  //
  // EXERCISE 2
  //
  // Map the `IO[Int, Nothing]` into an `IO[String, Nothing]` by converting the
  // integer error into its string rendering using the `leftMap` method of the
  // `IO` object.
  //
  (IO.fail(42).leftMap(_.toString) : IO[String, Nothing])

  //
  // EXERCISE 3
  //
  // Using the `flatMap` and `map` methods of `IO`, add `ioX` and `ioY`
  // together.
  //
  val ioX: IO[Nothing, Int] = IO.point(42)
  val ioY: IO[Nothing, Int] = IO.point(58)
  val ioXPlusY: IO[Nothing, Int] = ioX.flatMap(a => ioY.map( b => a + b))
  val ioXPlusY2: IO[Nothing, Int] =
    for {
      x <- ioX
      y <- ioY
    } yield (x + y)

  //
  // EXERCISE 4
  //
  // Using the `flatMap` method of `IO`, implement `ifThenElse`.
  //
  def ifThenElse[E, A](bool: IO[E, Boolean])(
    ifTrue: IO[E, A], ifFalse: IO[E, A]): IO[E, A] = bool.flatMap( b => if(b) ifTrue else ifFalse)

  def ifThenElse2[E, A](bool: IO[E, Boolean])(
    ifTrue: IO[E, A], ifFalse: IO[E, A]): IO[E, A] =
    for {
      cond   <- bool
      result <- if(cond) ifTrue else ifFalse
    } yield result

  val exampleIf = ifThenElse(IO.point(true))(IO.point("It's true!"), IO.point("It's false!"))

  //
  // EXERCISE 5
  //
  // Translate the following program, which uses for-comprehensions, to its
  // equivalent chain of `flatMap`'s, followed by a final `map`.
  //
  for {
    v1 <- IO.point(42)
    v2 <- IO.point(58)
  } yield "The total is: " + (v1 + v2).toString

  IO.point(42).flatMap( v1 => IO.point(58).map( v2 => "The total is: " + (v1 + v2).toString))

  //
  // EXERCISE 6
  //
  // Rewrite the following ZIO program, which uses conditionals, into its
  // procedural equivalent.
  //
  def decode1(read: () => Byte): Either[Byte, Int] = {
    val b = read()
    if (b < 0) Left(b)
    else {
      Right(b.toInt +
      (read().toInt << 8) +
      (read().toInt << 8) +
      (read().toInt << 8))
    }
  }
  def decode2[E](read: IO[E, Byte]): IO[E, Either[Byte, Int]] =
    for {
      b <- read
      e <- if (b < 0)
             IO.now(Left(b))
           else for
                {
                  b2 <- read
                  b3 <- read
                  b4 <- read

                } yield Right(b.toInt +
                                     (b2.toInt << 8) +
                                     (b3.toInt << 8) +
                                     (b4.toInt << 8))
    } yield e

  //
  // EXERCISE 7
  //
  // Rewrite the following procedural program, which uses conditionals, into its
  // ZIO equivalent.
  //
  def getName1(print: String => Unit, read: () => String): Option[String] = {
    print("Do you want to enter your name?")
    read().toLowerCase.take(1) match {
      case "y" => Some(read())
      case _ => None
    }
  }
  def getName2[E](print: String => IO[E, String], read: IO[E, String]): IO[E, Option[String]] = ???

  //
  // EXERCISE 8
  //
  // Translate the following loop into its ZIO equivalent.
  //
  def sumList1(ints: List[Int], acc: Int): Int = ints match {
    case Nil => acc
    case x :: xs => sumList1(xs, acc + x)
  }
  def sumList2(ints: IO[Nothing, List[Int]], acc: IO[Nothing, Int]): IO[Nothing, Int] = ???

  //
  // EXERCISE 9
  //
  // Translate the following loop into its ZIO equivalent.
  //
  def repeatN1(n: Int, action: () => Unit): Unit =
    if (n <= 0) ()
    else {
      action()
      repeatN1(n - 1, action)
    }
  def repeatN2(n: Int, action: IO[Nothing, Unit]): IO[Nothing, Unit] =
    if( n <= 0)
      IO.unit
    else action *> repeatN2(n - 1, action)

  //
  // EXERCISE 10
  //
  // Translate the following expression into its `flatMap` equivalent.
  //
  IO.point(42) *> IO.point(19)
  IO.point(42).flatMap( _ => IO.point(19))

  //
  // EXERCISE 11
  //
  // Translate the following expression into its `flatMap` equivalent.
  //
  IO.point(42) <* IO.point(19)
  IO.point(42).flatMap(a => IO.point(19).map(_ => a))

  //
  // EXERCISE 12
  //
  // Translate the following expression into an equivalent expression using
  // the `map` and `flatMap` methods of the `IO` object.
  //
  (IO.point(42) <* IO.point(19)) *> IO.point(1)
  IO.point(42).flatMap(a => IO.point(19).map(_ => a)).flatMap(_ => IO.point(1))
}

object zio_failure {
  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }

  //
  // EXERCISE 1
  //
  // Using the `IO.fail` method, create an `IO[String, Int]` value that
  // represents a failure with a string error message, containing a user-
  // readable description of the failure.
  //
  val stringFailure1: IO[String, Int] = IO.fail("The program did not compile")

  //
  // EXERCISE 2
  //
  // Using the `IO.fail` method, create an `IO[Int, String]` value that
  // represents a failure with an integer error code.
  //
  val intFailure: IO[Int, String] = IO.fail(42)

  //
  // EXERCISE 3
  //
  // Transform the error of `intFailure` into its string representation using
  // the `leftMap` method of `IO`.
  //
  val stringFailure2: IO[String, String] = intFailure.leftMap(_.toString)

  //
  // EXERCISE 4
  //
  // Translate the following exception-throwing program into its ZIO equivalent.
  //
  def accessArr1[A](i: Int, a: Array[A]): A =
    if (i < 0 || i >= a.length) throw new IndexOutOfBoundsException("The index " + i + " is out of bounds [0, " + a.length + ")")
    else a(i)

  def accessArr2[A](i: Int, a: Array[A]): IO[IndexOutOfBoundsException, A] =
    if (i < 0 || i >= a.length)
      IO.fail(new IndexOutOfBoundsException("The index " + i + " is out of bounds [0, " + a.length + ")"))
    else
      IO.now(a(i))

  //
  // EXERCISE 5
  //
  // Translate the following ZIO program into its exception-throwing equivalent.
  //
  trait DenomIsZero
  object DenomIsZero extends DenomIsZero {}
  def divide1(n: Int, d: Int): IO[DenomIsZero, Int] =
    if (d == 0) IO.fail(DenomIsZero)
    else IO.now(n / d)

  def divide2(n: Int, d: Int): Int = n / d

  //
  // EXERCISE 6
  //
  // Recover from a division by zero error by returning `-1`.
  //
  val recovered1: IO[Nothing, Int] = divide1(100, 0).attempt.map {
    case Left(error) => -1
    case Right(value) => value
  }

  //
  // EXERCISE 7
  //
  // Recover from a division by zero error by using `redeem`.
  //
  val recovered2: IO[Nothing, Int] = divide1(100, 0).redeem(_ => IO.now(-1), IO.now(_))

  //
  // EXERCISE 8
  //
  // Use the `orElse` method of `IO` to try `firstChoice`, and fallback to
  // `secondChoice` only if `firstChoice` fails.
  //
  val firstChoice: IO[DenomIsZero, Int] = divide1(100, 0)
  val secondChoice: IO[Nothing, Int] = IO.now(400)
  val combined: IO[Nothing, Int] = firstChoice orElse secondChoice
}

object zio_effects {
  import scala.io.StdIn.readLine
  import scala.io.Source
  import java.io.File
  import java.util.concurrent.{Executors, TimeUnit}

  type ??? = Nothing
  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }

  // IO.sync is for effectful computation that cannot fail
  // IO.syncException for effectful computation that can throw an Exception
  // IO.syncThrowable for effectful computation that can throw a Throwable
  // IO.syncCatch for effectful computation that can throw Throwables, but can receive a partial function to deal with exceptions and transforming them
  //   IO.syncCatch(Source.fromFile(file).getLines.toList) {
  //      case e: IOException => e
  //   }
  //

  //
  // EXERCISE 1
  //
  // Using the `IO.sync` method, wrap Scala's `println` method to import it into
  // the world of pure functional programming.
  //
  def putStrLn(line: String): IO[Nothing, Unit] = IO.sync(println(line))

  //
  // EXERCISE 2
  //
  // Using the `IO.sync` method, wrap Scala's `readLine` method to import it
  // into the world of pure functional programming.
  //
  val getStrLn: IO[Nothing, String] = IO.sync(readLine)

  //
  // EXERCISE 3
  //
  // Using the `IO.syncException` method, wrap Scala's `getLines` method to
  // import it into the world of pure functional programming.
  //
  def readFile(file: File): IO[Exception, List[String]] =
    IO.syncException(Source.fromFile(file).getLines.toList)

  // This one catches all Throwables instead of just Exceptions
  def readFile2(file: File): IO[Throwable, List[String]] =
    IO.syncThrowable(Source.fromFile(file).getLines.toList)

  //
  // EXERCISE 4
  //
  // Identify the correct method and error type to import `System.nanoTime`
  // safely into the world of pure functional programming.
  //
  def nanoTime: IO[Nothing, Long] = IO.sync(System.nanoTime())

  //
  // EXERCISE 5
  //
  // Identify the correct method, error, and value type to import `System.exit`
  // safely into the world of pure functional programming.
  //
  def sysExit(code: Int): IO[SecurityException, Unit] = IO.syncCatch(System.exit(code)) {
    case e: SecurityException => e
  }

  def sysExit2(code: Int): IO[Throwable, Unit] = IO.syncThrowable(System.exit(code))

  //
  // EXERCISE 6
  //
  // Identify the correct method, error, and value type to import
  // `Array.update` safely into the world of pure functional programming.
  //
  def arrayUpdate[A](a: Array[A], i: Int, f: A => A): IO[ArrayIndexOutOfBoundsException, Unit] =
    IO.syncCatch(a.update(i, f(a(i)))) {
      case e: ArrayIndexOutOfBoundsException => e
    }

  //
  // EXERCISE 7
  //
  // Use the `IO.async` method to implement the following `sleep` method, and
  // choose the correct error type.
  //
  val scheduledExecutor = Executors.newScheduledThreadPool(1)
  def sleep(l: Long, u: TimeUnit): IO[Nothing, Unit] =
    IO.async[Nothing, Unit](k => scheduledExecutor.schedule(new Runnable {
      def run(): Unit = k(ExitResult.Completed(()))
    }, l, u))

  //
  // EXERCISE 8
  //
  // Translate the following procedural program into ZIO.
  //
  def playGame1(): Unit = {
    val number = scala.util.Random.nextInt(5)
    println("Enter a number between 0 - 5: ")
    scala.util.Try(scala.io.StdIn.readLine().toInt).toOption match {
      case None =>
        println("You didn't enter an integer!")
        playGame1
      case Some(guess) if (guess == number) =>
        println("You guessed right! The number was " + number)
      case _ =>
        println("You guessed wrong! The number was " + number)
    }
  }

  def playGame2: IO[Exception, Unit] =
    for {
      n     <- IO.sync(scala.util.Random.nextInt(5))
      _     <- putStrLn("Enter a number between 0 - 5: ")
      input <- getStrLn
      _     <- IO.syncCatch(input.toInt) { case e => Unit }
                                                           .redeem(
          _ => putStrLn("You didn't enter an integer!") *> playGame2,
          guess =>
            if (guess == n) putStrLn("You guessed right! The number was " + n)
            else putStrLn("You guessed wrong! The number was " + n)
        )
    } yield ()

  /*
  def pureNextInt(i: Int): IO[Nothing, Int]      = IO.sync(scala.util.Random.nextInt(i))
  def purePrint(s: => String): IO[Nothing, Unit] = IO.sync(println(s))
  val pureReadInt: IO[Nothing, Option[Int]]      = IO.sync(Try(scala.io.StdIn.readLine().toInt).toOption)

  def playGame2: IO[Exception, Unit] =
    pureNextInt(5).flatMap { number =>
      purePrint("Enter a number between 0 - 5: ") *>
        pureReadInt.flatMap {
          case None                         => purePrint("You didn't enter an integer!") *> playGame2
          case Some(gess) if gess == number => purePrint("You guessed right! The number was " + number)
          case _                            => purePrint("You guessed wrong! The number was " + number)
        }
    }

  def playGame3: IO[Exception, Unit] =
    for {
      number <- pureNextInt(5)
      _      <- purePrint("Enter a number between 0 - 5: ")
      line   <- getStrLn
      _ <- IO
        .syncException(line.toInt)
        .redeem(
          _ => purePrint("You didn't enter an integer!") *> playGame2,
          guess =>
            if (guess == number) purePrint("You guessed right! The number was " + number)
            else purePrint("You guessed wrong! The number was " + number)
        )
    } yield ()

    def nextInt(upper: Int): IO[Nothing, Int] =
    IO.sync(scala.util.Random.nextInt(5))

  def checkGuess(actual: Int, guess: Int): IO[Nothing, Unit] =
    if(guess == actual)
      putStrLn(" You guessed right! The number was ")
    else
      putStrLn("You guess wrong")

  def handleNotAnInt: IO[Nothing, Unit] =
    putStrLn("You didn't enter an integer") *> playGame2

  def getInput: IO[Nothing, String] =
    putStrLn("Enter a number between 0 - 5: ") *> getStrLn

  def checkInput(number: Int, input: String): IO[Nothing, Unit] =
    IO
      .syncException(input.toInt)
      .redeem(_ => handleNotAnInt, checkGuess(number, _))

  def playGame2: IO[Nothing, Unit] = for {
    number <- nextInt(5)
    input  <- getInput
    _      <- checkInput(number, input)
  } yield ()

   */

}

object zio_concurrency {
  type ??? = Nothing

  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }

  //
  // EXERCISE 1
  //
  // Race `leftContestent1` and `rightContestent1` using the `race` method of
  // `IO` to see which one finishes first with a successful value.
  //
  val leftContestent1 = IO.never
  val rightContestent1 = putStrLn("Hello World")
  val raced1 = leftContestent1 race rightContestent1
  val raced2 = (leftContestent1.attempt) race (rightContestent1.attempt)

  //
  // EXERCISE 2
  //
  // Race `leftContestent2` and `rightContestent2` using the `race` method of
  // `IO` to see which one finishes first with a successful value.
  //
  val leftContestent2: IO[IOException, Nothing] = IO.fail(new IOException("Uh oh!"))
  val rightContestent2: IO[Exception, Unit] = IO.sleep(10.milliseconds) *> putStrLn("Hello World")
  val raced3: IO[Nothing, Either[Either[IOException, Nothing], Either[Exception,Unit]]] = (leftContestent2.attempt) raceBoth (rightContestent2.attempt)

  //
  // EXERCISE 3
  //
  // Compute `leftWork1` and `rightWork1` in parallel using the `par` method of
  // `IO`.
  //
  val leftWork1: IO[Nothing, Int] = fibonacci(10)
  val rightWork1: IO[Nothing, Int] = fibonacci(10)
  val par1: IO[Nothing, (Int, Int)] = leftWork1 par rightWork1

  //
  // EXERCISE 4
  //
  // Compute all values `workers` in parallel using `IO.parAll`.
  //
  val workers: List[IO[Nothing, Int]] = (1 to 10).toList.map(fibonacci(_))
  val workersInParallel: IO[Nothing, List[Int]] = IO.parAll(workers)

  //
  // EXERCISE 5
  //
  // Implement `myPar` by forking `left` and `right`, and then joining them
  // and yielding a tuple of their results.
  //
  def myPar[E, A, B](left: IO[E, A], right: IO[E, B]): IO[E, (A, B)] =
    for {
      fiberA <- left.fork
      fiberB <- right.fork
      a      <- fiberA.join
      b      <- fiberB.join
    } yield (a, b)

  //
  // EXERCISE 6
  //
  // Use the `IO.supervise` method to ensure that when the main fiber exits,
  // all fibers forked within it will be terminated cleanly.
  //
  val supervisedExample: IO[Nothing, Unit] =
  IO.supervise {
    for {
      fiber <- fibonacci(10000).fork
    } yield ()
  }

  val supervisedExample2: IO[Nothing, Unit] =
    (for {
        fiber <- fibonacci(10000).fork
      } yield ()).supervised

  val fiberWithErrorHandler = fibonacci(19999).fork0(error => putStr(error.toString).attempt.void)

  //
  // EXERCISE 7
  //
  // Use the `interrupt` method of the `Fiber` object to cancel the long-running
  // `fiber`.
  //
  val interrupted1: IO[Nothing, Unit] =
    for {
      fiber <- fibonacci(10000).fork
      _     <- fiber.interrupt
    } yield ()

  //
  // EXERCISE 8
  //
  // Use the `zipWith` method of the `Fiber` object to combine `fiber1` and
  // `fiber2` into a single fiber (by summing the results), so they can be
  // interrupted together.
  //
  val interrupted2: IO[Nothing, Unit] =
    for {
      fiber1 <- fibonacci(10).fork
      fiber2 <- fibonacci(20).fork
      both   <- IO.point(fiber1.zipWith(fiber2)(_ + _))
      _      <- both.interrupt
    } yield ()

  // Same thing as above without the IO.point
  val interrupted3: IO[Nothing, Unit] =
    for {
      fiber1 <- fibonacci(10).fork
      fiber2 <- fibonacci(20).fork
      both    = fiber1.zipWith(fiber2)(_ + _)
      _      <- both.interrupt
    } yield ()


  def fibonacci(n: Int): IO[Nothing, Int] =
    if (n <= 1) IO.now(n)
    else fibonacci(n - 1).seqWith(fibonacci(n - 2))(_ + _)
}

object zio_resources {
  import java.io.{File, FileInputStream}
  class InputStream private (is: FileInputStream) {
    def read: IO[Exception, Option[Byte]] =
      IO.syncException(is.read).map(i => if (i < 0) None else Some(i.toByte))
    def close: IO[Exception, Unit] =
      IO.syncException(is.close())
  }
  object InputStream {
    def openFile(file: File): IO[Exception, InputStream] =
      IO.syncException(new InputStream(new FileInputStream(file)))
  }

  //
  // EXERCISE 1
  //
  // Rewrite the following procedural program to ZIO, using `IO.fail` and the
  // `bracket` method of the `IO` object.
  //
  // IO.bracket:
  //   - acquire, which produces an A (may fail, uninterruptible)
  //   - release, which releases the A (may not fail, uninterruptible)
  //   - use, which uses the A (may fail, interruptible)
  //
  // acquire.bracket(release(_)) { resource => for { ... } }
  def tryCatch1(): Unit =
    try throw new Exception("Uh oh")
    finally println("On the way out...")
  val tryCatch2: IO[Exception, Unit] =
    IO.fail(new Exception("Uh oh")).
      bracket(_ => putStrLn("On the way out...").attempt.void)(IO.now)

  //
  // EXERCISE 2
  //
  // Rewrite the `readFile1` function to use `bracket` so resources can be
  // safely cleaned up in the event of errors, defects, or interruption.
  //
  def readFile1(file: File): IO[Exception, List[Byte]] = {
    def readAll(is: InputStream, acc: List[Byte]): IO[Exception, List[Byte]] =
      is.read.flatMap {
        case None => IO.now(acc.reverse)
        case Some(byte) => readAll(is, byte :: acc)
      }

    for {
      stream <- InputStream.openFile(file)
      bytes  <- readAll(stream, Nil)
    } yield bytes
  }

  def readFile2(file: File): IO[Exception, List[Byte]] = {
    def readAll(is: InputStream, acc: List[Byte]): IO[Exception, List[Byte]] =
      is.read.flatMap {
        case None => IO.now(acc.reverse)
        case Some(byte) => readAll(is, byte :: acc)
      }

    InputStream.openFile(file).bracket( _.close.attempt.void)(readAll(_, Nil))
  }

  //
  // EXERCISE 3
  //
  // Implement the `tryCatchFinally` method using `bracket`.
  //
//  def tryCatchFinally[E, A]
//    (try0: IO[E, A])
//    (catch0: PartialFunction[E, IO[E, A]])
//    (finally0: IO[Nothing, Unit]): IO[E, A] =
//      try0.attempt.bracket(a => finally0) {
//        case Left(e) => catch0.applyOrElse(e, IO.fail)
//        case Right(a) => IO.now(a)
//      }

  def tryCatchFinally2[E, A]
  (try0: IO[E, A])
  (catch0: PartialFunction[E, IO[E, A]])
  (finally0: IO[Nothing, Unit]): IO[E, A] =
    try0.catchSome(catch0).ensuring(finally0)

  //
  // EXERCISE 4
  //
  // Use the `tryCatchFinally` method to rewrite the following snippet to ZIO.
  //
  def readFileTCF1(file: File): List[Byte] = {
    var fis : FileInputStream = null

    try {
      fis = new FileInputStream(file)
      val array = Array.ofDim[Byte](file.length.toInt)
      fis.read(array)
      array.toList
    } catch {
      case e : java.io.IOException => Nil
    } finally if (fis != null) fis.close()
  }

  def readFileTCF2(file: File): IO[Exception, List[Byte]] =
    IO.syncException(new FileInputStream(file))
      .bracket(fis ⇒ IO.syncException(fis.close()).attempt.void) {
        fis =>
               for {
                     l     <- IO.syncException(file.length.toInt)
                     array <- IO.syncException(Array.ofDim[Byte](l))
               } yield array.toList
      }
}

object zio_schedule {

  //
  // Every Schedule[A,B] can:
  //
  //  - start with some initial state
  //  - for every step of the schedule:
  //    - update its state
  //    - consume input values of type A
  //    - produce output values of type B
  //    - decides to continue of complete the schedule
  //    - decice to delay the next step of the schedule by some duration
  //
  // Use cases:
  //   - Consume errors:
  //     - Retries (io: IO[E,A]).retry(s: Schedule[E, B])
  //     - Retries with default (io: IO[E,A]).retryOrElse(s: Schedule[E, B], (b: B))
  //   - Consume outputs/values:
  //     - Repeat an action (io: IO[E,A]).repeat(s: Schedule[E, B])
  //     - Repeat an action with error handling(io: IO[E,A]).repeatOrElse(s: Schedule[E, B], (e: E))

  implicit class FixMe[A](a: A) {
    def ? = ???
  }

  //
  // EXERCISE 1
  //
  // Using `Schedule.recurs`, create a schedule that recurs 5 times.
  //
  val fiveTimes: Schedule[Any, Int] = Schedule.recurs(5)

  //
  // EXERCISE 2
  //
  // Using the `repeat` method of the `IO` object, repeat printing "Hello World"
  // five times to the console.
  //
  val repeated1 = putStrLn("Hello World").repeat(fiveTimes)

  //
  // EXERCISE 3
  //
  // Using `Schedule.spaced`, create a schedule that recurs forever every 1
  // second.
  //
  val everySecond: Schedule[Any, Int] = Schedule.spaced(1.second)

  //
  // EXERCISE 4
  //
  // Using the `&&` method of the `Schedule` object, the `fiveTimes` schedule,
  // and the `everySecond` schedule, create a schedule that repeats fives times,
  // every second.
  //
  val fiveTimesEverySecond = fiveTimes && everySecond

  //
  // EXERCISE 5
  //
  // Using the `repeat` method of the `IO` object, repeat the action
  // putStrLn("Hi hi") using `fiveTimesEverySecond`.
  //
  val repeated2 = putStrLn("Hi hi").repeat(fiveTimesEverySecond)

  //
  // EXERCISE 6
  //
  // Using the `andThen` method of the `Schedule` object, the `fiveTimes`
  // schedule, and the `everySecond` schedule, create a schedule that repeats
  // fives times rapidly, and then repeats every second forever.
  //
  val fiveTimesThenEverySecond = fiveTimes andThen everySecond

  //
  // EXERCISE 7
  //
  // Using the `retry` method of the `IO` object, retry the following error
  // a total of five times.
  //
  val error1 = IO.fail("Uh oh!")
  val retried5 = error1 retry fiveTimes

  //
  // EXERCISE 8
  //
  // Produce a jittered schedule that first does exponential (starting from 10 ms) spacing but then
  // after the spacing reaches 60 seconds, switches over to fixed scpacing of
  // 1 min between recurrences, but will only do that for up to 100 times
  // and produce a list of the results
  //

  def exponentialScheduleUpTo1Min[A]: Schedule[A, Duration] = Schedule.exponential(10.milliseconds).whileValue(_ < 60.seconds)
  def scheduleSpaced60SecsApartUpTo100Times[A]: Schedule[A, (Int, Int)] = Schedule.spaced(60.seconds) && Schedule.recurs(100)
  def customScheduleWithValuesAndErrors[A]: Schedule[A, Either[Duration, (Int, Int)]] = exponentialScheduleUpTo1Min andThen scheduleSpaced60SecsApartUpTo100Times
  def extractedValues[A]: Schedule[A, A] = customScheduleWithValuesAndErrors *> Schedule.identity[A]
  def mySchedule[A]: Schedule[A, List[A]] = extractedValues.collect.jittered

}

object zio_interop {
  implicit class FixMe[A](a: A) {
    def ? = ???
  }

  import scala.concurrent.Future
  import scalaz.zio.interop.future._
  import scala.concurrent.ExecutionContext.Implicits.global

  //
  // EXERCISE 1
  //
  // Use `IO.fromFuture` method to convert the following `Future` into an `IO`.
  //
  val future1 = () => Future.successful("Hello World")
  val io1 = IO.fromFuture(???)(global)

  //
  // EXERCISE 2
  //
  // Use the `toFuture` method on `IO` to convert the following `io` to `Future`.
  //
  val io2: IO[Throwable, Int] = IO.point(42)
  val future2: IO[Nothing, Future[Int]] = io2 ?

  //
  // EXERCISE 3
  //
  // Use the Fiber.fromFuture` method to convert the following `Future` into
  // an `IO`.
  //
  val future3 = () => Future.failed[Int](new Error("Uh ohs!"))
  val fiber1: Fiber[Throwable, Int] = Fiber.fromFuture(???)(global)

  import scalaz.zio.interop.Task
  import scalaz.zio.interop.catz._
  import cats.effect.concurrent.Ref

  //
  // EXERCISE 4
  //
  // The following example uses the `Ref` from `cats-effect`, demonstrating how
  // `cats-effect` structures work with ZIO.
  //
  class Worker(number: Int, ref: Ref[Task, Int]) {
    def work: Task[Unit] =
      for {
        c1 <- ref.get
        _  <- putStrLn(s"#$number >> $c1")
        c2 <- ref.modify(x => (x + 1, x))
        _  <- putStrLn(s"#$number >> $c2")
      } yield ()
  }

  val program: Task[Unit] =
    for {
      ref <- Ref.of[Task, Int](0)
      w1  = new Worker(1, ref)
      w2  = new Worker(2, ref)
      w3  = new Worker(3, ref)
      f   <- IO.forkAll(List(w1.work, w2.work, w3.work))
      _   <- f.join
    } yield ()
}

object zio_ref {

}

object zio_promise {

}

object zio_queue {

}

object zio_rts {

}

object zio_challenge {

}

object zio_advanced {

}
