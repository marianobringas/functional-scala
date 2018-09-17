package net.degoes

//import scala.concurrent.duration._

import scalaz.zio._
import scalaz.zio.console._

import scalaz._
import Scalaz._

object FunctionalScala extends App {
  final case class URL private (url: String) {
    final def relative(page: String): Option[URL] = URL(url + "/" + page)
  }
  object URL {
    def apply(url: String): Option[URL] =
      scala.util.Try(new java.net.URI(url).parseServerAuthority()).toOption match {
        case None => None
        case Some(_) => Some(new URL(url))
      }
  }

  trait HttpClient[F[_, _]] {
    def getURL(url: URL): F[Exception, String]
  }
  object HttpClient {
    def apply[F[_, _]](implicit F: HttpClient[F]): HttpClient[F] = F

    implicit val HttpClientIO: HttpClient[IO] =
      new HttpClient[IO] {
        def getURL(url: URL): IO[Exception, String] =
          IO.syncException(scala.io.Source.fromURL(url.url)(scala.io.Codec.UTF8).mkString).
            retry(DefaultCrawlSchedule)
      }
  }

  def getURL[F[_, _]: HttpClient](url: URL): F[Exception, String] =
    HttpClient[F].getURL(url)

  def extractURLs(root: URL, html: String): List[URL] = {
    val pattern = "href=[\"\']([^\"\']+)[\"\']".r

    scala.util.Try({
      val matches = (for (m <- pattern.findAllMatchIn(html)) yield m.group(1)).toList

      for {
        m   <- matches
        url <- URL(m).toList ++ root.relative(m).toList
      } yield url
    }).getOrElse(Nil)
  }

  final case class Crawl[E, A](error: E, value: A) {
    def leftMap[E2](f: E => E2): Crawl[E2, A] = Crawl(f(error), value)
    def map[A2](f: A => A2): Crawl[E, A2] = Crawl(error, f(value))
  }
  object Crawl {
    implicit def CrawlMonoid[E: Monoid, A: Monoid]: Monoid[Crawl[E, A]] =
      new Monoid[Crawl[E, A]]{
        def zero: Crawl[E, A] = Crawl(mzero[E], mzero[A])
        def append(l: Crawl[E, A], r: => Crawl[E, A]): Crawl[E, A] =
          Crawl(l.error |+| r.error, l.value |+| r.value)
      }
  }

  val DefaultCrawlSchedule: Schedule[Exception, Unit] =
    Schedule.once
  // val DefaultCrawlSchedule: Schedule[Exception, Unit] =
  //    (Schedule.exponential(10.milliseconds).jittered && Schedule.recurs(20)).void

  trait Effect[F[+_, +_]] {
    def monad[E]: Monad[F[E, ?]]

    def fail[E](e: E): F[E, Nothing]

    def redeem[E1, E2, A, B](fa: F[E1, A])(err: E1 => F[E2, B], succ: A => F[E2, B]): F[E2, B]
  }
  object Effect {
    def apply[F[+_, +_]](implicit F: Effect[F]): Effect[F] = F

    implicit val EffectIO: Effect[IO] = new Effect[IO] {
      def monad[E]: Monad[IO[E, ?]] = new Monad[IO[E, ?]] {
        def point[A](a: => A): IO[E, A] = IO.point(a)
        def bind[A, B](fa: IO[E, A])(f: A => IO[E, B]): IO[E, B] = fa.flatMap(f)
      }
      def fail[E](e: E): IO[E, Nothing] = IO.fail(e)
      def redeem[E1, E2, A, B](fa: IO[E1, A])(err: E1 => IO[E2, B], succ: A => IO[E2, B]): IO[E2, B] =
        fa.redeem(err, succ)
    }
  }
  implicit def EffectMonad[F[+_, +_]: Effect, E]: Monad[F[E, ?]] =
    new Monad[F[E, ?]] {
      def point[A](a: => A): F[E, A] = Effect[F].monad.point(a)

      def bind[A, B](fa: F[E, A])(f: A => F[E, B]): F[E, B] =
        Effect[F].monad.bind(fa)(f)
    }
  implicit class EffectSyntax[F[+_, +_], E1, A](fea: F[E1, A]) {
    def redeem[E2, B](err: E1 => F[E2, B], succ: A => F[E2, B])(implicit F: Effect[F]): F[E2, B] =
      F.redeem(fea)(err, succ)
    def redeemPure[B](err: E1 => B, succ: A => B)(implicit F: Effect[F]): F[Nothing, B] =
      redeem[Nothing, B](
        err.andThen(F.monad[Nothing].point[B](_)),
        succ.andThen(F.monad[Nothing].point[B](_)))
  }

  def crawl[F[+_, +_]: HttpClient: Effect, E: Monoid, A: Monoid](
                                                                  seeds     : Set[URL],
                                                                  router    : URL => Set[URL],
                                                                  processor : (URL, String) => F[E, A]): F[Exception, Crawl[E, A]] = {
    def loop(seeds: Set[URL], visited: Set[URL], crawl0: Crawl[E, A]): F[Exception, Crawl[E, A]] =
      (seeds.toList.traverse { url =>
        for {
          html  <- getURL[F](url)
          crawl <- process1(url, html)
          links = extractURLs(url, html).toSet.flatMap(router) diff visited
        } yield (crawl, links)
      }).map(_.foldMap(identity)).flatMap {
        case (crawl1, links) => loop(links, visited ++ seeds, crawl0 |+| crawl1)
      }

    def process1(url: URL, html: String): F[Nothing, Crawl[E, A]] =
      processor(url, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _))

    loop(seeds, Set(), mzero[Crawl[E, A]])
  }

  final case class ProcessorError[E](error: E, url: URL, html: String)
  def crawlE[F[+_, +_]: Effect: HttpClient, E, A: Monoid](
                                                           seeds     : Set[URL],
                                                           router    : URL => Set[URL],
                                                           processor : (URL, String) => F[E, A]): F[Exception, Crawl[List[ProcessorError[E]], A]] =
    crawl(seeds,  router, (url, html) =>
      processor(url, html).redeem(
        e => Effect[F].fail(List(ProcessorError(e, url, html))),
        Effect[F].monad[List[ProcessorError[E]]].point[A](_)))

  def toURL(url: String): IO[Exception, URL] =
    IO.fromOption(URL(url)).leftMap(
      _ => new Exception("Invalid seed URL: " + url))

  def run(args: List[String]): IO[Nothing, ExitStatus] =
    (for {
      seeds <- IO.traverse(args)(toURL).map(_.toSet)
      _     <- putStrLn(s"Seeds: ${seeds.mkString("\n")}")
      router    = (url: URL) => if (url.url.contains("zio")) Set(url) else Set.empty[URL]
      processor = (url: URL, html: String) => putStrLn(s"Traversing ${url.url}: ${html.take(100)}")
      crawl <- crawlE(seeds, router, processor)
      _     <- putStrLn(crawl.error.mkString("\n"))
    } yield ()).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))

  case class CrawlState(content: Map[URL, String], error: URL => Exception)

  case class CrawlTest[+E, +A](run: CrawlState => Either[E, A])

  object CrawlTest {
    implicit val HttpClientCrawlTest: HttpClient[CrawlTest] =
      new HttpClient[CrawlTest] {
        def getURL(url: URL): CrawlTest[Exception, String] =
          CrawlTest(state =>
            state.content.get(url).fold[Either[Exception, String]](Left(state.error(url)))(Right(_))
          )
      }

    implicit val EffectCrawlTest: Effect[CrawlTest] =
      new Effect[CrawlTest] {
        def fail[E](e: E): CrawlTest[E, Nothing] =
          CrawlTest(state => Left(e))

        def monad[E]: Monad[CrawlTest[E, ?]] = new Monad[CrawlTest[E, ?]] {
          def point[A](a: => A): CrawlTest[E,A] = CrawlTest(_ => Right(a))
          def bind[A, B](fa: CrawlTest[E,A])(f: A => CrawlTest[E,B]): CrawlTest[E,B] =
            CrawlTest(state => fa.run(state) match {
              case Left(e) => Left(e)
              case Right(a) => f(a).run(state)
            })
        }

        def redeem[E1, E2, A, B](fa: CrawlTest[E1,A])(
          err: E1 => CrawlTest[E2,B], succ: A => CrawlTest[E2,B]): CrawlTest[E2,B] =
          CrawlTest(state => fa.run(state) match {
            case Left(e1) => err(e1).run(state)
            case Right(a) => succ(a).run(state)
          })
      }
  }

  /**
    * Composing algebras in Tagless Final
    * @tparam F
    */

  trait Read[F[_]] {
    val readLine: F[String]
  }

  trait Write[F[_]] {
    def writeLine(s: String): F[Unit]
  }

  trait Console[F[_]] extends Read[F] with Write[F]

  /**
    * End
    */

  val TestData1: CrawlState =
    CrawlState(
      Map(
        URL("http://scalaz.org").get -> """<a href="index.html">This link</a> <a href="missing-page.html">Bad link</a>""",
        URL("http://scalaz.org/index.html").get -> """<a href="overview.html">This link</a> <a href="index.html">Link to itself</a>""",
        URL("http://scalaz.org/overview.html").get -> """<a href="http://scalaz.org/index.html">Back to home</a>"""),
      url => new Exception("Bad URL: " + url)
    )
  val TestResult1 = crawlE(
    Set(URL("http://scalaz.org").get), Set(_),
    (url: URL, html: String) => CrawlTest(_ => Right(List((url, html))))).run(TestData1)

  //  def run(args: List[String]): IO[Nothing, ExitStatus] =
//    (for {
//      _ <- putStrLn("Hello World!")
//    } yield ()).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))
}
