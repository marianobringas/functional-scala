# Guidelines to develop typeclasses

1. Each typeclass represents some business subdomain, eg: account credits/debits
2. All methods in the typeclass relate to each other through algebraic laws (encoded in scalacheck properties) or operational laws (encoded using unit tests)
   i.e. algebraic law:
     transaction(debit(account, amount) |+| credit(account, amount)) === balance(account)
3. Delete all methods that don't relate to others through algebraic or operational laws
4. All operations should be able to be expressed in terms of small set of orthogonal operations
5. It's a good sign if you can express one typeclass in terms of another lower-level one
 
# Example

```$scala
trait NameGetter[F[_]] {
 //Here I do not leak implementation details of needing F[_]: Console: Logging: Monad
  def getName: F[String]
}
class DefaultNameGetter[F[_]: Console: Logging: Monad]() extends NameGetter[F] {

  override def getName: F[String] = {
    for {
      _ <- Console.putStrLn("Hello, what is your name?")
      _ <- Logging[F].log("My log line")
      n <- Console.getStrLn
      _ <- Console.putStrLn("Good to meet you, " + n + "!")
    } yield n
  }
}
```

Refactored to avoid using extends/override

```$scala
def myGetName[F[_]: Console: Logging: Monad]: F[String] = 
  for {
    _ <- Console.putStrLn("Hello, what is your name?")
    _ <- Logging[F].log("My log line")
    n <- Console.getStrLn
    _ <- Console.putStrLn("Good to meet you, " + n + "!")
  } yield n


// As getName is a program, myThirdPartyCode can compose the call to getName three times
def myThirdPartyCode[F[_]: Monad](getName: F[String]): F[Unit] = 
  getName *> getName *> getName
  
myThirdPartyCode(myGetName)
```

Here you change the relationship from extension to composition via parameter passing and the requirement encoded in the dependency type

# Sample with Effect Stacks

```$scala
Future[Either[E, Geocode]]

Future[Either[E, Option[Geocode]]]

Future[List[Either[E, Geocode]]]

Either[Error, Future[Option[A]]]

type Effect[E, A] = Future[Either[E, Option[A]]]

def geoAPI(url: String): Future[Either[E, Option[Geocode]]] = ???

def cacheAPI(key: Array[Byte]): Future[Either[E, Option[Value]]] = ???

def queryDatabase(query: String): Future[Either[E, Option[Response]]] = ???
```
Lots of overhead to pay as you would need to create a lot of classes

1. First step to solve it is to create a type alias

```$scala

type Effect[E, A] = Future[Either[E, Option[A]]]

def geoAPI(url: String): Effect[E, Geocode] = ???

def cacheAPI(key: Array[Byte]): Effect[E, Value] = ???

def queryDatabase(query: String): Effect[E, Response] = ???
```

2. Then, turn it into a Transformer to navigate the stack

```$scala
case class Effect[E, A](run: Future[Either[E, Option[A]]]) {
  def map[B](f: A => B): Effect[E, B] = Effect(run.map(_.map(f)))
  def flatMap[B](f: A => Effect[E, B]): Effect[E, B] = 
    Effect(run.flatMap {
      case Left(e) => Future.successful(Left(e))
      case Right(None) => Future.successful(Right(None))
      case Right(Some(a)) => f(a).run
    })
}
object Effect {
  def point[E, A](a: => A): Effect[E, A] = Effect(Future.successful(Right(Some(a))))
}

def geoAPI(url: String): Effect[E, Geocode] = ???

def cacheAPI(key: Array[Byte]): Effect[E, Value] = ???

def queryDatabase(query: String): Effect[E, Response] = ???
```

```$scala
// type Effect0[E, A] = Future[Either[E, Option[A]]]

trait Effect[F[_, _]] {
  def monad[E]: Monad[F[E, ?]]
  
  def fail[E, A](e: E): F[E, A]
  
  def attempt[E, A](fea: F[E, A]): F[Nothing, Either[E, A]]
  
  def none[E, A]: F[E, A]
  
  def some[E, A](a: A): F[E, A]
  
  def fromFuture[E, A](f: Future[A]): F[E, A]
}
object Effect {
  type MyIO[E, A] = IO[Option[E], A]
  
  implicit val EffectMyIO: Effect[MyIO] = new Effect[MyIO] {
    def monad[E]: Monad[MyIO[E, ?]] = 
    	new Monad[MyIO[E, ?]] {
        def point[A](a: => A): MyIO[E, A] = IO.point(a)
        def bind[A, B](fa: MyIO[E, A])(f: A => MyIO[E, B]): MyIO[E, B] = fa.flatMap(f)
      }
  
    def fail[E, A](e: E): MyIO[E, A] = IO.fail(Some(e))

    def attempt[E, A](fea: MyIO[E, A]): MyIO[Nothing, Either[E, A]] = 
    	fea.attempt.flatMap {
        case Left(None) 		=> IO.fail[Option[E]](None)
        case Left(Some(e)) 	=> IO.now(Left(e))
        case Right(a) 			=> IO.now(Right(a)) 
      }

    def none[E, A]: MyIO[E, A] = IO.fail[Option[E]](None)

    def some[E, A](a: A): MyIO[E, A] = IO.now(a)

    def fromFuture[E, A](f: Future[A]): MyIO[E, A] = ???
  }
}

def geoAPI[F[_, _]: Effect](url: String): F[E, Geocode] = ???

def cacheAPI[F[_, _]: Effect](key: Array[Byte]): F[E, Value] = ???

def queryDatabase[F[_, _]: Effect](query: String): F[E, Response] = ???
```

# Comments

MonadT = Product of effects whereas Free Monad is a Sum of effects
when you want to flatMap over it, you go through 1 stack of monad in free
and all of them in MonadT

Optimization of stacks:

Either[E, Option[A]] == SumType of E + Option[A] == SumType of E + (Unit + A)

SumTypes are associative, so

SumType of E + (Unit + A) == SumType of (E + Unit) + A == Either[Option[E], A]

# Selectable Functors



