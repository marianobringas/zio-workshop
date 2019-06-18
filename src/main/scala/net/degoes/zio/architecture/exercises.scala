// Copyright(C) 2019 - John A. De Goes. All rights reserved.

package net.degoes.zio.architecture

import java.io.{File, FileOutputStream}

import scala.util._
import scalaz.zio._
import net.degoes.zio._

import scala.compat.Platform
import scalaz.zio.internal.PlatformLive
import scalaz.zio.internal.Executor

import scala.concurrent.ExecutionContext
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.Executors

import scalaz.zio.blocking.Blocking
import java.sql.ResultSet

import net.degoes.zio.architecture.zio_errors.HttpClientError.{ClientError, Redirection, ServerError}
import net.degoes.zio.architecture.zio_errors.UserServiceError.{InvalidUserId, ServiceUnavailable}

object zio_errors {

  /**
   * EXERCISE 1
   *
   * Correct what is wrong with the return type of the following method.
    *
    * [MB]
    * ParseInt can only fail if it's not provided with a valid number. Option is the best way of failing.
   */
  def parseInt(value: String): Int = ???

  def parseIntTry(value: String): Try[Int] = Try(Integer.parseInt(value))
  def parseIntOption(value: String): Option[Int] = Try(Integer.parseInt(value)).toOption
  def parseIntEither(value: String): Either[Throwable, Int] = Try(Integer.parseInt(value)).toEither
  def parseIntZIO(value: String): IO[String, Int] = ZIO.effectTotal(Integer.parseInt(value)).mapError(_ => "Value is not a number")

  /**
   * EXERCISE 2
   *
   * Correct what is wrong with the return type of the following method.
   * Hint: You may have to make your own error type or modified the
   * `Result` type.
    *
    * [MB]
    * There might be more than one way this might fail (i.e. not a Date, not an integer, etc).
    * Likely, Either would be better to encode this error with a custom error type.
    * Use case is likely to evolve, developing more and more error cases.
   */
  sealed trait Result
  object Result {
    final case class Date(value: java.time.Instant) extends Result
    final case class Integer(value: Int)            extends Result
  }
  def parseResult(value: String): Option[Result] = ???

  /**
   * EXERCISE 3
   *
   * Correct what is wrong with the return type of the following method.
    *
    * [MB]
    * Try is more likely in this case as Throwable is being mapped to false.
    * The logic is not short-circuiting, so likely List[(Int, Throwable)]
   */
  def repeatN(n: Int, action: () => Unit, accum: Boolean = true): Boolean =
    if (n <= 0) accum
    else {
      val result = try {
        action(); true
      } catch { case _: Throwable => false }

      repeatN(n - 1, action, accum && result)
    }

  /**
   * EXERCISE 4
   *
   * Design an error type that can model failure to validate user-input.
   */
  sealed trait InvalidDataError
  object InvalidDataError {
    final case object EmptyStringProvided extends InvalidDataError
    final case class StringTooLong(l: Int) extends InvalidDataError
  }

  /**
   * EXERCISE 5
   *
   * Correct what is wrong with the following data model for HTTP client errors.
    *
    * [MB]
    * Information and successful are not errors. They should be removed. Similar to OtherError.
    * It's an error condition not expected. Every time you have a wrapper for Throwable is a smell.
   */
  sealed trait HttpClientError extends Throwable
  object HttpClientError {
    final case class Redirection(subtype: Int, message: String)   extends HttpClientError
    final case class ClientError(subtype: Int, message: String)   extends HttpClientError
    final case class ServerError(subtype: Int, message: String)   extends HttpClientError
  }

  /**
   * EXERCISE 6
   *
   * Using `ZIO#refineOrDie`, refine the error type of the following method to
   * `HttpClientError`.
   */
  trait Request
  trait Response
  class HttpError(code: Int, message: String) extends Throwable
  def httpRequest(request: Request): IO[HttpClientError, Response] = {
    lazy val response: IO[Throwable, Response] = ??? // Assume this is implemented

    response.refineOrDie {
      case httpClientError: HttpClientError => httpClientError
    }
  }

  /**
   * EXERCISE 7
   *
   * Modify all these methods to return ZIO effects, using the most narrow
   * error possible.
   */
  def parseInt2(value: String): Option[Int]                       = ???
  def httpRequest2(request: Request): Try[Response]               = ???
  def ensureEmailValid(email: String): Either[InvalidEmail, Unit] = ???
  final case class InvalidEmail(message: String)

  def parseInt2ZIO(value: String): IO[Unit, Int]                 = ???
  def httpRequest2ZIO(request: Request): IO[Throwable, Response] = ???
  def ensureEmailValidZIO(email: String): IO[InvalidEmail, Unit] = ???

  /**
   * EXERCISE 8
   *
   * Revise your solutions to Exercise 7, this time unifying the error types,
   * so all effects can be used in the same `for` comprehension.
   */

  sealed trait MyCustomErrors
  final case object ParseError extends MyCustomErrors
  final case class MyHttpError(t: Throwable) extends MyCustomErrors
  final case class MyInvalidEmailError(m: String) extends MyCustomErrors

  def parseInt2ZIOMyError(value: String): IO[MyCustomErrors, Int]             = ???
  def httpRequest2ZIOMyError(request: Request): IO[MyCustomErrors, Response]  = ???
  def ensureEmailValidZIOMyError(email: String): IO[MyCustomErrors, Unit]     = ???

  def func(s: String, r: Request, e: String) = for {
    _ <- parseInt2ZIOMyError(s)
    _ <- httpRequest2ZIOMyError(r)
    _ <- ensureEmailValidZIOMyError(e)
  } yield ()

  /**
   * EXERCISE 9
   *
   * Create a partial function that (plausibly) maps the "recoverable" errors
   * of `HttpClientError` into the semantic error type `UserServiceError`.
   */
  sealed trait UserServiceError
  object UserServiceError {
    final case class InvalidUserId(id: String) extends UserServiceError
    final object ServiceUnavailable            extends UserServiceError
  }
  val mapErrors1: PartialFunction[HttpClientError, UserServiceError] = {
    case ClientError(404, message) => InvalidUserId(message)
    case ServerError(_, _) => ServiceUnavailable
  }

  /**
   * EXERCISE 10
   *
   * Using `ZIO#refineOrDie` and `mapErrors1`, map the output of this
   * function.
   */
  def translateErrors(response: IO[HttpClientError, Response]): IO[UserServiceError, Response] = response.refineOrDie(mapErrors1)

  /**
   * EXERCISE 11
   *
   * Using `ZIO#sandbox`, bulletproof this web server to defects in the
   * request handler.
   */
  lazy val acceptConnection: Task[(Request, Response => Task[Unit])] = ??? // Assume implemented
  def launchServer(handler: Request => Task[Response]): UIO[Nothing] = {
    def loop: UIO[Nothing] =
      acceptConnection.foldM(_ => loop, {
        case (request, responder) =>
          handler(request).sandbox.foldM(
            _ => loop,
            response =>
              responder(response).sandbox.foldM(
                _ => loop,
                _ => loop
              )
          )
      })

    loop
  }

  /**
   * EXERCISE 12
   *
   * An app is built from services returning the following error types.
   * These error types do not compose. Fix the problem.
   */
  sealed trait AppError

  sealed trait CacheError                 extends AppError
  sealed trait PricingServiceError        extends AppError
  sealed trait ProductCatalogServiceError extends AppError
}

object zio_threads {

  /**
   * EXERCISE 1
   *
   * Count how many fibers are will be used in the following effect.
   */
  val effect1 =
    for {
      fiber1 <- fib(10).fork
      fiber2 <- fib(20).fork
      tuple  <- (fiber1 zip fiber2).join
    } yield tuple
  lazy val Effect1FiberCount = ???

  /**
   * EXERCISE 2
   *
   * Create a `Platform` with a single threaded executor.
   */
  val singleThread           = ExecutionContext.fromExecutor(java.util.concurrent.Executors.newSingleThreadExecutor())
  lazy val oneThreadPlatform = PlatformLive.fromExecutionContext(???)

  /**
   * EXERCISE 3
   *
   * Create a `Runtime` from the platform in Exercise 2.
   */
  lazy val runtime = Runtime(new DefaultRuntime {}.Environment, ???)

  /**
   * EXERCISEE 4
   *
   * Execute `effect1` using the `Runtime` you created in Exercise 3.
   */
  lazy val fibs: (BigInt, BigInt) = ???

  /**
   * EXERCISE 5
   *
   * Create a ZIO `Executor` from a Java `ThreadPoolExecutor`.
   */
  lazy val pool         = new ThreadPoolExecutor(???, ???, ???, ???, ???)
  lazy val poolExecutor = PlatformLive.ExecutorUtil.fromThreadPoolExecutor(_ => 1024)(pool)

  /**
   * EXERCISE 6
   *
   * Using `ZIO#lock`, make an effect that locks `effect1` to the pool you
   * created in Exercise 5.
   */
  lazy val lockedEffect1 = effect1 ?

  /**
   * EXERCISE 7
   *
   * Using the `blocking` combinator from `zio.blocking`, make an effect that
   * shifts `effect1` to a blocking thread pool.
   */
  lazy val blockedEffect1: ZIO[Blocking, Nothing, (BigInt, BigInt)] = blocking.blocking(???)

  /**
   * EXERCISE 8
   *
   * Write an `onDatabase` combinator that shifts execution of the provided
   * effect to the `databaseExecutor`, which is a special thread pool used
   * just for database connections.
   */
  lazy val databaseExecutor: Executor                      = Executor.fromExecutionContext(1024)(ExecutionContext.global)
  def onDatabase[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = zio.lock(databaseExecutor)

  /**
   * EXERCISE 9
   *
   * Implement a combinator to print out thread information before and after
   * executing the specified effect.
   */
  def threadLogged[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = {
    val log = UIO {
      val thread = Thread.currentThread()

      val id        = thread.getId()
      val name      = thread.getName()
      val groupName = thread.getThreadGroup().getName()

      println(s"Thread($id, $name, $groupName)")
    }

    zio
  }

  /**
   * EXERCISE 10
   *
   * Use the `threadLogged` combinator around different effects below to
   * determine which threads are executing which effects.
   */
  lazy val investigation =
    onDatabase {
      UIO(println("Database")) *>
        blocking.blocking {
          UIO(println("Blocking"))
        } *>
        UIO(println("Database"))
    }

  /**
   * EXERCISE 11
   *
   * Implement a method on `UserService` that can return a decorated
   * `UserService` that locks all methods onto the specified `Executor`.
   */
  trait UserService { self =>
    def lookupName(id: Long): Task[String]

    def lookupAddress(id: Long): Task[(Int, String)]

    def lock(exec: Executor): UserService = new UserService {

      override def lookupName(id: FiberId): Task[String] = self.lookupName(id).lock(exec)

      override def lookupAddress(id: FiberId): Task[(Int, String)] = self.lookupAddress(id).lock(exec)

    }
  }

  def fib(n: BigInt): UIO[BigInt] =
    if (n <= 1) UIO(n)
    else fib(n - 1).zipWith(fib(n - 2))(_ + _)
}

object reader {
  final case class Reader[-R, +A](run: R => A) { self =>
    def provide(r: R): Reader[Any, A] = Reader(_ => run(r))

    def map[B](f: A => B): Reader[R, B] = Reader(r => f(self.run(r)))

    def flatMap[R1 <: R, B](f: A => Reader[R1, B]): Reader[R1, B] =
      Reader(r => f(self.run(r)).run(r))
  }

  object Reader {
    def succeed[A](a: => A): Reader[Any, A] = Reader(_ => a)

    def environment[R]: Reader[R, R] = Reader(r => r)

    def access[R, A](f: R => A): Reader[R, A] = Reader(f)

    def accessM[R, A](f: R => Reader[R, A]): Reader[R, A] = ???
  }

  trait HasConfig1 {
    def config1: Config1
  }
  def config1: Reader[HasConfig1, Config1] = Reader.access(_.config1)

  final case class Config1(logFile: String, port: Int, timeout: Long)

  trait HasConfig2 {
    def config2: Config2
  }
  def config2: Reader[HasConfig2, Config2] = Reader.access(_.config2)

  final case class Config2(apiEndpoint: String)

  def printTimeout: Reader[HasConfig1, Unit] =
    config1.map(config => println(config.timeout))

  def openLogFile: Reader[HasConfig1, FileOutputStream] =
    config1.map(config => new FileOutputStream(new File(config.logFile)))

  def logPort(fos: FileOutputStream): Reader[HasConfig1, Unit] =
    config1.map(config => fos.write(config.port.toString.getBytes()))

  def printEndpoint: Reader[HasConfig2, Unit] =
    config2.map(config => println(config.apiEndpoint))

  def program: Reader[HasConfig1 with HasConfig2, Unit] =
    for {
      _   <- printTimeout
      fos <- openLogFile
      _   <- logPort(fos)
      _   <- printEndpoint
    } yield fos.close()

  program.run(new HasConfig1 with HasConfig2 {
    val config1 = Config1("logfile.dat", 8080, 1000L)
    val config2 = Config2("https://google.com/api")
  })
}

object environment {
  /**
   * EXERCISE 1
   * 
   * Make the `LiveUserStore` depend on a `Database` by having the 
   * `LiveUserStore` trait extend `Database`.
   */
   trait LiveUserStore extends UserStore with Database {

    override val userStore: UserStore.Service = new UserStore.Service {
      def getUserById(id: Long): Task[UserProfile] = database.query(s"SELECT * FROM USER WHERE ID = ${ id }") ?
    }

  }

  def getUserById(id: Long): ZIO[UserStore, Throwable, UserProfile] = ZIO.accessM[UserStore](_.userStore.getUserById(id))

   /**
    * EXERCISE 2
    *
    * Effectfully create a `Database` module inside `Task`. In a real 
    * implementation, this method might actually perform the database 
    * connection.
    */
   def connect(connectionUrl: String): Task[Database] = Task.effect {

     // This is where the connection creation would happen

     val dbConn = ???

     new Database {
       override val database: Database.Service = new Database.Service {
         def query(sql: String): Task[ResultSet] = ???
       }
     }
   }

   /**
    * EXERCISE 3
    * 
    * Define a `UserStore` in terms of a `Database`.
    */
   lazy val userService: ZIO[Database, Nothing, UserStore] = ZIO.accessM[Database]( env =>
   UIO {
      new UserStore {
        val userStore: UserStore.Service = new UserStore.Service {
          def getUserById(id: Long): Task[UserProfile] = env.database.query(s"SELECT * FROM USER WHERE ID = ${ id }") ?
        }
      }
   })

   /**
    * EXERCISE 4
    *
    * Using `provideSomeM`, eliminate the `UserStore` dependency.
    */
   lazy val myProgram: ZIO[UserStore, Throwable, Unit] = ??? // Assume implemented
   lazy val eliminated: ZIO[Database, Throwable, Unit] = myProgram.provideSomeM(userService)

   trait Database {
     val database: Database.Service
   }
   object Database {
     trait Service {
       def query(query: String): Task[ResultSet]
     }
   }

   trait UserStore {
     val userStore: UserStore.Service
   }
   object UserStore {
     trait Service {
      def getUserById(id: Long): Task[UserProfile]
     }
   }
   case class UserProfile(name: String, age: Int, address: String)
}