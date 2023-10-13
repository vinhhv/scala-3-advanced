package com.rockthejvm.part3async

import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.{Failure, Random, Success, Try}
import scala.concurrent.duration.*

object Futures {

  def calculateMeaningOfLife(): Int = {
    // simulate long compute
    Thread.sleep(1000)
    42
  }

  // thread pool (Java-specific)
  val executor = Executors.newFixedThreadPool(4)
  // thread pool (Scala-specific)
  given executionContext: ExecutionContext =
    ExecutionContext.fromExecutorService(executor)

  // a future = an async computation that will finish at some point
  val aFuture: Future[Int] =
    Future.apply(calculateMeaningOfLife())(executionContext)

  // Option[Try[Int]]
  // we don't if we have a value
  // if we do, it could fail
  val futureInstantResult: Option[Try[Int]] = aFuture.value

  // callbacks
  // aFuture.onComplete {
  //   case Success(value)     => println(s"I've completed the MOL: $value")
  //   case Failure(exception) => println(s"Async computation failed: $exception")
  // } // on SOME other thread

  /*
    Functional Composition
   */
  case class Profile(id: String, name: String) {
    def sendMessage(profile: Profile, message: String) = {
      println(s"${this.name} sending message to ${profile.name}: $message")
    }
  }

  object SocialNetwork {
    // "database"
    val names = Map(
      "rtjvm.id.1-daniel" -> "Daniel",
      "rtjvm.id.2-jane"   -> "Jane",
      "rtjvm.id.3-mark"   -> "Mark"
    )

    val friends = Map(
      "rtjvm.id.2-jane" -> "rtjvm.id.3-mark"
    )

    val random = new Random()

    // "API"
    def fetchProfile(id: String): Future[Profile] = Future {
      // fetch something from the database
      Thread.sleep(random.nextInt(300)) // simulate the time delay
      Profile(id, names(id))
    }

    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(400))
      val bestFriendId = friends(profile.id)
      Profile(bestFriendId, names(bestFriendId))
    }
  }

  // problem: sending a message to my best friend
  def sendMessageToBestFriend(accountId: String, message: String): Unit = {
    // 1. call fetch profile
    // 2. call fetch best friend
    // 3. call send message
    val profileFuture = SocialNetwork.fetchProfile(accountId)
    profileFuture.onComplete {
      case Success(profile) =>
        val friendProfileFuture = SocialNetwork.fetchBestFriend(profile)
        friendProfileFuture.onComplete {
          case Success(friendProfile) =>
            profile.sendMessage(friendProfile, message)
          case Failure(ex) => ex.printStackTrace()
        }
      case Failure(ex) => ex.printStackTrace()
    }
  }

  // onComplete is a hassle
  // solution: functional composition
  def sendMessageToBestFriend_v2(accountId: String, message: String): Unit = {
    val profileFuture = SocialNetwork.fetchProfile(accountId)
    val action: Future[Unit] =
      profileFuture
        .flatMap { profile =>
          SocialNetwork
            .fetchBestFriend(profile)
            .map { bestFriend => profile.sendMessage(bestFriend, message) }
        }
  }

  def sendMessageToBestFriend_v3(accountId: String, message: String): Unit = {
    for {
      profile    <- SocialNetwork.fetchProfile(accountId)
      bestFriend <- SocialNetwork.fetchBestFriend(profile)
    } yield profile.sendMessage(bestFriend, message)
  }

  val janeProfileFuture = SocialNetwork.fetchProfile("rtjvm.id.2-jane")
  val janeFuture: Future[String] =
    janeProfileFuture.map(_.name) // map transforms value inside, ASYNC
  val janesBestFriend: Future[Profile] =
    janeProfileFuture.flatMap(profile => SocialNetwork.fetchBestFriend(profile))
  val janesBestFriendFilter: Future[Profile] =
    janesBestFriend.filter(profile => profile.name.startsWith("Z"))

  // fallbacks
  val profileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recover {
    case e: Throwable => Profile("rtjvm.id.0-dummy", "Forever alone")
  }

  // if both futures fail, exception is from the recovered call
  val aFetchedProfileNoMatterWhat: Future[Profile] =
    SocialNetwork.fetchProfile("unknown id").recoverWith { case e =>
      SocialNetwork.fetchProfile("rtjvm.id.0-dummy")
    }

  // if both futures fail, exception is from the original call
  val fallBackProfile: Future[Profile] = SocialNetwork
    .fetchProfile("unknown id")
    .fallbackTo(SocialNetwork.fetchProfile("rtjvm.id.0-dummy"))

  /*
    Block for a future
   */
  case class User(name: String)
  case class Transaction(
      sender: String,
      receiver: String,
      amount: Double,
      status: String
  )

  object BankingApp {
    // APIs
    def fetchUser(name: String): Future[User] = Future {
      // simulate some DB fetching
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(
        user: User,
        merchantName: String,
        amount: Double
    ): Future[Transaction] = Future {
      // simulate payment
      Thread.sleep(1000)
      Transaction(user.name, merchantName, amount, "SUCCESS")
    }

    // external API
    def purchase(
        username: String,
        item: String,
        merchantName: String,
        price: Double
    ): String = {
      /*
        1. fetch user
        2. create txn
        3. WAIT for the txn to finish
       */
      val transactionStatusFuture: Future[String] = for {
        user        <- fetchUser(username)
        transaction <- createTransaction(user, merchantName, price)
      } yield transaction.status

      // blocking call
      Await.result(
        transactionStatusFuture,
        2.seconds
      ) // throws TimeoutException if takes longer than 2 seconds
    }
  }

  /*
    Promises
   */
  def demoPromises(): Unit = {
    val promise                   = Promise[Int]()
    val futureInside: Future[Int] = promise.future

    // thread 1 - "consumer": monitor the future for completion
    futureInside.onComplete {
      case Success(value) =>
        println(s"[consumer] I've just completed with $value")
      case Failure(exception) => exception.printStackTrace()
    }

    // thread 2 - "producer"
    val producerThread = new Thread(() => {
      println("[producer] crunching numbers...")
      Thread.sleep(1000)
      // "fulfill" the promise
      promise.success(42)
      println("[producer] I'm done")
    })

    producerThread.start()
  }

  /**
   * Exercises
   * 1) Fulfill a future IMMEDIATELY with a value.
   * 2) In sequence: make sure the first Future has been completed before
   *    returning the second
   * 3) first(fa, fb) => new Future with the value of the first Future to complete
   * 4) last(fa, fb) => new Future with the value of the LAST Future to complete
   * 5) retry an action returning a Future until a predicate holds true
   */

  // 1
  val immediateFuture: Future[Int] =
    Future.successful(1) // async completion as soon as possible

  def completeImmediately[A](value: A): Future[A] =
    Future.successful(value) // synchronous

  // 2
  def inSequence[A, B](first: Future[A], second: Future[B]): Future[B] = {
    for {
      _     <- first
      value <- second
    } yield value
  }

  // 3
  def first[A](f1: Future[A], f2: Future[A]): Future[A] = {
    val promise = Promise[A]()
    f1.onComplete(result1 => promise.tryComplete(result1))
    f2.onComplete(result2 => promise.tryComplete(result2))

    promise.future
  }

  // 4
  def second[A](f1: Future[A], f2: Future[A]): Future[A] = {
    val bothPromise = Promise[A]()
    val lastPromise = Promise[A]()

    def checkAndComplete(result: Try[A]): Unit =
      if (!bothPromise.tryComplete(result))
        lastPromise.complete(result)

    f1.onComplete(result => checkAndComplete(result))
    f2.onComplete(result => checkAndComplete(result))

    lastPromise.future
  }

  def retryUntil[A](
      action: () => Future[A],
      predicate: A => Boolean
  ): Future[A] = {
    action().filter(predicate).recoverWith { case _ =>
      retryUntil(action, predicate)
    }
  }

  def testRetries(): Unit = {
    val random = new Random()
    val action = () =>
      Future {
        Thread.sleep(100)
        val nextValue = random.nextInt(100)
        println(s"Generated $nextValue")
        nextValue
      }

    val predicate = (x: Int) => x < 10

    retryUntil(action, predicate).foreach(finalResult =>
      println(s"Settled at $finalResult")
    )
  }

  def main(args: Array[String]): Unit = {
    // sendMessageToBestFriend_v3("rtjvm.id.2-jane", "Hey best friend, sup")
    // println("purchasing...")
    // println(BankingApp.purchase("vinh-234", "shoes", "merchant-987", 3.56))
    // println("purchase complete")
    // demoPromises()

    def slow = Future {
      Thread.sleep(3000)
      3
    }

    def fast = Future {
      Thread.sleep(2000)
      2
    }

    // println(Await.result(first(slow, fast), 5.seconds))
    // println(Await.result(first(fast, slow), 5.seconds))
    // println(Await.result(second(slow, fast), 5.seconds))
    // println(Await.result(second(fast, slow), 5.seconds))

    testRetries()

    Thread.sleep(3000)
    executor.shutdown()
  }
}
