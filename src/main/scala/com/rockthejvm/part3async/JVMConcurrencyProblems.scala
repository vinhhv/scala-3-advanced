package com.rockthejvm.part3async

object JVMConcurrencyProblems {

  def runInParallel(): Unit = {
    var x = 0

    val thread1 = new Thread(() => {
      x = 1
    })

    val thread2 = new Thread(() => {
      x = 2
    })

    thread1.start()
    thread2.start()
    Thread.sleep(1000)
    println(x) // race condition
  }

  case class BankAccount(var amount: Int)

  def buy(account: BankAccount, thing: String, price: Int): Unit = {
    /*
      invovles 3 steps
      - read old value
      - compute result
      - write new value
     */
    account.amount -= price
  }

  def buySafe(account: BankAccount, thing: String, price: Int): Unit = {
    account.synchronized { // does not allow multiple threads to run critical section AT THE SAME TIME
      account.amount -= price // critical section
    }
  }
  /*
    Example race condition:
    thread1 (shoes)
      - reads amount 50000
      - compute result 50000 - 3000 = 47000
    thread2 (iphone)
      - reads amount 50000
      - compute result 50000 - 4000 = 46000
    thread1 (shoes)
      - write amount 47000
    thread2 (iphone)
      - write amount 46000
   */
  def demoBankingProblem(): Unit = {
    (1 to 10000).foreach { _ =>
      val account = BankAccount(50000)
      val thread1 = new Thread(() => buy(account, "shoes", 3000))
      val thread2 = new Thread(() => buy(account, "iPhone", 4000))
      thread1.start()
      thread2.start()
      thread2.join()
      thread1.join()
      if (account.amount != 43000) println(s"Bank broken: ${account.amount}")
    }
  }

  /**
   * Execises
   * 1 - create "inception threads"
   *  thread1
   *    -> thread2
   *      -> thread3
   *        ...
   *  each thread prints "hello from thread $i"
   *  Print all messages IN REVERSE ORDER
   *
   * 2 - what's the max/min value of x
   * 3 = "sleep falacy" - what's the value of the message?
   */

  def printThread(count: Int): Unit = {
    println(count)
  }

  // 1
  def inceptionThreadsMe(start: Int = 1, until: Int = 50): Unit = {
    if start > until then ()
    else {
      val thread = new Thread(() => inceptionThreadsMe(start + 1))
      thread.start()
      Thread.sleep(1000)
      thread.join()
      println(start)
    }
  }

  def inceptionThreads(maxThreads: Int, i: Int = 1): Thread = {
    new Thread(() => {
      if (i < maxThreads) {
        val newThread = inceptionThreads(maxThreads, i + 1)
        newThread.start()
        newThread.join()
      }
      println(s"Hello from thread $i")
    })
  }

  // 2 - min = 1 and max = 100
  // all threads read, compute and write at the same
  def minMaxX(): Unit = {
    var x = 0
    val threads = (1 to 100).map(_ => new Thread(() => x += 1))
    threads.foreach(_.start())
  }

  // 3 -
  /*
    almost always, message = "Scala is awesome"
    not guaranteed!!!

    main thread:
      message = "Scala sucks"
      awesomeThread.start()
      sleep(1001) - yields execution

    awesome thread:
      sleep(1000) - yields execution

    OS gives the CPU to some important thread, takes > 2s
    OS gives the CPU back to the main thread
    main thread:
      println(message) // "Scala sucks"

    awesome thread:
      message = "Scala is awesome"
   */
  def demoSleepFallacy(): Unit = {
    var message = ""
    val awesomeThread = new Thread(() => {
      Thread.sleep(1000)
      message = "Scala is awesome"
    })

    message = "Scala sucks"
    awesomeThread.start()
    Thread.sleep(1001)

    // solution: join the worker thread
    awesomeThread.join()

    println(message)
  }

  def main(args: Array[String]): Unit = {
    // runInParallel()
    // demoBankingProblem()
    // inceptionThreadsMe()
    inceptionThreads(50).start()
    demoSleepFallacy()
  }
}
