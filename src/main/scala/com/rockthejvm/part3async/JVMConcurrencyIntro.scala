package com.rockthejvm.part3async

import java.util.concurrent.Executors

object JVMConcurrencyIntro {

  val runnable = new Runnable {
    override def run(): Unit = {
      println("waiting")
      Thread.sleep(2000)
      println("running on some thread")
    }
  }

  // threads on the JVM
  val aThread = new Thread(runnable)
  // aThread.start() // will run the runnable on some JVM thread
  // JVM thread == OS thread (soon to change via Project Loom (schedule multiple threads on less OS thread))
  // aThread.join() // block until thread finishes

  // order of operations is NOT guaranteed
  // different runs = different results!
  def orderOfExecution(): Unit = {
    val threadHello = new Thread(() => (1 to 5).foreach(_ => println("Hello")))
    val threadGoodbye = new Thread(() => (1 to 5).foreach(_ => println("Goodbye")))
    threadHello.start()
    threadGoodbye.start()
  }

  // executors
  def demoExecutors(): Unit = {
    val threadPool = Executors.newFixedThreadPool(4)
    // submit a computation
    threadPool.execute(() => println("something in the thread pool"))
    threadPool.execute{ () =>
      Thread.sleep(1000)
      println("done after one second")
    }
    threadPool.execute { () =>
      Thread.sleep(1000)
      println("almost done")
      Thread.sleep(1000)
      println("done after 2 seconds")
    }

    threadPool.shutdown()
    // threadPool.execute(() => println("should throw"))
  }

  def main(args: Array[String]): Unit = {
    demoExecutors()
  }
}
