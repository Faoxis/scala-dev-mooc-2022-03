package futures

import HomeworksUtils.TaskSyntax

import java.util.concurrent.CountDownLatch
import scala.::
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    Future {
      val latch = new CountDownLatch(futures.size)
      val blockedFutures = futures
        .map(future => {
          future.transform(x => { latch.countDown(); x }, e => { latch.countDown(); e})
        })
      latch.await()

      blockedFutures.foldLeft[(List[A], List[Throwable])](List(), List()) { (results, future) =>
        future.value.get match {
          case Success(value) => (results._1 :+ value, results._2)
          case Failure(e) => (results._1, results._2 :+ e)
        }
      }
    }
  }
}
