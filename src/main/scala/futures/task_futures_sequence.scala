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
    Future.traverse(futures)(f => f.transform(Success.apply)).map { trys =>
      val as = trys.collect { case Success(a) => a }
      val errs = trys.collect { case Failure(err) => err }
      as -> errs
    }
  }
}
