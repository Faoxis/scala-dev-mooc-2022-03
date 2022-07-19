package module1

import java.util.UUID
import scala.annotation.tailrec
import scala.language.postfixOps



/**
 * referential transparency
 */
 object referential_transparency{


  case class Abiturient(id: String, email: String, fio: String)

  type Html = String

  sealed trait Notification

  object Notification{
    case class Email(email: String, text: Html) extends Notification
    case class Sms(telephone: String, msg: String) extends Notification
  }


  case class AbiturientDTO(email: String, fio: String, password: String)

  trait NotificationService{
    def sendNotification(notification: Notification): Unit
    def createNotification(abiturient: Abiturient): Notification
  }


  trait AbiturientService{

    def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient
  }

  class AbiturientServiceImpl(val notificationService: NotificationService) extends AbiturientService{
    override def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient = {
      val notification = Notification.Email("", "")
      val abiturient = Abiturient(UUID.randomUUID().toString, abiturientDTO.email, abiturientDTO.fio)
      //notificationService.sendNotification(notification)
      // save(abiturient)
      abiturient
    }
  }


}


 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }


  def factRec(n: Int): Int = {
    if( n <= 0) 1 else n * factRec(n - 1)
  }

  def factTailRec(n: Int): Int = {
    @tailrec
    def loop(n: Int, accum: Int): Int =
      if( n <= 1) accum
      else loop(n - 1, n * accum)
    loop(n, 1)
  }



  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */


}

object hof{

   trait Consumer{
       def subscribe(topic: String): LazyList[Record]
   }

   case class Record(value: String)

   case class Request()
   
   object Request {
       def parse(str: String): Request = ???
   }

  /**
   *
   * Реализовать ф-цию, которая будет четать записи Request из топика, и сохранять их в базу
   */
   def createRequestSubscription() = {
     val cons : Consumer = ???
     val stream: LazyList[Record] = cons.subscribe("request")
     stream.foreach{ rec =>
       val req = Request.parse(rec.value)
       // save(req)
     }
   }

  def createSubscription(topic: String)(action: Record => Unit) = {
    val cons : Consumer = ???
    val stream: LazyList[Record] = cons.subscribe(topic)
    stream.foreach{ rec =>
     action(rec)
    }
  }

  def createRequestSubscription2() = createSubscription("request"){rec =>
    val req = Request.parse(rec.value)
    // save(req)
  }

  

  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val result: B = f(a)
    val end = System.currentTimeMillis()
    println(end - start)
    result
  }






  // изменение поведения ф-ции

  val arr = Array(1, 2, 3, 4, 5)

  def isOdd(i: Int): Boolean = i % 2 > 0

  
   
  def not[T](f: T => Boolean): T => Boolean = i => ! f(i)
  
  
  lazy val isEven = not(isOdd)




  // изменение самой функции

  // Follow type implementation

  def partial[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  def sum(x: Int, y: Int): Int = x + y

  val res: Int => Int = partial(1, sum)
  res(2) // sum(1, 2)


}






/**
 *  Реализуем тип Option
 */


 object opt {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

   sealed trait Option[+T]{
      def isEmpty: Boolean = this match {
        case Option.Some(v) => false
        case Option.None => true
      }

      def get: T = this match {
        case Option.Some(v) => v
        case Option.None => throw new Exception("Get on empty Option")
      }

      def map[B](f: T => B): Option[B] = this match {
        case Option.Some(v) => Option.Some(f(v))
        case Option.None => Option.None
      }

      def flatMap[B](f: T => Option[B]): Option[B] = this match {
        case Option.Some(v) => f(v)
        case Option.None => Option.None
      }

      def forEach(f: T => Unit): Unit = this match {
        case Option.Some(v) => f(v)
        case Option.None => { }
      }
   }

   object Option{
     case class Some[T](v: T) extends Option[T]
     case object None extends Option[Nothing]

     def apply[T](v: T): Option[T] = Some(v)
   }



  implicit class OptionExtensions[T](val option: Option[T]) {
    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */
    def printIfAny(): Unit = option match {
      case Option.Some(v) => println(v)
      case Option.None => { }
    }

    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */
    def zip[V](zipOption: Option[V]): Option[(T, V)] = {
      for {
        v1 <- option
        v2 <- zipOption
      } yield (v1, v2)
    }

    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */
    def filter(predicate: T => Boolean): Option[T] = option match {
      case Option.Some(v) if predicate(v) => option
      case _ => Option.None
    }
  }

}

 object list {
   /**
    *
    * Реализовать односвязанный иммутабельный список List
    * Список имеет два случая:
    * Nil - пустой список
    * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список)
    */
   sealed trait List[+T] {

     def headOption: Option[T] = this match {
       case List.::(head, _) => Option(head)
       case List.Nil => Option.empty
     }

     /**
      * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
      *
      */
     def ::[A >: T](elem: A): List[A] = List.::(elem, this)

     /**
      * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
      *
      */
     def mkString(separator: String = " "): String = {
       @tailrec
       def mkStringRec(acc: String, list: List[T], separator: String): String = {
         list match {
           case List.Nil => ""
           case List.::(head, List.Nil) => s"$acc$head"
           case List.::(head, tail) => mkStringRec(s"$acc$head$separator", tail, separator)
         }
       }
       mkStringRec("", this, separator)
     }

     /**
      *
      * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
      */
     def reverse(): List[T] = {
       @tailrec
       def reverse(list: List[T], acc: List[T]): List[T] = {
         list match {
           case List.Nil => acc
           case List.::(head, tail) => reverse(tail, List.::(head, acc))
         }
       }
       reverse(this, List.Nil)
     }

     /**
      *
      * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
      */
     def map[D](f: T => D): List[D] = {
       @tailrec
       def map(list: List[T], acc: List[D]): List[D] = {
         list match {
           case List.Nil => acc
           case List.::(head, tail) => map(tail, f(head) :: acc)
         }
       }
       // Аналогично с apply
       // Не могу придумать как простой хвостовой рекурсией справится без reverse
       map(this, List.Nil).reverse()
     }

     /**
      * Реализовать метод filter для списка который будет фильтровать список по некому условию
      */
     def filter(f: (T => Boolean)): List[T] = {
       @tailrec
       def filter(list: List[T], acc: List[T]): List[T] = {
         list match {
           case List.Nil => acc
           case List.::(head, tail) =>
             if (f(head)) filter(tail, head :: acc)
             else filter(tail, acc)
         }
       }
       filter(this, List.Nil).reverse()
     }


   }

   object List {
     case class ::[A](head: A, tail: List[A]) extends List[A]
     case object Nil extends List[Nothing]

     /**
      * Конструктор, позволяющий создать список из N - го числа аргументов
      * Для этого можно воспользоваться *
      *
      * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
      * def printArgs(args: Int*) = args.foreach(println(_))
      */
     def apply[A](elements: A*): List[A] = {
        @tailrec
        def applyRec(acc: List[A], elements: A*): List[A] = {
          if (elements.isEmpty) acc
          else applyRec(List.::(elements.head, acc), elements.tail: _*)
        }
        // Здесь идет reverse, чтобы список работал как стандартный List
        // Интересно, можно ли сделать это более эффективно без reverse в лямбде
        applyRec(Nil, elements: _*).reverse()
     }
   }


    /**
      *
      * Написать функцию incList котрая будет принимать список Int и возвращать список,
      * где каждый элемент будет увеличен на 1
      */
    def incList(list: List[Int]): List[Int] = list.map(_ + 1)

    /**
      *
      * Написать функцию shoutString котрая будет принимать список String и возвращать список,
      * где к каждому элементу будет добавлен префикс в виде '!'
      */
    def shoutString(list: List[String]): List[String] = list.map(_ + "!")

 }