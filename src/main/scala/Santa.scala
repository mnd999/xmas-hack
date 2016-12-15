import akka.{Done, NotUsed}
import akka.actor.{Actor, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl._
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.ws._
import play.api.libs.json.{JsObject, JsValue, Json}

import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * Created by markdixon on 12/12/2016 for West London Hack Night
  * https://www.meetup.com/West-London-Hack-Night/
  */
object Santa extends App {

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val pw = Json.reads[Position]
  implicit val gw = Json.reads[Gps]

  import system.dispatcher

  val json = """{"gpss":[{"distance":18.601075237738275,"position":{"x":-10,"y":-8}},{"distance":10.63014581273465,"position":{"x":12,"y":-5}},{"distance":10.816653826391969,"position":{"x":-4,"y":9}}],"sampleCommands":[{"tag":"SetName","contents":"Kris"},{"tag":"SetColor","contents":"#ff0000"},­{"tag":"Move","contents":{"x":1,"y":-2}}],"players":[{"color":null,"score":0,"name":"<Your Name Here>","position":{"x":0,"y":0}}]} """

  val outgoing =  Source.actorRef[TextMessage](100, OverflowStrategy.dropHead)

  val incomingActor = system.actorOf(Props[GameActor])

  val incoming: Sink[Message, NotUsed] = Sink.actorRef[Message](incomingActor, None)
  val webSocketFlow = Http().webSocketClientFlow(WebSocketRequest("ws://localhost:8000/"))

  system.scheduler.schedule(1 second, 205 milliseconds, incomingActor, Tick)

  val ((queue, upgradeResponse), closed) =
  outgoing
    .viaMat(webSocketFlow)(Keep.both) // keep the materialized Future[WebSocketUpgradeResponse]
    .toMat(incoming)(Keep.both)
    .run()

  val connected = upgradeResponse.flatMap { upgrade =>
    if (upgrade.response.status == StatusCodes.OK) {
      Future.successful(Done)
    } else {
      throw new RuntimeException(s"Connection failed: ${upgrade.response.status}")
    }
  }

  connected.onComplete(println)

  queue ! msg("SetName","Badger")
  Thread.sleep(1000)
  queue ! msg("SetColor","#ffff00")

  def msg(tag: String, msg: String) = TextMessage(s"""{"tag":"$tag", "contents":"$msg"}""")

  class GameActor extends Actor {

    var state: Option[JsValue] = None

    override def receive: Receive = {
        case message: TextMessage.Strict =>
          val json = Json.parse(message.text)
          println(json)
          state = Some(json)
        case stream: TextMessage.Streamed => println("stream")
        case Tick => state.foreach(calc)
      }


    def calc(json: JsValue) = {


      val gpss = (json \ "gpss").as[List[Gps]]

      val intersect = CircleIntersect.calculateThreeCircleIntersection(
        gpss(0).position.x, gpss(0).position.y, gpss(0).distance,
        gpss(1).position.x, gpss(1).position.y, gpss(1).distance,
        gpss(2).position.x, gpss(2).position.y, gpss(2).distance)

      println(intersect)

      val pos = (json \ "players").as[Seq[JsObject]].find(x => (x \ "name").as[String] == "Badger").map(_ \ "position") match {
        case None =>
          queue ! msg("SetName", "Badger")
          None
        case Some(x) => Some(x.get.as[Position])
      }

      println(s"mypos $pos")

      for {
        p <- pos
        i <- intersect
        x = i.x - p.x
        y = i.y - p.y
      } yield moveMsg(Math.round(x), Math.round(y))

    }

    def moveMsg( x: Long, y: Long) = {
      println(s"moving to $x, $y")
      queue ! TextMessage(s"""{"tag":"Move","contents":{"x":$x,"y":$y}}""")
    }

  }

    case class Gps(distance: Double, position: Position)
    case class Position(x: Double, y: Double)
    case object Tick

  }

