package net.scalax.slick.async

import net.scalax.umr.{ MappedShape, ShapeHelper }

import scala.language.higherKinds
import slick.jdbc.H2Profile.api._
import org.h2.jdbcx.JdbcDataSource
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case class Friends(
  id: Option[Long] = None,
  name: String,
  nick: String)

class FriendTable(tag: slick.lifted.Tag) extends Table[Friends](tag, "firend") {
  def id = column[Long]("id", O.AutoInc)
  def name = column[String]("name")
  def nick = column[String]("nick")

  def * = (id.?, name, nick).mapTo[Friends]
}

class AsyncTest extends FlatSpec
  with Matchers
  with EitherValues
  with ScalaFutures
  with BeforeAndAfterAll
  with BeforeAndAfter
  with ShapeHelper {

  val t = 10.seconds
  override implicit val patienceConfig = PatienceConfig(timeout = t)
  val logger = LoggerFactory.getLogger(getClass)

  val friendTq = TableQuery[FriendTable]

  lazy val db = {
    val datasource = new JdbcDataSource()
    datasource.setUrl(s"jdbc:h2:mem:hfTest;DB_CLOSE_DELAY=-1")
    Database.forDataSource(datasource, None)
  }

  override def beforeAll = {
    db.run(friendTq.schema.create).futureValue
  }

  before {
    val friend1 = Friends(None, "喵", "汪")
    val friend2 = Friends(None, "jilen", "kerr")
    val friend3 = Friends(None, "小莎莎", "烟流")
    db.run(friendTq ++= List(friend1, friend2, friend3)).futureValue
    friendTq.result
  }

  after {
    db.run(friendTq.delete).futureValue
  }

  "model" should "select with DBIO mode" in {
    try {
      val friendQuery = for {
        inFriend <- friendTq.result
      } yield {
        inFriend.map { s =>
          //println(s)
          s
        }
      }
      db.run(friendQuery).futureValue
      db.run(friendTq.update(Friends(None, "hahahahaha", "hahahahaha"))).futureValue
      db.run(friendQuery).futureValue
    } catch {
      case e: Exception =>
        logger.error("error", e)
        throw e
    }
  }

  "shape" should "decode reps with db" in {
    val query = friendTq.map { friend =>
      (friend.id, List(friend.nick, MappedShape.repMap(friend.name -> friend.nick).map { case (name, nick) => s"姓名：$name 昵称：$nick" }), friend.id)
    }
    try {
      val friendQuery = for {
        inFriend <- query.result
      } yield for {
        s <- inFriend
      } yield {
        println(s)
        s
      }
      db.run(friendQuery).futureValue
    } catch {
      case e: Exception =>
        logger.error("error", e)
        throw e
    }
  }

  "shape" should "decode reps with update" in {
    val query = friendTq.map { friend =>
      List(friend.nick, friend.name)
    }
    try {
      val action = query.update(List("汪汪酱", "喵喵酱"))
      db.run(action).futureValue
    } catch {
      case e: Exception =>
        logger.error("error", e)
        throw e
    }
  }

  import io.circe.generic.auto._
  import io.circe.syntax._
  import io.circe._

  case class ColInfo(name: String, typeName: String)

  //列信息来源
  val infos = Seq(
    ColInfo("id", "Long"),
    ColInfo("name", "String"),
    ColInfo("nick", "String"))

  def tableToCol(commonTable: FriendTable) = {
    infos.map { info =>
      info match {
        case ColInfo(name, "Long") => MappedShape.toJson(name, commonTable.column[Long](name))
        case ColInfo(name, "String") => MappedShape.toJson(name, commonTable.column[String](name))
        case ColInfo(name, "Int") => MappedShape.toJson(name, commonTable.column[Int](name))
      }
    }
  }

  "shape" should "decode data to json object" in {
    val query = friendTq.map { friend =>
      tableToCol(friend)
    }
    try {
      val action = for {
        inFriend <- query.result
      } yield for {
        s <- inFriend
      } yield {
        JsonObject.fromMap(s.toMap)
      }
      val jobj = db.run(action).futureValue
      println(jobj.map(_.asJson).mkString("\n"))
    } catch {
      case e: Exception =>
        logger.error("error", e)
        throw e
    }
  }

}