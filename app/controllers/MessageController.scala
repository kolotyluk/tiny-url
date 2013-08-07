package controllers

import java.io.File
import java.io.PrintWriter
import java.security.MessageDigest
import java.util.UUID

import play.api.mvc.{Action, Controller}
import play.api.libs.json.Json
import play.api.Routes
import play.Logger

import scala.io.Source
import scala.annotation.tailrec

import sun.misc.BASE64Encoder

case class Message(value: String)

/**
 * MVC Controller for Typesafe Play Framework
 * @author Eric Kolotyluk
 */
object MessageController extends Controller {

  implicit val fooWrites = Json.writes[Message]
  
  /**
   * Process a Tiny URL by redirecting to the Big URL.
   * The external hash file is named with the external hash
   * of the original URL, and contains the original URL,
   * the internal hash, and the click count.
   */
  def t(externalHash: String) = Action { implicit request =>
    val externalHashFile = new File(externalHashfiles, externalHash)
    if (externalHashFile.exists) {
      val contents = Source.fromFile(externalHashFile).mkString.split(" ")
      if (contents.length > 0) {
        val clicks = if (contents.length < 3) 1 else contents(2).toInt + 1
        write(externalHashFile, contents(0) + " " + contents(1) + " " + clicks)
        Logger.info(externalHashFile.getAbsolutePath() + " click-count = " + clicks)
        Redirect(contents(0))
      } else {
        Logger.error(externalHashFile.getAbsolutePath() + " is empty. Cannot find original URL to redirect to")
        InternalServerError("Oops")
      }
    } else {
      Logger.error("Could not find " + externalHashFile.getAbsolutePath())
      NotFound(<h1>Page not found</h1>)
    }
  }
  
  /**
   * Process the submitted big URL into a tiny URL
   */
  def submit() = Action { implicit request =>
    val bigUrl = request.queryString("urlString").head
    val tinyUrl = convert(bigUrl)
    lastUrl = tinyUrl
    Logger.info("")
    Ok(Json.toJson(Message(tinyUrl)))
  }


  def getMessage = Action {
    Ok(Json.toJson(Message(lastUrl)))
  }

  def javascriptRoutes = Action { implicit request =>
    Ok(Routes.javascriptRouter("jsRoutes")(routes.javascript.MessageController.getMessage)).as(JAVASCRIPT)
  }

  var lastUrl = ""

  /**
   * We use the file system as a persistent store of hashed URLs.
   * A hash file contains "<Big URL> <Alternate Hash>", two strings
   * separated by a space.
   */
  val hashfiles = new File("Hashfiles")
  val externalHashfiles = new File(hashfiles, "External")
  val internalHashfiles = new File(hashfiles, "Internal")
  mkdirs(externalHashfiles)
  mkdirs(internalHashfiles)
  
  def mkdirs(parent: File) = {
    if (parent.mkdirs()) {
      Logger.info("created: " + parent.getAbsolutePath())
      val readme = new File(parent, "readme.txt")
      // TODO - Write a more informative message.
      write(readme, "Do not modify any files in this directory!")
      // However, for testing purposes it is OK to delete the entire Hashfiles directory
      // In fact, it is best to delete the whole directory and let it be recreated.
    } else {
      Logger.info(parent.getAbsolutePath() + " already exists")
    }
  }

  val base64Encoder = new BASE64Encoder()

  def base64(message: Array[Byte]) = base64Encoder.encode(message)
  
  /**
   * Technically this should work with hash functions that produce
   * a shorter hash, but for now this is safe and easy. CRC-32 or
   * CRC-16 should work, but there will be more collisions.
   */
  def hash(message: String) = {
    MessageDigest.getInstance("MD5").digest(message.getBytes)
  }

  /**
   * We are using filenames for the URL hashes, so we convert the binary
   * to base 64 and replace / with _ to keep the file system happy, and
   * to produce a valid tiny URL.
   * 
   * NOTE: this will fail on file systems that do not support mixed case.
   * 
   * @see http://en.wikipedia.org/wiki/Base64
   */
  def filenameHash(message: String) = base64(hash(message)).replace("/", "_")

  def write(file: File, string: String) {
    val printWriter = new PrintWriter(file , "UTF-8")
    try{ printWriter.print(string) }
    finally{ printWriter.close }
  }

  /**
   * Make a hashfile for a given key/value pair, and if there is a collision,
   * try a different version of the key. This currently does not support the
   * deletion of a hash, as that would break collision handling.
   * 
   * TODO - This is not guaranteed to terminate, but it probably will.
   */
  @tailrec def makeFile(parent: File, key: String, value: String, version: Int): File = {
    val file = new File(parent, key + version)
    if (!file.exists()) file
    else {
      if (file.length == 0) Logger.error("Internal Consistency Error: file should not be empty: " + file.getAbsolutePath())
      val contents = Source.fromFile(file).mkString.split(" ")
      if (contents.length > 0 && contents(0) == value) file
      else makeFile(parent, key, value, version + 1)
    }
  }

  /**
   * Convert a given URL to a Tiny URL based on a hash. This is a rather primitive
   * implementation that just uses the file system for persistence instead of a DBMS.
   * The internal hash is used to produce consistent hashes so we remember what we have
   * hashed, while the external hash will be unique among parallel services.
   */
  def convert(value: String) : String = {
    val internalHash = filenameHash(value)
    val externalHash = filenameHash(value + UUID.randomUUID())
    val finalInternalHashfile = makeFile(internalHashfiles, internalHash, value, 0)
    val finalExternalHash = 
      if (!finalInternalHashfile.exists() || finalInternalHashfile.length() == 0) {
      val finalExternalHashfile = makeFile(externalHashfiles, externalHash, finalInternalHashfile.getName(), 0)
      write(finalInternalHashfile, value + " " + finalExternalHashfile.getName())
      write(finalExternalHashfile, value + " " + finalInternalHashfile.getName())
      finalExternalHashfile.getName()
    } else {
      Logger.info("Found internal hash file " + finalInternalHashfile.getAbsolutePath())
      val contents = Source.fromFile(finalInternalHashfile).mkString.split(" ")
      if (contents.length > 1) {
        Logger.info("Using external hash: " + contents(1))
        contents(1)
      }
      else {
        Logger.error("Cannot find external hash")
        "error"
      }
    }
    "http://quantcast.dlinkddns.com:9000/t/" + finalExternalHash
  }

}