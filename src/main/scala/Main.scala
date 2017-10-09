import java.io.PrintWriter

import io.circe._
import model._
import org.htmlcleaner._

import sys.process._
import scalaj.http._

object Main {

    val baseURL = "http://gostream.is"

    /**
      * 입력받은 url로 부터 movieID를 찾아내서 반환
      * @param url
      * @return movieID
      */
    def findMovieID(url: String): Option[String] = {

        val pattern = """https:\/\/gostream.is\/film\/.*\-(\d+)""".r

        pattern.findFirstMatchIn(url) match {
            case Some(m) => Some(m.group(1))
            case None => None
        }
    }

    /**
      * movieID로 부터 에피소드 정보들을 받아와서 EpisodeData로 만들어 반환
      * @param movieID
      * @return
      */
    def retrieveEpisodeData(movieID: String): Array[EpisodeData] = {

        // http request
        val retEpisodeIdsURL: String = baseURL + "/ajax/movie_episodes/" + movieID

        val response: HttpResponse[String] = Http(retEpisodeIdsURL)
            .option(HttpOptions.followRedirects(true))
            .asString

        // json parsing
        val html = io.circe.parser
            .parse(response.body)
            .getOrElse(Json.Null)
            .hcursor
            .get[String]("html")
            .getOrElse("")


        // html parsing
        val cleaner: HtmlCleaner = new HtmlCleaner()
        val props: CleanerProperties = cleaner.getProperties()
        props.setOmitComments(true)

        val rootNode: TagNode = cleaner.clean(html)

        val episodes: Array[EpisodeData] = rootNode
            .findElementByName("body", false)
            .getAllElements(false)
            .filter(tagNode => {

                // "class"들을 모아서,
                val classArr: Array[String] = Option(tagNode.getAttributeByName("class"))
                    .map(_.split(" "))
                    .getOrElse(Array.empty)

                // "server-item"이지만 "backup", "embed"가 아닌 놈들만 골라낸다.
                if (classArr.contains("server-item") &&
                    !classArr.contains("backup") &&
                    !classArr.contains("embed"))
                    true
                else
                    false
            })
            .map(_.findElementByAttValue("class", "les-content", false, false))
            .flatMap(_.getAllElements(false))
            .map(tagNode => {

                // build `EpisodeData`

                val title = tagNode.getAttributeByName("title")
                val episodeID = tagNode.getAttributeByName("data-id")
                val serverIndex = tagNode.getAttributeByName("data-server").toInt
                val episodeIndex = tagNode.getAttributeByName("data-index").toInt

                new EpisodeData(movieID, title, episodeID, episodeIndex, serverIndex)
            })

        return episodes
    }

    /**
      * 에피소드 파일의 x, y좌표값을 서버로부터 받아온다.
      * @param movieID
      * @param episodeID
      * @return
      */
    def retrieveEpisodeCoords(movieID: String, episodeID: String): Option[(String, String)] = {

        val coordURL: String = baseURL + "/ajax/movie_token?eid=" + episodeID + "&mid=" + movieID

        val response: HttpResponse[String] = Http(coordURL)
            .option(HttpOptions.followRedirects(true))
            .asString

        val jsScript = response.body + "\n" + "console.log(_x);\n" + "console.log(_y);\n"

        // Can't use the ScriptEngineManager due to its method bytecode limitation.

        try {

            // Create a temp file
            val file: java.io.File = java.io.File.createTempFile(s"${movieID}_${episodeID}", ".tmp")

            // Save result (java script code) to that temp file
            val out: PrintWriter = new PrintWriter(file)
            out.println(jsScript)
            out.flush()
            out.close()

            // Invoke node.js
            val res: String = "node " + file.getAbsolutePath !!
            val arr: Array[String] = res.split("\n")

            file.deleteOnExit()

            if (arr.length >= 2)
                Some(arr(0), arr(1))
            else
                None
        }
        catch {
            case e: Exception => return None
        }
    }

    /**
      * 에피소드 파일 url, 자막 url(있으면)을 서버로부터 가져온다.
      * @param episodeID
      * @param x
      * @param y
      * @return
      */
    def retrieveFileURLs(episodeID: String, x: String, y: String): (Option[Source], Option[Track]) = {

        val fileJsonURL: String = baseURL + "/ajax/movie_sources/" + episodeID + "?x=" + x + "&y=" + y

        val response: HttpResponse[String] = Http(fileJsonURL)
            .option(HttpOptions.followRedirects(true))
            .asString

        val playList: Vector[Json] = io.circe.parser.parse(response.body)
            .getOrElse(Json.Null)
            .hcursor
            .downField("playlist")
            .values.getOrElse(Vector.empty)

        val sources: Vector[Source] = playList
            .flatMap(_.hcursor.downField("sources").values)
            .flatten
            .map(json => {

                val cursor = json.hcursor

                val resolution = Util.toInt(cursor.get[String]("label").getOrElse("0p").dropRight(1)).getOrElse(0)
                val fileExt = cursor.get[String]("type").getOrElse("")
                val fileURL = cursor.get[String]("file").getOrElse("")

                new Source(fileURL, resolution, fileExt)
            })

        // Video files
        val source: Option[Source] = sources.nonEmpty match {
            case true => Some(sources.reduce((s0: Source, s1: Source) => if (s0.resolution - s1.resolution >= 0) s0 else s1))
            case false => None
        }

        // Subtitle files
        val track: Option[Track] = playList
            .flatMap(_.hcursor.downField("tracks").values)
            .flatten
            .map(json => {
                val cursor = json.hcursor

                val fileURL = cursor.get[String]("file").getOrElse("")
                val language = cursor.get[String]("label").getOrElse("")
                val kind = cursor.get[String]("kind").getOrElse("")
                val isDefault = cursor.get[Boolean]("default").getOrElse(false)

                new Track(fileURL, language, kind, isDefault)
            })
            .find(_.isDefault)

        (source, track)
    }

    def main(args: Array[String]): Unit = {

        val usage = "arguments: url server_index"

        if (args.length < 2) {
            println(usage)
            return
        }

        val url: String = args(0)
        val serverIndex: Int = Util.toInt(args(1)).getOrElse(0)

        // 1. Find movie id from the URL
        findMovieID(url)
            // 2. Retrieve episode data in every server
            .map(retrieveEpisodeData).getOrElse(Array.empty)
            .groupBy(_.serverIndex).filter { case(k, v) => k == serverIndex }
            .flatMap { case (k, v) => v }
            // 3. Retrieve x, y coordinates
            // .par
            .flatMap(epData => {
                retrieveEpisodeCoords(epData.movieID, epData.episodeID).map(epData.SetCoord)
            })
            // 4. From x, y coords, Retrieve (best) file URLs
            .map(epData => {
                val Pair(src, trk) = retrieveFileURLs(epData.episodeID, epData.x, epData.y)
                (epData, src, trk)
            })
            // .seq
            // and, print
            .foreach(trp => {
                println(trp._1.episodeName)
                trp._2.map(_.fileURL).foreach(println)
                trp._3.map(_.fileURL).foreach(println)
            })
    }
}