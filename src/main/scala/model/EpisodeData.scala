package model

class EpisodeData (_movieID: String, _episodeName: String, _episodeID: String, _episodeIndex: Int, _serverIndex: Int) {

    val movieID: String     = _movieID
    val episodeName: String = _episodeName
    val episodeID: String   = _episodeID
    val episodeIndex: Int   = _episodeIndex
    val serverIndex: Int    = _serverIndex

    var x: String = ""
    var y: String = ""

    def SetCoord(_x: String, _y: String): EpisodeData = {
        x = _x
        y = _y

        this
    }

    def SetCoord(coord: (String, String)): EpisodeData = {
        x = coord._1
        y = coord._2

        this
    }
}
