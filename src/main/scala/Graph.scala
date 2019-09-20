import sun.security.provider.certpath.Vertex

class Graph(private var vertices: Int) {
  private val graph = new Array[List[Int]](vertices)

  def addEdge(src: Int, dest: Int): Unit = {
    updateList(src, dest)
    updateList(dest, src)
  }

  private def updateList(src: Int, dest: Int): Unit = {
    var listOpt = Option(graph(src - 1))
    listOpt match {
      case Some(list) => graph(src - 1) = list :+ dest
      case None => graph(src - 1) = List(dest)
    }
  }

  def getAdjacenyList(vertex: Int): List[Int] = {
    val list = graph(vertex - 1)
    list match {
      case null => List.empty[Int]
      case list => list
    }
  }

  def getKthDistanceNodes(sourceNode: Int, distance: Int): Set[Int] = {
    def getKthRec(src: Int, visited: Set[Int], distance: Int, result: Set[Int]): Set[Int] = {
      if(distance <1){
        result
      } else {
        getAdjacenyList(src).flatMap(n =>
          getKthRec(n, visited + src, distance - 1, getAdjacenyList(src).toSet.diff(visited))).toSet
      }
    }
    getKthRec(sourceNode, Set(sourceNode), distance, Set.empty[Int])
  }


  def getUptoKthDistanceNodes(sourceNode: Int, distance: Int): Set[Int] = {
    def getKthRec(src: Int, visited: Set[Int], distance: Int, result: Set[Int]): Set[Int] = {
      if(distance <1){
        result
      } else {
        getAdjacenyList(src).flatMap(n => getKthRec(n, visited + src, distance - 1, result++getAdjacenyList(src))).toSet
      }
    }
    getKthRec(sourceNode, Set.empty[Int], distance, Set.empty[Int])
  }

  override def toString: String = {
    var result: String = ""
    var counter = 1
    graph.foreach {
      case null => counter += 1
      case list =>
        list.foreach { elem =>
          result = result + s"( $counter, $elem)\n"
        }
        counter += 1
    }
    result
  }
}
