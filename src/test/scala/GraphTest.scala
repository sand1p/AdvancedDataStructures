import org.scalatest.FunSuite

class GraphTest extends FunSuite {
  val graph = new Graph(7)
  graph.addEdge(1,2)
  graph.addEdge(5,2)
  graph.addEdge(5,3)
  graph.addEdge(4,3)
  graph.addEdge(5,6)
  graph.addEdge(7,6)

  test("2ShouldBeInAAdjacencyListOf1") {
    assert( graph.getAdjacenyList(1).contains(2) && graph.getAdjacenyList(1).length == 1)
  }

  test("2IsAtDistanceOf1From1") {
    val result = graph.getUptoKthDistanceNodes(1,1)
    println(result)
    assert(result.contains(2))
  }

  test("5IsAtDistanceOf2From1") {
    val result = graph.getUptoKthDistanceNodes(1,2)
    println(result)
    assert(result.contains(5))
  }

  test("3And6AreAtUptoADistanceOf3From1") {
    val result = graph.getUptoKthDistanceNodes(1,3)
    println(result)
    assert(result.contains(3) && result.contains(6))
  }

  test("3And6AreAtDistanceOf3From1") {
    val result = graph.getKthDistanceNodes(1,3)
    println(result)
    assert(result.contains(3) && result.contains(6))
  }





}
