// helper to get coords for tiles around a tile
def neighbors(x: Int, y: Int, maxX: Int, maxY: Int): List[(Int, Int)] =
  List(
    (x - 1, y), // left
    (x + 1, y), // right
    (x, y - 1), // up
    (x, y + 1)  // down
  ).filter { case (nx, ny) =>
  nx >= 0 && nx < maxX && ny >= 0 && ny < maxY
}
