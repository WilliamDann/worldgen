// encapsulates world state information
case class WorldState(
  turn      : Int,
  map       : WorldMap,
  actors    : Vector[Actor],
  pops      : Vector[Population],
  gods      : Vector[God],
)

object WorldState:
  def initial(gods: Vector[God], width: Int, height: Int): WorldState =
    WorldState(
      turn = 0,
      map = WorldMap(width, height, Vector.tabulate(height, width) { (y, x) =>
        Tile(x, y, Void)
      }),
      actors = Vector.empty,
      pops   = Vector.empty,
      gods = gods
    )

case class Tile(x: Int, y: Int, terrain: Terrain)
case class WorldMap(width: Int, height: Int, tiles: Vector[Vector[Tile]])

object AnsiColor:
  val Reset         = "\u001b[0m"
  val Black         = "\u001b[30m"
  val Red           = "\u001b[31m"
  val Green         = "\u001b[32m"
  val Yellow        = "\u001b[33m"
  val Blue          = "\u001b[34m"
  val Magenta       = "\u001b[35m"
  val Cyan          = "\u001b[36m"
  val White         = "\u001b[37m"
  val BrightBlue    = "\u001b[94m"
  val BrightCyan    = "\u001b[96m"
  val BrightGreen   = "\u001b[92m"
  val BrightYellow  = "\u001b[93m"
  val BrightMagenta = "\u001b[95m"
  val Gray          = "\u001b[90m"
  val BrightRed     = "\u001b[91m"

def terrainSymbol(terrain: Terrain): String =
  import AnsiColor.*

  val (symbol, color) = terrain match
    case Void      => (" ", Reset)
    case Forest    => ("F", Green)
    case Desert    => ("D", BrightYellow)
    case Hills     => ("h", Yellow)
    case Mountain  => ("M", Gray)
    case Tundra    => ("T", White)
    case Plains    => ("P", BrightGreen)
    case Canyon    => ("C", Red)
    case Plateau   => ("L", Magenta)
    case Swamp     => ("S", Green)
    case Glacier   => ("G", Cyan)
    case Lake      => ("~", BrightBlue)
    case Marsh     => ("m", BrightCyan)
    case Ocean     => ("~", Blue)
    case River     => ("~", BrightCyan)
    case Volcano   => ("V", BrightRed)

  s"$color$symbol$Reset"

extension (map: WorldMap)
  def updateTile(x: Int, y: Int, f: Tile => Tile): WorldMap =
    val updatedRow = map.tiles(x).updated(y, f(map.tiles(x)(y)))
    val updatedTiles = map.tiles.updated(x, updatedRow)
    map.copy(tiles = updatedTiles)

  def toAsciiString: String =
    map.tiles.transpose.map { row =>
      row.map(t => terrainSymbol(t.terrain)).mkString(" ")
    }.mkString("\n")

extension (world: WorldState)
  def toAsciiString: String = {
    val width = world.map.width
    val height = world.map.height

    def tileChar(tile: Tile): String = {
      // Find populations on this tile
      val popsHere = world.pops.filter(p => p.tileX == tile.x && p.tileY == tile.y)

      if popsHere.nonEmpty then
        "s"
      else
        terrainSymbol(tile.terrain)
    }
  
    (0 until height).map { y =>
      (0 until width).map { x =>
        tileChar(world.map.tiles(x)(y))
      }.mkString(" ")
    }.mkString("\n")
  }
