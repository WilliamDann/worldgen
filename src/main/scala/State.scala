case class WorldState(
  turn    : Int,
  map     : WorldMap,
  pops    : Vector[Population],
  actors  : Vector[Actor],
  gods    : Vector[God]
)

object WorldState:
  def initial(gods: Vector[God]): WorldState =
    WorldState(
     turn = 0,
      map = WorldMap(50, 50, Vector.fill(50)(Vector.fill(50)(Tile(0, 0, Void, "region-1")))),
      actors = Vector.empty,
      pops   = Vector.empty,
      gods = gods
    )

