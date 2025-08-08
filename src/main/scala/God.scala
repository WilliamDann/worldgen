import scala.util.Random

// a god takes actions (GodIntents) in the world aligned with their reward function
trait God:
  def id          : String
  def name        : String
  def domain      : Domain

  def ipPerTurn(state: WorldState): Int
  def generateIntents(state: WorldState): List[Intent]

// base instance for a god type
case class BaseGod(
  id            : String,
  name          : String,
  domain        : Domain,
  ipPerTurnFn   : WorldState => Int,
  ai            : WorldState => List[Intent]
) extends God:
  def ipPerTurn(state: WorldState): Int = ipPerTurnFn(state)
  def generateIntents(state: WorldState): List[Intent] = ai(state)

// base class for gods that are able to modify world tiles
case class PrimordialGod(
  id            : String,
  name          : String,
  domain        : Terrain,
  ipPerTurn     : Int
) extends God:
  
  // get coords for tiles around a tile
  def neighbors(x: Int, y: Int, maxX: Int, maxY: Int): List[(Int, Int)] =
    List(
      (x - 1, y), // left
      (x + 1, y), // right
      (x, y - 1), // up
      (x, y + 1)  // down
    ).filter { case (nx, ny) =>
      nx >= 0 && nx < maxX && ny >= 0 && ny < maxY
    }

  // generate legal 'moves' for a god
  def generateIntents(state: WorldState): List[Intent] = {
    val maxX = state.map.width
    val maxY = state.map.height

    val sourceTiles =
      for
        x <- 0 until maxX
        y <- 0 until maxY
        if state.map.tiles(x)(y).terrain == domain
      yield (x, y)

    val domainSize = sourceTiles.size
    val maxDomainSize = maxX * maxY / 10

    // Spread to orthogonal neighbors that are not already your domain
    val spreadingIntents =
      for
        (x, y) <- sourceTiles
        (nx, ny) <- neighbors(x, y, maxX, maxY)
        neighborTile = state.map.tiles(nx)(ny)
        if neighborTile.terrain != domain
      yield ModifyTerrain(id, nx, ny, domain, cost = 1)

    // Seeding: pick tiles that are Void and not adjacent to domain terrain
    val seedCandidates =
      for
        x <- 0 until maxX
        y <- 0 until maxY
        tile = state.map.tiles(x)(y)
        if tile.terrain == Void
        if neighbors(x, y, maxX, maxY).forall { case (nx, ny) =>
          state.map.tiles(nx)(ny).terrain != domain
        }
      yield ModifyTerrain(id, x, y, domain, cost = 2)

    val shouldSeed = 
      domainSize < 10 || Random.nextDouble() < 0.1

    val chosenSeeds =
      if shouldSeed then Random.shuffle(seedCandidates.toList).take(1)
      else Nil

    val chosenSpreads =
      Random.shuffle(spreadingIntents.toList).take(ipPerTurn - chosenSeeds.map(_.cost).sum)

    val allIntents = (chosenSpreads ++ chosenSeeds).distinct
    val sortedIntents = allIntents.sortBy(_.cost)

    sortedIntents.take(ipPerTurn)
  }

  def ipPerTurn(state: WorldState): Int = ipPerTurn
