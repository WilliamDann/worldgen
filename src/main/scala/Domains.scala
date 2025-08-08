sealed trait Domain
trait Terrain extends Domain
trait Concept extends Domain

// terrain types
case object Void      extends Terrain
case object Forest    extends Terrain
case object Desert    extends Terrain
case object Hills     extends Terrain
case object Mountain  extends Terrain
case object Tundra    extends Terrain
case object Plains    extends Terrain
case object Canyon    extends Terrain
case object Plateau   extends Terrain
case object Swamp     extends Terrain
case object Glacier   extends Terrain
case object Lake      extends Terrain
case object Marsh     extends Terrain
case object Ocean     extends Terrain
case object River     extends Terrain
case object Volcano   extends Terrain

// concept types
case object War       extends Concept
case object Peace     extends Concept
case object Harvest   extends Concept
case object Famine    extends Concept
case object Knowledge extends Concept

// todo change this
enum RegionTrait:
  case Fertile, Holy, WarTorn
enum ActorTrait:
  case Heroic, Corrupt, Wise, Ambitious


// lists
val terrainDomains: List[Terrain] = List(
  Forest, Desert, Hills, Mountain, Tundra, Plains, Canyon, Plateau,
  Swamp, Glacier, Lake, Marsh, Ocean, River, Volcano
)

val conceptDomains: List[Concept] = List(
  War, Peace, Harvest, Famine, Knowledge
)
