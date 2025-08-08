object GodRegistry:
  val ipPerTurn: Map[Domain, Int] = Map(
  Ocean     -> 13,
  Plains    -> 10,
  Forest    -> 10,
  Desert    -> 7,
  Hills     -> 5,
  Swamp     -> 7,
  Volcano   -> 2,
  Mountain  -> 5,
  Lake      -> 6,
  ).withDefaultValue(0)

  // name formatter helper
  def prettyName(domain: Domain): String =
    domain.toString.capitalize

  // basic PrimordialGods for terrains
  val terrainGods: List[PrimordialGod] =
    terrainDomains.map { t =>
      PrimordialGod(
        id = s"god_of_${t.toString.toLowerCase}",
        name = s"${prettyName(t)}ar",
        domain = t,
        ipPerTurn = ipPerTurn(t)
     )
    }

  // base gods for concepts (can’t spread terrain, maybe control actors later)
  val conceptGods: List[BaseGod] =
    conceptDomains.map { c =>
      BaseGod(
        id = s"god_of_${c.toString.toLowerCase}",
        name = s"${prettyName(c)}el",
        domain = c,
        ipPerTurnFn = _ => 5,
        ai = _ => Nil  // no behavior yet
      )
    }

  // all starting gods  
  val defaultGods: Vector[God] = (terrainGods ++ conceptGods).toVector
