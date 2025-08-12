object GodRegistry:
  val ipPerTurn: Map[Domain, Int] = Map(
  Ocean     -> 13,
  Plains    -> 10,
  Forest    -> 10,
  Desert    -> 7,
  Swamp     -> 5,
  Volcano   -> 1,
  Mountain  -> 5,
  Lake      -> 6,
  ).withDefaultValue(0)

  // name formatter helper
  def prettyName(domain: Domain): String =
    domain.toString.capitalize

  // basic PrimordialGods for terrains
  val terrainGods: List[God] =
    terrainDomains.map { t =>
      PrimordialGod(
        idSuffix = t.toString.toLowerCase,
        name = s"${prettyName(t)}ar",
        domain = t,
        ipPerTurn = ipPerTurn(t)
     )
    }

  // base gods for concepts (can’t spread terrain, maybe control actors later)
  val conceptGods: List[God] =
    conceptDomains.map { c =>
      ConceptGod(
        idSuffix = {c.toString.toLowerCase},
        name = s"${prettyName(c)}el",
        domain = c,
      )
    }

  // all starting gods  
  val defaultGods: Vector[God] = (terrainGods ++ conceptGods).toVector
