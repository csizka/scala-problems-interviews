package com.rockthejvm.solutions.various.flatmap.option

object Exercises {
  type Name = String
  type Year = Int
  type Title = String

  val flatLands = "Map of the flat lands"
  val monadsEverywhere = "Monads, monads everywhere"
  val theBirth = "The Birth: Creative Chaos"
  val theBirth2 = "The Birth II: Monad Messiah"
  val startrekWars = "Startrek Wars: Return of the Sci-fi subreddit trolls"

  // actor name -> year -> list of movies they acted in
  val actors: Map[Name, Map[Year, List[Title]]] = Map(
    "John" -> Map(
      2000 -> List(
        flatLands,
        monadsEverywhere,
      ),
      1996 -> List(
        theBirth2
      ),
      1993 -> List(
        theBirth
      ),
    ),
    "Terry" -> Map(
      2003 -> List(
        startrekWars,
      ),
      2000 -> List(
        flatLands,
      )
    )
  )

  // film title -> director name
  val films: Map[Title, Name] = Map(
    flatLands -> "Huenos Dios",
    monadsEverywhere -> "John",
    startrekWars -> "Lukacs Gyorgy",
    theBirth -> "Mutika",
    theBirth2 -> "Anya"
  )

  // TODO: For a given actor and year, return the list of films they acted in (if it exists, else None)
  def getFilms(actor: Name, year: Year): Option[List[Title]] = ???

  // TODO: For a given actor and year, return the first film they acted in that contains the given substring (if it exists, else None)
  // HINT (at the end of the line):                                                                                                                                                                                                                  try using flatMap + find
  def getFilmContainingSubstring(actor: Name, year: Year, substring: String): Option[Title] = ???

  // TODO: For a given actor, director and year, return a film they worked on together (if it exists, else None)
  // HINT (at the end of the line):                                                                                                                                                                                                                  try using find
  def getCommonFilm(actor: Name, director: Name, year: Year): Option[Title] = ???

  // TODO: For a given actor, director and year, return a film they worked on together (if it exists AND contains the given substring, else None)
  def getCommonFilmIfContaisSubstring(actor: Name, director: Name, year: Year, substring: String): Option[Title] = ???

  // TODO: For a given actor and film, return the year when the actor acted in the film (if it exists, else None)
  def getYear(actor: Name, film: Title): Option[Year] = ???

  // TODO (*): for a given actor, return one of their earliest film's director (if exists, else None - if multiple "earliest" films exist, choose the first one in the list)
  // HINT (at the end of the line):                                                                                                                                                                                                                  minByOption for getting the earliest film + lookup the titles in the films Map (getting a List[Option[director]], then use find on this list with _.isDefined (you'll get an Option[Option], but you can use flatMap twice (or use bind [<-] twice in a for)
  def getEarliestDirector(actor: Name): Option[Name] = ???
}

object ExercisesTests extends App {
  import Exercises._

  assert(getFilms("John", 2000) == Some(List(flatLands, monadsEverywhere)))
  assert(getFilms("John", 2001) == None)
  assert(getFilms("Johny", 2000) == None)
  assert(getFilms("Terry", 2003) == Some(List(startrekWars)))

  assert(getFilmContainingSubstring("John", 2000, "everywhere") == Some(monadsEverywhere))
  assert(getFilmContainingSubstring("John", 2001, "foo") == None)
  assert(getFilmContainingSubstring("Johny", 2000, "foo") == None)
  assert(getFilmContainingSubstring("Terry", 2003, "War") == Some(startrekWars))
  assert(getFilmContainingSubstring("Terry", 2003, "Peace") == None)

  assert(getCommonFilm("John", "Huenos Dios", 2000) == Some(flatLands))
  assert(getCommonFilm("Terry", "Lukacs Gyorgy", 2003) == Some(startrekWars))
  assert(getCommonFilm("Terry", "Huenos Dios", 2003) == None)
  assert(getCommonFilm("Terrz", "Lukacs Gyorgy", 2003) == None)

  assert(getCommonFilmIfContaisSubstring("John", "Huenos Dios", 2000, "flat") == Some(flatLands))
  assert(getCommonFilmIfContaisSubstring("John", "Huenos Dios", 2000, "asd") == None)
  assert(getCommonFilmIfContaisSubstring("Terry", "Lukacs Gyorgy", 2003, "Star") == Some(startrekWars))
  assert(getCommonFilmIfContaisSubstring("Terry", "Lukacs Gyorgy", 2003, "asd") == None)
  assert(getCommonFilmIfContaisSubstring("Terry", "Huenos Dios", 2003, "foo") == None)
  assert(getCommonFilmIfContaisSubstring("Terrz", "Lukacs Gyorgy", 2003, "foo") == None)

  assert(getYear("John", theBirth2) == Some(1996))
  assert(getYear("john", theBirth2) == None)
  assert(getYear("Terry", theBirth2) == None)

  assert(getEarliestDirector("John") == Some("Mutika"))
  assert(getEarliestDirector("Terry") == Some("Huenos Dios"))
  assert(getEarliestDirector("Mr Nobody") == None)


}
