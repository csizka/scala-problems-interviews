package com.rockthejvm.solutions.various.flatmap.either

sealed trait LookupError

case class NotFoundError(keyAsString: String) extends LookupError
case class CustomError(errMsg: String) extends LookupError

object Exercises {
  type Name = String
  type Year = Int
  type Title = String

  // an Either can contain different types of errors (a LookupError can be either a NotFoundError or a CustomError)
  type Result[T] = Either[LookupError, T]

  // NOTE: you can turn an Option into an Either with .toRight(err) where 'err' is the value of Left if the Option is None
  def lookup[K, V](m: Map[K, V], k: K): Result[V] =
    m.get(k).toRight(NotFoundError(k.toString))

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

  // For a given actor and year, return the list of films they acted in (if it exists, else Left)
  def getFilms(actor: Name, year: Year): Result[List[Title]] =
    lookup(actors, actor).flatMap(years => lookup(years, year))

  // Define + use toRight on the result of find and give a descriptive custom error message (so that the assertions pass)
  // For a given actor, director and year, return a film they worked on together (if it exists, else None)
  def getCommonFilm(actor: Name, director: Name, year: Year): Result[Title] = {
    getFilms(actor, year).flatMap { filmography =>
      filmography
        .find(films.get(_) == Some(director))
        .toRight(CustomError(s"No common films were found for actor '$actor' and director '$director' in the year '$year'."))
    }
  }

  def getCommonFilmSugared(actor: Name, director: Name, year: Year): Result[Title] =  for {
    filmList <- getFilms(actor, year)
    title <- filmList
      .find(films.get(_) == Some(director))
      .toRight(CustomError(s"No common films were found for actor '$actor' and director '$director' in the year '$year'."))
  } yield title


   //Define + use lookup + use toRight on the result of find and give a descriptive custom error message (so that the assertions pass)
   //For a given actor and film, return the year when the actor acted in the film (if it exists, else None)
  def getYearDesugared(actor: Name, film: Title): Result[Year] = {
    lookup(actors, actor).flatMap{ filmography =>
      filmography
        .find{ case (year, filmsInYear) => filmsInYear.contains(film) }
        .toRight(CustomError(s"'$actor' did not act in '$film"))
        .map { case (year, filmsInYear) => year }
    }
  }

  def getYear(actor: Name, film: Title): Result[Year] = for {
    filmography <- lookup(actors, actor)
    (year, filmsThatYear) <- filmography.find{ case (year, filmsThatYear) =>
      filmsThatYear.contains(film)}
      .toRight(CustomError(s"'$actor' did not act in '$film"))
  } yield year

}

object ExercisesTests extends App {
  import Exercises._

  // NOTE: we define some special assertions to check the correct error messages
  def shouldFail[T](computation: Result[T])(errMsgPred: LookupError => Boolean = _ => true): Unit =
    computation match {
      case Right(_) => throw new Exception("This should have failed")
      case Left(err) => assert(errMsgPred(err))
    }

  def shouldFailWithNotFoundError[T](computation: Result[T], keyAsString: Title): Unit =
    shouldFail(computation) {
      case NotFoundError(k) => k == keyAsString
      case CustomError(_) => throw new Exception("Should have failed with NotFoundError, not CustomError")
    }

  def shouldFailWithCustomError[T](computation: Result[T], errMsgSubstring: String): Unit =
    shouldFail(computation) {
      case NotFoundError(_) => throw new Exception("Should have failed with CustomError, not NotFoundError")
      case CustomError(errMsg) => errMsg.contains(errMsgSubstring)
    }

  // NOTE: Remember, the flatMap of Either fails on the __first__ error. Checkout the assertions below, they all have
  // the error message of the first failing subcomputation

  assert(getFilms("John", 2000) == Right(List(flatLands, monadsEverywhere)))

  shouldFailWithNotFoundError(
    getFilms("John", 2001),
    "2001"
  )

  shouldFailWithNotFoundError(
    getFilms("Johny", 2001),
    "Johny"
  )
  assert(getFilms("Terry", 2003) == Right(List(startrekWars)))


  assert(getCommonFilm("John", "Huenos Dios", 2000) == Right(flatLands))
  assert(getCommonFilm("John", "Huenos Dios", 2000) == getCommonFilmSugared("John", "Huenos Dios", 2000))
  assert(getCommonFilm("Terry", "Lukacs Gyorgy", 2003) == Right(startrekWars))
  shouldFailWithCustomError(
    getCommonFilm("Terry", "Huenos Dios", 2003),
    "No common films were found for actor 'Terry' and director 'Huenos Dios' in the year '2003'."
  )
  shouldFailWithCustomError(
    getCommonFilmSugared("Terry", "Huenos Dios", 2003),
    "No common films were found for actor 'Terry' and director 'Huenos Dios' in the year '2003'."
  )
  shouldFailWithNotFoundError(
    getCommonFilm("Terrz", "Lukacs Gyorgy", 2003),
    "Terrz"
  )

  assert(getYear("John", theBirth2) == Right(1996))
  shouldFailWithNotFoundError(
    getYear("john", theBirth2),
    "john"
  )
  shouldFailWithCustomError(
    getYear("Terry", theBirth2),
    "'Terry' did not act in 'The Birth II: Monad Messiah"
  )
}

