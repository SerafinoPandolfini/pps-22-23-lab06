package u06lab.solution

import java.util
import java.util.OptionalInt

// Optional!
object ConnectThree extends App:
  val bound = 3
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)
  /**
   * Board:
   * y
   *
   * 3
   * 2
   * 1
   * 0
   *   0 1 2 3 <-- x
   */
  type Board = Seq[Disk]
  type Game = Seq[Board]

  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] =
    board.findLast(d => d.x == x && d.y == y).map(_.player)

  def firstAvailableRow(board: Board, x: Int): Option[Int] =
    if x < 0 || x > bound then return None
    val availableY = for
      y <- 0 to bound
      if find(board, x, y).isEmpty
    yield
      y
    if availableY.isEmpty
    then None
    else Some(availableY.min)

  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 to bound
      y = firstAvailableRow(board, x)
      if y.isDefined
    yield
      Seq(Disk(x, y.get , player)).appendedAll(board)



  def computeAnyGame(player: Player, moves: Int): LazyList[Game] =
    def _computeAnyGame(player: Player, moves: Int): LazyList[(Game, Boolean)] = moves match
      case 0 => LazyList((Seq(List()), false))
      case _ =>
        for
          game <- _computeAnyGame(player.other, moves - 1)
          board <- if game._2 then Seq(List()) else placeAnyDisk(game._1.head, player)
        yield
          if game._2
          then game
          else (Seq(board).appendedAll(game._1), isGameWon(game._1.head, board))
    _computeAnyGame(player, moves).map(_._1)

  def isGameWon(last: Board, board: Board): Boolean =
    val MIN_SIZE = 5
    if board.size < MIN_SIZE then return false
    val lastMove = board.findLast(!last.contains(_)).get
    val x = lastMove.x
    val y = lastMove.y
    val player = lastMove.player
    // vertical
    (last.contains(Disk(x, y - 1, player)) && last.contains(Disk(x, y - 2, player))) ||
    //horizontal
      (last.contains(Disk(x - 1, y, player)) && last.contains(Disk(x - 2, y, player))) ||
      (last.contains(Disk(x - 1, y, player)) && last.contains(Disk(x + 1, y, player))) ||
      (last.contains(Disk(x + 1, y, player)) && last.contains(Disk(x + 2, y, player))) ||
    //lateral ascending
      (last.contains(Disk(x - 2, y - 2, player)) && last.contains(Disk(x - 1, y - 1, player))) ||
      (last.contains(Disk(x - 1, y - 1, player)) && last.contains(Disk(x + 1, y + 1, player))) ||
      (last.contains(Disk(x + 1, y + 1, player)) && last.contains(Disk(x + 2, y + 2, player))) ||
    // lateral descending
      (last.contains(Disk(x + 2, y - 2, player)) && last.contains(Disk(x + 1, y - 1, player))) ||
      (last.contains(Disk(x + 1, y - 1, player)) && last.contains(Disk(x - 1, y + 1, player))) ||
      (last.contains(Disk(x - 1, y + 1, player)) && last.contains(Disk(x - 2, y + 2, player)))


  def printBoards(game: Seq[Board]): Unit =
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()

  // Exercise 1: implement find such that..
  println("EX 1: ")
  println(find(List(Disk(0, 0, X)), 0, 0)) // Some(X)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1)) // Some(O)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1)) // None

  // Exercise 2: implement firstAvailableRow such that..
  println("EX 2: ")
  println(firstAvailableRow(List(), 0)) // Some(0)
  println(firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None

  // Exercise 2: implement placeAnyDisk such that..
  println(placeAnyDisk(List(), X))
  printBoards(placeAnyDisk(List(), X))
  // .... .... .... ....
  // .... .... .... ....
  // .... .... .... ....
  // ...X ..X. .X.. X...
  println(placeAnyDisk(List(Disk(0, 0, O)), X))
  printBoards(placeAnyDisk(List(Disk(0, 0, O)), X))
  // .... .... .... ....
  // .... .... .... ....
  // ...X .... .... ....
  // ...O ..XO .X.O X..O
  println("EX 3: ")
// Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  computeAnyGame(O, 4).foreach { g =>
    printBoards(g)
    println()
  }
//  .... .... .... .... ...O
//  .... .... .... ...X ...X
//  .... .... ...O ...O ...O
//  .... ...X ...X ...X ...X
//
//
// .... .... .... .... O...
// .... .... .... X... X...
// .... .... O... O... O...
// .... X... X... X... X...

  println("EX 4: ")
// Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
  var win = 0
  var total = 0
  computeAnyGame(O, 7).foreach { g =>
    total = total + 1
    if isGameWon(g.tail.head, g.head) then //show only winning games, easier to see the difference in length
      printBoards(g)
      println()
      win = win + 1
  }
  println("total games: "+total + "\nwinning games: " + win)