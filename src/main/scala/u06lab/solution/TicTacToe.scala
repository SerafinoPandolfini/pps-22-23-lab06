package u06lab.solution

import u06lab.solution.TicTacToe.Player.*

object TicTacToe extends App :
  val bound = 2

  enum Player:
    case X, O

    def other: Player = this match
      case X => O
      case _ => X

  case class Cell(x: Int, y: Int, player: Player)

  type Board = Seq[Cell]
  type Game = Seq[Board]

  def find(board: Board, x: Int, y: Int): Option[Player] =
    board.findLast(d => d.x == x && d.y == y).map(_.player)

  def placeAnyCell(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 to bound
      y <- 0 to bound
      if find(board, x, y).isEmpty
    yield
      Seq(Cell(x, y , player)).appendedAll(board)

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] =
    def _computeAnyGame(player: Player, moves: Int): LazyList[(Game, Boolean)] = moves match
      case 0 => LazyList((Seq(List()), false))
      case _ =>
        for
          game <- _computeAnyGame(player.other, moves - 1)
          board <- if game._2 then Seq(List()) else placeAnyCell(game._1.head, player)
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
    (last.contains(Cell(x, y - 1, player)) && last.contains(Cell(x, y - 2, player))) ||
      (last.contains(Cell(x, y - 1, player)) && last.contains(Cell(x, y + 1, player))) ||
      (last.contains(Cell(x, y + 1, player)) && last.contains(Cell(x, y + 2, player))) ||
    //horizontal
      (last.contains(Cell(x - 1, y, player)) && last.contains(Cell(x - 2, y, player))) ||
      (last.contains(Cell(x - 1, y, player)) && last.contains(Cell(x + 1, y, player))) ||
      (last.contains(Cell(x + 1, y, player)) && last.contains(Cell(x + 2, y, player))) ||
    //lateral ascending
      (last.contains(Cell(x - 2, y - 2, player)) && last.contains(Cell(x - 1, y - 1, player))) ||
      (last.contains(Cell(x - 1, y - 1, player)) && last.contains(Cell(x + 1, y + 1, player))) ||
      (last.contains(Cell(x + 1, y + 1, player)) && last.contains(Cell(x + 2, y + 2, player))) ||
    // lateral descending
      (last.contains(Cell(x + 2, y - 2, player)) && last.contains(Cell(x + 1, y - 1, player))) ||
      (last.contains(Cell(x + 1, y - 1, player)) && last.contains(Cell(x - 1, y + 1, player))) ||
      (last.contains(Cell(x - 1, y + 1, player)) && last.contains(Cell(x - 2, y + 2, player)))

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

  //find
  println("\nFind:")
  println(find(List(Cell(0, 0, X)), 0, 0)) // Some(X)
  println(find(List(Cell(0, 0, X), Cell(0, 1, O), Cell(0, 2, X)), 0, 1)) // Some(O)
  println(find(List(Cell(0, 0, X), Cell(0, 1, O), Cell(0, 2, X)), 1, 1)) // None
  //place
  println("\nPlaceAnyCell:")
  printBoards(placeAnyCell(List(), X))
  println()
  printBoards(placeAnyCell(List(Cell(0, 0, O)), X))
  //computeAnyGame
  println("\nComputeAnyGame:")

  var win = 0
  var total = 0
  computeAnyGame(O, 7).foreach { g =>
    total = total + 1
    if isGameWon(g.tail.head, g.head) then //show only winning games, easier to see the difference in length
      printBoards(g)
      println()
      win = win + 1
  }
  println("total games: " + total + "\nwinning games: " + win)