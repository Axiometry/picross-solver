import java.awt.BorderLayout

object PicrossSolver extends App {
  import collection.parallel.ParSeq
  implicit def optionFunc2IterFunc[A, B](f: A => Option[B]): A => Iterable[B] = (x: A) => f(x)

  case class Memo[A,B](f: A => B) extends (A => B) {
    private val cache = collection.mutable.Map.empty[A, B]
    def apply(x: A) = cache.getOrElseUpdate(x, f(x))
  }

  trait Cell
  case object Unknown extends Cell
  case object Yes extends Cell
  case object No extends Cell
  type Line = ParSeq[Cell]
  type Grid = ParSeq[Line]
  case class Board(rowInfo: Seq[Seq[Int]], colInfo: Seq[Seq[Int]]) {
    val rowCount = rowInfo.length
    val colCount = colInfo.length
  }
  object State {
    private val emptyLineMemo = Memo(ParSeq.fill(_: Int)(Unknown))
    def emptyLine(size: Int): Line = emptyLineMemo(size)
    def emptyGrid(rows: Int, cols: Int): Grid = ParSeq.fill(rows)(emptyLine(cols))
    def apply(board: Board): State = State(board: Board,
      emptyGrid(board.rowCount, board.colCount),
      emptyGrid(board.colCount, board.rowCount))
  }
  case class State(board: Board, rows: Grid, cols: Grid) {
    def apply(r: Int, c: Int) = rows(r)(c)
    def update(r: Int, c: Int, cell: Cell): State =
      State(board, rows.updated(r, rows(r).updated(c, cell)), cols.updated(c, cols(c).updated(r, cell)))
  }

  def merge(s: Line, t: Line): Option[Line] = if(s.size == t.size) {
    val v = s.zip(t).flatMap {
      case (Unknown, c) => Some(c)
      case (c, Unknown) => Some(c)
      case (c1, c2) if c1 == c2 => Some(c1)
      case _ => None
    }
    if(v.size == s.size) Some(v) else None
  } else throw new RuntimeException("WAT")

  def intersect(a: Line, b: Line): Line = a.zip(b).map {
    case (c1, c2) if c1 == c2 => c1
    case _ => Unknown
  }
  def intersectUnknown(a: Line, b: Line): Line = a.zip(b).map {
    case (Unknown, Unknown) => Unknown
    case _ => No
  }

  def setRange(line: Line, s: Int, e: Int, c: Cell): Line = line.zipWithIndex.map {
    case (v, i) if i >= s && i < e => c
    case (v, i) => v
  }

  def producePossible(lineLength: Int, info: Seq[Int]): List[Line] = {
    val empty = (State.emptyLine(lineLength), lineLength)
    def rec(i: Int): List[(Line, Int)] = if(i < info.size) {
      rec(i+1).flatMap { case (line, epos) =>
        val v = info(i)
        0 to (epos-v) map { spos =>
          var newLine = setRange(line, spos, spos+v, Yes)
          newLine = setRange(newLine, spos+v, epos, No)
          if(spos > 0)
            newLine = setRange(newLine, spos-1, spos, No)
          (newLine, spos-1)
        }
      }
    } else List(empty)

    rec(0).map(s => setRange(s._1, 0, s._2, No))
  }

  def cellToStr(cell: Cell) = cell match {
    case Yes => "*"
    case No => "x"
    case Unknown => "."
  }
  def printGrid(grid: Grid) = grid.map(line => line.map(cellToStr).mkString).foreach(println)
  def testBoard = Board(
    Seq(Seq(0), Seq(1, 1), Seq(0), Seq(3), Seq(0)),
    Seq(Seq(0), Seq(1, 1), Seq(1), Seq(1, 1), Seq(0))
  )
  def readBoard() = {
    val Array(r, c) = readLine.split(" ").map(_.toInt)
    val rowInfo = Seq.fill(r)(readLine.split(" ").map(_.toInt).toSeq)
    val colInfo = Seq.fill(c)(readLine.split(" ").map(_.toInt).toSeq)
    println("Board loaded")
    Board(rowInfo, colInfo)
  }
  def solved(state: State) = state.rows.forall(!_.exists(_ == Unknown))

  def solve(board: Board): Option[State] = solve(State(board))
  def solveIter(board: Board): Iterator[State] = solveIter(State(board))
  def solve(initialState: State): Option[State] = solveIter(initialState).find(solved)
  def solveIter(initialState: State): Iterator[State] = {
    type Possible = collection.parallel.ParSeq[List[Line]]
    val filterer = (possible: List[Line], line: Line) => possible.filter(p => merge(line, p).isDefined)
    val updater = (possible: List[Line], line: Line) => merge(line, possible.reduce(intersect))

    def rec(state: State, rowPossible: Possible, colPossible: Possible): Option[(State, Possible, Possible)] = {
      val newRowPossible = rowPossible.zip(state.rows).map(filterer.tupled)
      val newColPossible = colPossible.zip(state.cols).map(filterer.tupled)

      val newState = if(newRowPossible.forall(_.nonEmpty) && newColPossible.forall(_.nonEmpty)) {
        val newRows = newRowPossible.zip(state.rows).flatMap(updater.tupled)
        val newCols = newColPossible.zip(state.cols).flatMap(updater.tupled)
        if(newRows.size == state.rows.size && newCols.size == state.cols.size) {
          /*println("New rows:")
          printGrid(newRows)
          println("New cols:")
          printGrid(newCols)
          println()*/
          println("iter")

          val mergedCols = newRows.transpose.zip(newCols).flatMap((merge _).tupled)
          if(mergedCols.size == state.cols.size)
            Some(State(state.board, mergedCols.transpose, mergedCols))
          else None
        } else None
      } else None

      newState match {
        case Some(ns) if state == ns => None
        case Some(ns) => Some((ns, newRowPossible, newColPossible))
        case None => None
      }
    }

    val state = initialState
    val rowPossible = state.board.rowInfo.map(producePossible(state.board.colCount, _))
    val colPossible = state.board.colInfo.map(producePossible(state.board.rowCount, _))

    Iterator.iterate(Option((state, rowPossible, colPossible))) {
      case Some((s, rp, cp)) => rec(s, rp, cp)
      case None => None
    }.takeWhile(_.isDefined).map(_.get._1)
  }

  def showWindow(states: Iterator[State]): Unit = {
    import scala.swing._
    import scala.swing.event._
    import scala.swing.Swing._
    import java.awt
    import javax.swing
    import com.alee.{laf => weblaf}

    trait WebButton extends Button { override lazy val peer = new weblaf.button.WebButton with SuperMixin }
    trait WebLabel extends Label { override lazy val peer = new weblaf.label.WebLabel with SuperMixin }
    trait WebPanel extends Panel { override lazy val peer = new weblaf.panel.WebPanel with SuperMixin }
    trait WebProgressBar extends ProgressBar { override lazy val peer = new weblaf.progressbar.WebProgressBar with SuperMixin }

    def createPanel(state: State) = new WebPanel {
      override def paintComponent(g: awt.Graphics2D): Unit = {
        g.setRenderingHint(awt.RenderingHints.KEY_ANTIALIASING, awt.RenderingHints.VALUE_ANTIALIAS_ON)
        g.setRenderingHint(awt.RenderingHints.KEY_INTERPOLATION, awt.RenderingHints.VALUE_INTERPOLATION_BICUBIC)
        g.setColor(awt.Color.WHITE)
        g.fillRect(0, 0, size.width, size.height)
        g.setColor(awt.Color.BLACK)
        def colX(i: Int) = (i*size.width/state.cols.size.toDouble).toInt
        def rowY(i: Int) = (i*size.height/state.rows.size.toDouble).toInt
        for(i <- 1 until state.cols.size; x = colX(i)) g.drawLine(x, 0, x, size.height)
        for(i <- 1 until state.rows.size; y = rowY(i)) g.drawLine(0, y, size.width, y)

        for(r <- state.rows.indices; c <- state.cols.indices) state(r, c) match {
          case Yes => g.fillRect(colX(c), rowY(r), colX(c + 1) - colX(c), rowY(r + 1) - rowY(r))
          case No =>
            val (x0, x1, y0, y1) = (colX(c), colX(c+1), rowY(r), rowY(r+1))
            def lerp(p0: Int, p1: Int, c: Double) = math.round(p0 + c*(p1-p0)).toInt
            g.drawLine(lerp(x0, x1, 0.2), lerp(y0, y1, 0.2), lerp(x0, x1, 0.8), lerp(y0, y1, 0.8))
            g.drawLine(lerp(x0, x1, 0.2), lerp(y0, y1, 0.8), lerp(x0, x1, 0.8), lerp(y0, y1, 0.2))
          case _ =>
        }
      }
    }

    val initialState = states.next

    lazy val progressFrame = new MainFrame {
      val label = new WebLabel {
        horizontalAlignment = Alignment.Center
      }
      val bar = new WebProgressBar {
        min = 0
        max = initialState.board.rowCount * initialState.board.colCount
        labelPainted = true
      }
      contents = new BorderPanel with WebPanel {
        layoutManager.setHgap(5)
        layoutManager.setVgap(5)
        border = EmptyBorder(5, 5, 5, 5)

        layout(label) = BorderPanel.Position.North
        layout(bar) = BorderPanel.Position.South
      }

      def updateProgress(step: Int, solved: Int): Unit = {
        label.text = s"Solving... (Step $step)"
        bar.value = solved
        bar.label = s"${bar.value}/${bar.max}"
        repaint()
      }
      updateProgress(0, 0)

      title = "Picross"
      pack()
      centerOnScreen()

      override def open(): Unit = {
        super.open()
        peer.toFront()
      }
    }
    onEDTWait(progressFrame.open)

    var completed = 0
    val statePanels = (Iterator.single(initialState) ++ states).map { state =>
      val panel = createPanel(state)
      completed += 1
      onEDT(progressFrame.updateProgress(completed, state.rows.map(_.count(_ != Unknown)).sum))
      panel
    }.toIndexedSeq

    onEDTWait(progressFrame.close)

    var idx = 0

    onEDTWait(com.alee.laf.WebLookAndFeel.install())

    lazy val frame = new MainFrame {
      val pageText = new WebLabel {
        def updateText() =
          text = s"${idx+1}/${statePanels.size}"
        updateText()

        horizontalAlignment = Alignment.Center
      }
      val leftButton = new WebButton {
        text = "<"
        enabled = false
      }
      val rightButton = new WebButton {
        text = ">"
        enabled = statePanels.size > 1
      }
      contents = new BorderPanel with WebPanel {
        val buttonPanel = new GridPanel(1, 3) {
          hGap = 5
          vGap = 5
          contents += leftButton
          contents += pageText
          contents += rightButton
        }

        def updatePanel(): Unit = {
          leftButton.enabled = idx > 0
          rightButton.enabled = idx < statePanels.size-1
          pageText.updateText()
          layout(statePanels(idx)) = BorderPanel.Position.Center
          revalidate()
          repaint()
        }
        updatePanel()

        layoutManager.setHgap(5)
        layoutManager.setVgap(5)
        layout(buttonPanel) = BorderPanel.Position.South

        leftButton.reactions += {
          case ButtonClicked(_) if idx > 0 =>
            idx -= 1
            updatePanel()
        }
        rightButton.reactions += {
          case ButtonClicked(_) if idx < statePanels.size-1 =>
            idx += 1
            updatePanel()
        }
      }

      title = "Picross"
      size = (500, 500)
      preferredSize = size
      centerOnScreen()

      override def open(): Unit = {
        super.open()
        peer.toFront()
      }
    }
    onEDTWait(frame.open)
  }

  showWindow(solveIter(readBoard()))
  /*solve(readBoard()) match {
    case Some(state) => printGrid(state.rows); showWindow(state)
    case None => println("error")
  }*/
}
