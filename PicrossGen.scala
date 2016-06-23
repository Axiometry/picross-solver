object PicrossGen extends App {

  val (r, c, grid) = if(args.isEmpty) {
    val Array(r, c) = readLine.split(" ").map(_.toInt)
    (r, c, Seq.fill(r)(readLine.toSeq.map(_ == '*')))
  } else {
    val img = javax.imageio.ImageIO.read(new java.io.File(args.mkString(" ")))
    (img.getHeight, img.getWidth, (0 until img.getHeight toSeq).map(y => (0 until img.getWidth toSeq).map(x => math.abs(img.getRGB(x, y)) > 1)))
  }

  def summarize(line: Seq[Boolean]) = line.foldLeft(List(0)) {
    case (acc, false) => 0 :: acc
    case (head :: tail, true) => (head+1) :: tail
  }.filter(_ != 0) match {
    case List() => List(0)
    case list => list
  }

  println(s"$r $c")
  //grid.map(_.map(if(_) '*' else '.').mkString).foreach(println)
  def output(line: Seq[Boolean]) = println(summarize(line).mkString(" "))
  grid.foreach(output)
  grid.transpose.foreach(output)
}
