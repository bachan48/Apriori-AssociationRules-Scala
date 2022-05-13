package basket

import scala.io.Source

object Main extends App {
  
  def lines(filename:String) = {
    Source.fromFile(filename).getLines()
  }

  // process arguments
  if (args.size < 3 || args.size > 4) {
    args.foreach(println)
    throw new  IllegalArgumentException(s"Arguments missing <filename> <delimiter> <relativeSupportThreshold> [limit] ")
  }
  val filename = args(0)
  val delim = args(1)
  if (delim.size != 1) {
    throw new  IllegalArgumentException(s"Delimiter should be one character ([${delim}] provided)")
  }
  val threshold = args(2).toDouble

  val limit = if (args.size == 4) Option(args(3).toInt) else None

  System.err.println(s"Running with parameters: Filename [$filename] Separator [$delim] Minimum relative support threshold [$threshold]."+ {
    limit match {
      case None    => ""
      case Some(i) => s" Print at most ${i} tuples."
    }}+"\n")

  // do the work

  val (count, supportT, items) = aPriori.doFirstPass(threshold, lines(filename), delim)

  println(s"""${count} records, only ${items.size} item${if (items.size > 1) "s" else ""} above support threshold ${supportT} (${threshold}).""" )
  val freqPairs = aPriori.doSecondPass(supportT, items, lines(filename), delim)
  aPriori.doResults(count, items, freqPairs, limit)
}
