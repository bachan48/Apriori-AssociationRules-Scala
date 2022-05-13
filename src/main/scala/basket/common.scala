package object basket {

  type Elem   = String
  type Pair = (Elem, Elem)
  type Items = Map[Elem, Int]
  type FreqPairs = Map[Pair, Int]

}

package basket  {

  case class Result(item :Elem, freqItem: Int, support: Double,
    boughtWith: Elem, freqPair: Int, supportPair: Double,
    confidence: Double, lift:Double)

  object output {
    def printResults(records: List[Result]) = {
      /// set width of item name to minimum of 20 maximum of max size + 2
      // we need to traverse the list to do that
      val (w1, w2) =  records.foldLeft((0,0))((p,r)=> (r.item.size.max(p._1), r.item.size.max(p._2)))
      val maxWidth = 20.max(w1 + 2)
      val maxWidth2 = 20.max(w2 + 2)
      
//      val maxWidth2 =  2 + records.foldLeft(0)((acc,r)=>r.boughtWith.size.max(acc)).max(20)

      val header = s"SupportPair-%-${maxWidth}s- Freq -Support-%-${maxWidth2-4}s- FreqPair-Confidence-     Lift".format("Item", "Bought with")
      println()
      println(header)
      val sep = "-"*(header.size)
      println(sep)
      records.foreach(r => println(s"  %9.6f %-${maxWidth}s %5d %8.6f %-${maxWidth2}s %5d %10.2f %9.3f".format(
        r.supportPair, r.item, r.freqItem, r.support,
        r.boughtWith, r.freqPair,
        r.confidence, r.lift)))
      println(sep)
    }

  }

}


