package basket

/*
 Student Name: Bachan Ghimire
 netlink id: bachan48 (at) uvic (dot) ca
 StudentID: V00996378
 */

object aPriori {

  def doFirstPass(threshold: Double, lines: Iterator[String], delim:String): (Int, Int, Items) = {
    val AllItemsWithCounter = lines.foldLeft(Map[String, Int](), 0)
    {(accumulatorTuple: (Items, Int), transaction:String) =>
    {
      val currentMap: Items = accumulatorTuple._1
      val currentCounter: Int = accumulatorTuple._2
      val itemsInTransaction = transaction.split(delim).map(item=>{
        val cleanItem = item.replace("\'","").trim
        if(currentMap.contains(cleanItem)){
          val count = currentMap(cleanItem)
          (cleanItem -> (count+1))
        }
        else (cleanItem -> 1)
      }).toMap
      val newMap= currentMap++itemsInTransaction
      val newCounter = currentCounter + 1
      (newMap, newCounter)
    }
    }
    val totalInputStream = AllItemsWithCounter._2
    val mimSupportThreshold = (threshold*totalInputStream).toInt
    val allItemsWithFrequency = AllItemsWithCounter._1
    val frequentItems = allItemsWithFrequency.filter(item=>item._2>=mimSupportThreshold)
    (totalInputStream, mimSupportThreshold, frequentItems)
  }

  def doSecondPass(supportT: Int, items: Items, lines: Iterator[String], delim:String): FreqPairs ={
    val allPairs = lines.foldLeft(Map[(Pair), Int]())
    {(accumulatorPairMap: FreqPairs, transaction:String) =>
    {
      val itemsInTransaction = transaction.split(delim).map(x=>x.replace("\"","".trim)).toList
      val filteredItems = itemsInTransaction.filter(item => {
        items.contains(item)
      })
      val crossProduct: FreqPairs = filteredItems.flatMap(x => filteredItems.map(y => {
        if (accumulatorPairMap.contains(x, y)) {
          val count = accumulatorPairMap(x, y)
          ((x, y), count + 1)
        }
        else ((x, y), 1)
      })).toMap
      val distinctPairsWithCounter = crossProduct.filter(x=>x._1._1 != x._1._2)
      val newAccumulatorPairMap : FreqPairs = accumulatorPairMap ++ distinctPairsWithCounter
      newAccumulatorPairMap
    }
    }
    val freqPairs : FreqPairs = allPairs.filter(pairs=>pairs._2 >= supportT)
    freqPairs
  }

  def doResults(count:Int, items: Items, freqPairs: FreqPairs, limit: Option[Int]) = {
    val recordsCount = freqPairs.size
    val printCount = if(limit!=None)limit.get else recordsCount
    val results : List[Result] = freqPairs.map(pair=>{
      val item_x = pair._1._1
      val item_y = pair._1._2
      val freq_x = items.get(item_x).get
      val support_x = precision(freq_x.toDouble / count.toDouble)
      val freq_y = items.get(item_y).get
      val support_y = precision(freq_y.toDouble / count.toDouble)
      val freq_x_y = pair._2
      val support_x_y = precision(freq_x_y.toDouble / count.toDouble)
      val confidence_x_y = precision(support_x_y / support_y)
      val lift_x_y = precision(support_x_y/(support_x * support_y))
      Result(item_x, freq_x, support_x, item_y, freq_x_y, support_x_y, confidence_x_y, lift_x_y)
    }).toList
    val sortedResult = results.sortBy(x=>(-x.confidence, -x.lift, -x.freqItem, x.item, x.boughtWith))
    val printResult = sortedResult.take(printCount)
    output.printResults(printResult)
    print(s"Found $recordsCount records." + {if (printCount < recordsCount) " Printed only " + printCount +"." else ""})
  }

  def precision(v: Double):Double = {
    val digits = 7
    Math.floor(v * Math.pow(10,digits))/Math.pow(10, digits)
  }

}