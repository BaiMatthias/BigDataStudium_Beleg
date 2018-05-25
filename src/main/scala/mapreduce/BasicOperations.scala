package mapreduce

object BasicOperations {

  	def mapper[KeyIn ,ValueIn, KeyMOut, ValueMOut](mapFun:((KeyIn,ValueIn))=>List[(KeyMOut,ValueMOut)],
							data:List[(KeyIn, ValueIn)]):List[(KeyMOut,ValueMOut)]={
		
		data.flatMap(mapFun(_))
	}
  	
  	def sorter[KeyMOut,ValueMOut](data:List[(KeyMOut,ValueMOut)]):List[(KeyMOut,List[ValueMOut])]= {
	
		data.groupBy(_._1).mapValues(X=> X.map(_._2)).toList

	}
  	
  	def reducer[KeyMOut,ValueMOut,KeyROut, ValueROut](redFun:((KeyMOut,List[ValueMOut]))=>List[(KeyROut,ValueROut)],
							data:List[(KeyMOut,List[ValueMOut])]):List[(KeyROut, ValueROut)]={
	
		data flatMap (redFun(_))
	}
  	
  	def mapReduce[KeyIn,ValueIn,KeyMOut,ValueMOut, KeyROut, ValueROut](mapFun:((KeyIn,ValueIn))=>List[(KeyMOut, ValueMOut)],
					redFun:(((KeyMOut,List[ValueMOut]))=>List[(KeyROut,ValueROut)]), data:List[(KeyIn,ValueIn)]): List[(KeyROut,ValueROut)] = {
					
		reducer(redFun,sorter(mapper(mapFun, data)))
		
		
	}
  	
}