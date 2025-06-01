package bitmanipulation

import chisel3._
import chisel3.util._

abstract class AbstractLeadingZerosCounter(bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bitWidth.W))
    val result = Output(UInt(log2Ceil(bitWidth + 1).W))
  })
}

// You may expect bitWidth to be a power of two.
class LeadingZerosCounter(bitWidth: Int)
    extends AbstractLeadingZerosCounter(bitWidth) {
   
   if(bitWidth == 1){
    io.result := ~(io.input(0))
   }
   else{
    val upperZeroes = Module(new LeadingZerosCounter(bitWidth/2))
    upperZeroes.io.input := io.input(bitWidth-1,bitWidth/2) 
    val lowerZeroes = Module(new LeadingZerosCounter(bitWidth/2))
     lowerZeroes.io.input := io.input((bitWidth/2)-1,0)
  
    when(io.input(bitWidth-1 , bitWidth/2) === 0.U){ 
     io.result :=  (lowerZeroes.io.result +& (bitWidth/2).U)(log2Ceil(bitWidth + 1)-1,0) 
      }
      .otherwise{
       io.result := upperZeroes.io.result.asUInt
      }
   }

}
