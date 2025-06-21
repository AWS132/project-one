package bitmanipulation

import chisel3._
import chisel3.util._

abstract class AbstractGeneralizedReverser(bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bitWidth.W))
    val pattern = Input(UInt(log2Ceil(bitWidth).W))
    val result = Output(UInt(bitWidth.W))
  })
}

object Util
{
  def createAlternatingString(bitWidth: Int, start: Int, alternateWhen: Int) : UInt =
  {
    var result = BigInt(0)
    var startMut = start
    var counter = 0

    for (i <- 0 until bitWidth) {
      result = result | (BigInt(startMut) << i)
      
      counter += 1
      if (counter == alternateWhen) {
        startMut = if (startMut == 0) 1 else 0
        counter = 0
      }
    }
    result.U(bitWidth.W)
  }
}

class GeneralizedReverser(bitWidth: Int)
    extends AbstractGeneralizedReverser(bitWidth) {
    import Util._

    val count = log2Ceil(bitWidth)
    val stages = Wire(Vec(count+1, UInt(bitWidth.W)))
    stages(0) := io.input

    if(bitWidth == 1){
      io.result := io.input(0);
    }
    for(i <- 0 until count)
    {
      val shiftSize = 1 << i
      val mask1= Util.createAlternatingString(bitWidth, 0, shiftSize)
      val mask0 = Util.createAlternatingString(bitWidth, 1, shiftSize)
      
      when(io.pattern(i)) {
        stages(i + 1) := ((stages(i) & mask0) << shiftSize) | ((stages(i) & mask1) >> shiftSize)
      }.otherwise {
        stages(i + 1) := stages(i)
      }
    }
  io.result := stages(count)

}