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
  def createAlternatingString(bitWidth: Int, start: BigInt, alternateWhen: Int) : UInt =
  {
    var result = BigInt(0);
    var startMut = start;
    for(_ <- 0 until bitWidth / alternateWhen)
    {
      for(j <- 0 until alternateWhen)
      {
        result = (result | start) << 1;
      }
      startMut = if (startMut == 0) 1 else 0;
    }

    result.U(bitWidth.U)
  }
}

class GeneralizedReverser(bitWidth: Int)
    extends AbstractGeneralizedReverser(bitWidth) {
    import Util._

    val count = log2Ceil(bitWidth);
    val stages = Wire(Vec(count+1, UInt(bitWidth.W)));
    stages(0) := io.input;

    if(bitWidth == 1){
      io.result := io.input(0); 
    }
    for(i <- 0 until count)
    {
      val shiftSize = 1 << i;
       when(io.pattern(i))
       {
        stages(i+1) := (stages(i) & Util.createAlternatingString(bitWidth, 0, i +1) << shiftSize) | (stages(i) & Util.createAlternatingString(bitWidth, 1, i +1) >> shiftSize);
       }
       .otherwise{
        stages(i+1) := stages(i); 
       }
    }
  io.result := stages(count);

}
