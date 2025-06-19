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

object Util {
  // Generate mask for GREV operation at stage i
  // For stage i, we need to create a mask that selects every other group of 2^i bits
  def createGrevMask(bitWidth: Int, stage: Int, selectLower: Boolean): UInt = {
    val groupSize = 1 << stage
    val pairSize = groupSize * 2
    var result = BigInt(0)
    
    var pos = 0
    while (pos < bitWidth) {
      // For each pair of groups, select either the lower or upper group
      val groupStart = if (selectLower) pos else pos + groupSize
      val groupEnd = scala.math.min(groupStart + groupSize, bitWidth)
      
      for (bit <- groupStart until groupEnd) {
        if (bit < bitWidth) {
          result = result | (BigInt(1) << bit)
        }
      }
      
      pos += pairSize
    }
    
    result.U(bitWidth.W)
  }
}

class GeneralizedReverser(bitWidth: Int) extends AbstractGeneralizedReverser(bitWidth) {
  import Util._
  
  val count = log2Ceil(bitWidth)
  val stages = Wire(Vec(count + 1, UInt(bitWidth.W)))
  stages(0) := io.input
  
  if (bitWidth == 1) {
    io.result := io.input
  } else {
    for (i <- 0 until count) {
      val shiftSize = 1 << i
      
      when(io.pattern(i)) {
        // Generate masks for this stage
        val lowerMask = Util.createGrevMask(bitWidth, i, true)  // Select lower groups
        val upperMask = Util.createGrevMask(bitWidth, i, false) // Select upper groups
        
        stages(i + 1) := ((stages(i) & lowerMask) << shiftSize) | ((stages(i) & upperMask) >> shiftSize)
      }.otherwise {
        stages(i + 1) := stages(i)
      }
    }
    
    io.result := stages(count)
  }
}