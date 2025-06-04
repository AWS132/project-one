package bitmanipulation

import chisel3._
import chisel3.util._

abstract class AbstractShuffler(bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bitWidth.W))
    val pattern = Input(UInt((log2Ceil(bitWidth) - 1).W))
    val unshuffle = Input(UInt(1.W))
    val result = Output(UInt(bitWidth.W))
  })
}

class Shuffler(bitWidth: Int) extends AbstractShuffler(bitWidth) {
  val numStages = log2Ceil(bitWidth) - 1
  
  val stages = Wire(Vec(numStages + 1, UInt(bitWidth.W)))
  stages(0) := io.input
  
  for (stageIdx <- 0 until numStages) {
    val swapDistance = 1 << stageIdx
    
    val maskBits = (0 until bitWidth).map { bitPos =>
      val blockSize = swapDistance * 2
      val posInBlock = bitPos % blockSize
      if (posInBlock < swapDistance) 1 else 0
    }
    val mask = maskBits.zipWithIndex.map { case (bit, idx) => 
      if (bit == 1) BigInt(1) << idx else BigInt(0)
    }.reduce(_ | _).U(bitWidth.W)
    
    val patternBit = Mux(io.unshuffle.asBool,
      io.pattern(numStages - 1 - stageIdx), 
      io.pattern(stageIdx)
    )
    
    val currentStage = stages(stageIdx)
    val staticBits = currentStage & ~(mask | (mask << swapDistance))
    
    val upperSwapBits = (currentStage >> swapDistance) & mask
    val lowerSwapBits = currentStage & mask
    
    val swappedResult = staticBits | (lowerSwapBits << swapDistance) | upperSwapBits
    
    stages(stageIdx + 1) := Mux(patternBit, swappedResult, currentStage)
  }
  
  io.result := stages(numStages)
}