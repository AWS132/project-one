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
    val swapDistanceRev = 1 << stageIdx
    val swapDistance = 1 << (numStages - stageIdx - 1)
    
    val maskBitsL = (0 until bitWidth).map { bitPos =>
      val blockSize = swapDistance * 4
      val posInBlock = bitPos % blockSize
      if (posInBlock >= swapDistance && posInBlock < 2*swapDistance) 1 else 0
    }.zipWithIndex.map { case (bit, idx) => 
      if (bit == 1) BigInt(1) << idx else BigInt(0)
    }.reduce(_ | _).U(bitWidth.W)
    
    val maskBitsR = (0 until bitWidth).map { bitPos =>
      val blockSize = swapDistance * 4
      val posInBlock = bitPos % blockSize
      if (posInBlock >= 2*swapDistance && posInBlock < 3*swapDistance) 1 else 0
    }.zipWithIndex.map { case (bit, idx) => 
      if (bit == 1) BigInt(1) << idx else BigInt(0)
    }.reduce(_ | _).U(bitWidth.W)
   
    val maskBitsLRev = (0 until bitWidth).map { bitPos =>
      val blockSize = swapDistanceRev * 4
      val posInBlock = bitPos % blockSize
      if (posInBlock >= swapDistanceRev && posInBlock < 2*swapDistanceRev) 1 else 0
    }.zipWithIndex.map { case (bit, idx) => 
      if (bit == 1) BigInt(1) << idx else BigInt(0)
    }.reduce(_ | _).U(bitWidth.W)
     
    val maskBitsRRev = (0 until bitWidth).map { bitPos =>
      val blockSize = swapDistanceRev * 4
      val posInBlock = bitPos % blockSize
      if (posInBlock >= 2*swapDistanceRev && posInBlock < 3*swapDistanceRev) 1 else 0
    }.zipWithIndex.map { case (bit, idx) => 
      if (bit == 1) BigInt(1) << idx else BigInt(0)
    }.reduce(_ | _).U(bitWidth.W)
    
    when(io.unshuffle === 1.U) {
      when(io.pattern(stageIdx)) {
        stages(stageIdx+1) := (stages(stageIdx) & ~(maskBitsLRev | maskBitsRRev)) | 
                              (((stages(stageIdx) >> swapDistanceRev) & maskBitsLRev) | 
                               ((stages(stageIdx) << swapDistanceRev) & maskBitsRRev))
      }
      .otherwise {
        stages(stageIdx+1) := stages(stageIdx) 
      }
    }
    .otherwise {
      when(io.pattern(numStages - stageIdx - 1)) {
        stages(stageIdx+1) := (stages(stageIdx) & ~(maskBitsL | maskBitsR)) | 
                              (((stages(stageIdx) >> swapDistance) & maskBitsL) | 
                               ((stages(stageIdx) << swapDistance) & maskBitsR))
      }
      .otherwise {
        stages(stageIdx+1) := stages(stageIdx)
      }
    }
  }
  
  io.result := stages(numStages)
}