package bitmanipulation

import chisel3._
import chisel3.util._

abstract class AbstractFixedRotater(bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bitWidth.W))
    val result = Output(UInt(bitWidth.W))
  })
}

class FixedRotater(bitWidth: Int, shamt: Int)
    extends AbstractFixedRotater(bitWidth) {
      val count = shamt &  (log2Ceil(bitWidth)-1)
      if(count == 0)
        io.result:=io.input
      else
        io.result:= Cat(io.input(count-1,0),io.input(bitWidth-1,count))
}

abstract class AbstractSequentialRotater(bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bitWidth.W))
    val shamt = Input(UInt(log2Ceil(bitWidth).W))
    val start = Input(Bool())
    val done = Output(Bool())
    val result = Output(UInt(bitWidth.W))
  })
}

class SequentialRotater(bitWidth: Int, generator: () => AbstractFixedRotater)
    extends AbstractSequentialRotater(bitWidth) {

  val Rotater = Module(generator())
  val reg1 = RegInit(io.shamt)
  val reg = RegInit(io.input)
     io.result:= reg
      io.done:=false.B
      Rotater.io.input:=reg
  when(io.start){
    when(reg1 === 0.U){
      io.done:= true.B
    }
    .otherwise{
      reg1:=reg1-1.U
      reg:= Rotater.io.result
     
    }
  }

}
