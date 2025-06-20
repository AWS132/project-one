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
      val count = shamt%bitWidth
      if(count == 0)
        io.result:=io.input
      else
        io.result:= io.input(count-1,0)##io.input(bitWidth-1,count)
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
  val reg1 = RegEnable(io.shamt%bitWidth.U,0.U,io.start)
  val reg = RegEnable(io.input,0.U,io.start)
  val redInp= RegInit(false.B)
     io.result:= reg
      io.done:=false.B
      Rotater.io.input:=reg
      when(io.start && !redInp){
        when(io.shamt === 0.U){
        io.done:=true.B
        io.result:=io.input
        }
        .otherwise{
          redInp :=true.B
        }
      }

  when(redInp){
    when(reg1 === 1.U){
      io.done:= true.B
      io.result := Rotater.io.result
      redInp:=false.B
    }
    .otherwise{
      reg1:=reg1-1.U
      reg:= Rotater.io.result
     
    }
  }

}
