package RISCV.implementation.RV32B

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._
import bitmanipulation.AbstractLeadingZerosCounter

class BasicBitManipulationUnit(
    genLeadingZerosCounter: () => AbstractLeadingZerosCounter
) extends AbstractExecutionUnit(InstructionSets.BasicBit) {
  io.misa := "b01__0000__0_00000_00000_00000_00000_00010".U

  val leadingZerosCounter = Module(genLeadingZerosCounter())

  io.stall := STALL_REASON.NO_STALL

  io_data <> DontCare
  io_reg <> DontCare
  io_pc <> DontCare
  io_reset <> DontCare
  io_trap <> DontCare

  leadingZerosCounter.io <> DontCare
  
  io_reg.reg_rs1 := 0.U
  io_reg.reg_rs2 := 0.U
  io_reg.reg_rd := 0.U
  io_reg.reg_write_en := false.B
  io_reg.reg_write_data := 0.U
  io_pc.pc_we := false.B
  
  switch(io.instr_type)
  {
    is(RISCV_TYPE.clz){
      io.valid := true.B
      io_reg.reg_rd := io.instr(11,7)
      io_reg.reg_rs1 := io.instr(19,15)
      io_reg.reg_write_en := true.B
      
      io_pc.pc_we := true.B
      io_pc.pc_wdata := io_pc.pc + 4.U

      leadingZerosCounter.io.input := io_reg.reg_read_data1
      io_reg.reg_write_data := leadingZerosCounter.io.result
    }
    is(RISCV_TYPE.ctz){
      io.valid := true.B
      io_reg.reg_rd := io.instr(11,7)
      io_reg.reg_rs1 := io.instr(19,15)
      io_reg.reg_write_en := true.B

      io_pc.pc_we := true.B
      io_pc.pc_wdata := io_pc.pc + 4.U

      val reversedBitString = Reverse(io_reg.reg_read_data1)
      leadingZerosCounter.io.input := reversedBitString
      io_reg.reg_write_data := leadingZerosCounter.io.result
    }
    is(RISCV_TYPE.cpop){
      io.valid := true.B
      io_reg.reg_rd := io.instr(11,7)
      io_reg.reg_rs1 := io.instr(19,15)
      io_reg.reg_write_en := true.B

      io_pc.pc_we := true.B
      io_pc.pc_wdata := io_pc.pc + 4.U

      io_reg.reg_write_data := PopCount(io_reg.reg_read_data1)
    }
    is(RISCV_TYPE.min)
    {
      io.valid := true.B
      io_reg.reg_rd := io.instr(11,7)
      io_reg.reg_rs1 := io.instr(19,15)
      io_reg.reg_rs2 := io.instr(24,20)
      io_reg.reg_write_en := true.B

      io_pc.pc_we := true.B
      io_pc.pc_wdata := io_pc.pc + 4.U
    
      when(io_reg.reg_read_data1.asSInt < io_reg.reg_read_data2.asSInt)
      {
        io_reg.reg_write_data := io_reg.reg_read_data1
      }.
      otherwise{
        io_reg.reg_write_data := io_reg.reg_read_data2
      }
    }
    is(RISCV_TYPE.max)
    {
      io.valid := true.B
      io_reg.reg_rd := io.instr(11,7)
      io_reg.reg_rs1 := io.instr(19,15)
      io_reg.reg_rs2 := io.instr(24,20)
      io_reg.reg_write_en := true.B

      io_pc.pc_we := true.B
      io_pc.pc_wdata := io_pc.pc + 4.U

      when(io_reg.reg_read_data1.asSInt > io_reg.reg_read_data2.asSInt)
      {
        io_reg.reg_write_data := io_reg.reg_read_data1
      }.
      otherwise{
        io_reg.reg_write_data := io_reg.reg_read_data2
      }
    }
    is(RISCV_TYPE.maxu)
    {
      io.valid := true.B
      io_reg.reg_rd := io.instr(11,7)
      io_reg.reg_rs1 := io.instr(19,15)
      io_reg.reg_rs2 := io.instr(24,20)
      io_reg.reg_write_en := true.B

      io_pc.pc_we := true.B
      io_pc.pc_wdata := io_pc.pc + 4.U

      when(io_reg.reg_read_data1 > io_reg.reg_read_data2)
      {
        io_reg.reg_write_data := io_reg.reg_read_data1
      }.
      otherwise{
        io_reg.reg_write_data := io_reg.reg_read_data2
      }
    } 
    is(RISCV_TYPE.minu)
    {
        io.valid := true.B
      io_reg.reg_rd := io.instr(11,7)
      io_reg.reg_rs1 := io.instr(19,15)
      io_reg.reg_rs2 := io.instr(24,20)
      io_reg.reg_write_en := true.B

      io_pc.pc_we := true.B
      io_pc.pc_wdata := io_pc.pc + 4.U

      when(io_reg.reg_read_data1 < io_reg.reg_read_data2)
      {
        io_reg.reg_write_data := io_reg.reg_read_data1
      }.
      otherwise{
        io_reg.reg_write_data := io_reg.reg_read_data2
      }
    } 
  }


}
