package RISCV.implementation.RV32B

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._
import bitmanipulation.AbstractGeneralizedReverser
import bitmanipulation.AbstractShuffler
import bitmanipulation.AbstractSequentialRotater
import RISCV.model.STALL_REASON.NO_STALL
import RISCV.model.STALL_REASON.EXECUTION_UNIT

class BitPermutationUnit(
    genGeneralizedReverser: () => AbstractGeneralizedReverser,
    genShuffler: () => AbstractShuffler,
    genRotater: () => AbstractSequentialRotater
) extends AbstractExecutionUnit(InstructionSets.BitPerm) {

  io.misa := "b01__0000__0_00000_00000_00000_00000_00010".U

  val generalizedReverser = Module(genGeneralizedReverser())
  val shuffler = Module(genShuffler())
  val rotater = Module(genRotater())

  io.stall := STALL_REASON.NO_STALL

  io_data <> DontCare
  io_reg <> DontCare
  io_pc <> DontCare
  io_reset <> DontCare
  io_trap <> DontCare

  generalizedReverser.io <> DontCare
  shuffler.io <> DontCare
  rotater.io <> DontCare

    
  io_reg.reg_rs1 := 0.U
  io_reg.reg_rs2 := 0.U
  io_reg.reg_rd := 0.U
  io_reg.reg_write_en := false.B
  io_reg.reg_write_data := 0.U
  io_pc.pc_we := false.B
  
  switch(io.instr_type)
  {
    is(RISCV_TYPE.grev)
    {
      io.valid := true.B
      io_reg.reg_rd := io.instr(11,7)
      io_reg.reg_rs1 := io.instr(19,15)
      io_reg.reg_rs2 := io.instr(24,20)
      io_reg.reg_write_en := true.B
      
      io_pc.pc_we := true.B
      io_pc.pc_wdata := io_pc.pc + 4.U

      generalizedReverser.io.input := io_reg.reg_read_data1
      generalizedReverser.io.pattern := io_reg.reg_read_data2
      io_reg.reg_write_data := generalizedReverser.io.result

    }
    is(RISCV_TYPE.grevi)
    {
      io.valid := true.B
      io_reg.reg_rd := io.instr(11,7)
      io_reg.reg_rs1 := io.instr(19,15)
      io_reg.reg_write_en := true.B

      io_pc.pc_we := true.B
      io_pc.pc_wdata := io_pc.pc + 4.U

      generalizedReverser.io.input := io_reg.reg_read_data1
      generalizedReverser.io.pattern := io.instr(26,20)
      io_reg.reg_write_data := generalizedReverser.io.result
    }
    is(RISCV_TYPE.shfl)
    {
      io.valid := true.B
      io_reg.reg_rd := io.instr(11,7)
      io_reg.reg_rs1 := io.instr(19,15)
      io_reg.reg_rs2 := io.instr(24,20)
      io_reg.reg_write_en := true.B
      
      io_pc.pc_we := true.B
      io_pc.pc_wdata := io_pc.pc + 4.U

      shuffler.io.input := io_reg.reg_read_data1
      shuffler.io.pattern := io_reg.reg_read_data2
      shuffler.io.unshuffle := 0.U
      io_reg.reg_write_data := shuffler.io.result
    }
    is(RISCV_TYPE.shfli)
    {
      io.valid := true.B
      io_reg.reg_rd := io.instr(11,7)
      io_reg.reg_rs1 := io.instr(19,15)
      io_reg.reg_rs2 := io.instr(24,20)
      io_reg.reg_write_en := true.B
      
      io_pc.pc_we := true.B
      io_pc.pc_wdata := io_pc.pc + 4.U

      shuffler.io.input := io_reg.reg_read_data1
      shuffler.io.pattern := io.instr(25,20)
      shuffler.io.unshuffle := 0.U
      io_reg.reg_write_data := shuffler.io.result
    }
    is(RISCV_TYPE.unshfl)
    {
      io.valid := true.B
      io_reg.reg_rd := io.instr(11,7)
      io_reg.reg_rs1 := io.instr(19,15)
      io_reg.reg_rs2 := io.instr(24,20)
      io_reg.reg_write_en := true.B
      
      io_pc.pc_we := true.B
      io_pc.pc_wdata := io_pc.pc + 4.U

      shuffler.io.input := io_reg.reg_read_data1
      shuffler.io.pattern := io_reg.reg_read_data2
      shuffler.io.unshuffle := 1.U
      io_reg.reg_write_data := shuffler.io.result
    }
    is(RISCV_TYPE.unshfli)
    {
      io.valid := true.B
      io_reg.reg_rd := io.instr(11,7)
      io_reg.reg_rs1 := io.instr(19,15)
      io_reg.reg_rs2 := io.instr(24,20)
      io_reg.reg_write_en := true.B
      
      io_pc.pc_we := true.B
      io_pc.pc_wdata := io_pc.pc + 4.U

      shuffler.io.input := io_reg.reg_read_data1
      shuffler.io.pattern := io.instr(25,20)
      shuffler.io.unshuffle := 1.U
      io_reg.reg_write_data := shuffler.io.result
    }
    is(RISCV_TYPE.ror)
    {
      io.valid := true.B
      io_reg.reg_rd := io.instr(11,7)
      io_reg.reg_rs1 := io.instr(19,15)
      io_reg.reg_rs2 := io.instr(24,20)

      io.stall := STALL_REASON.EXECUTION_UNIT
      
      rotater.io.input := io_reg.reg_read_data1
      rotater.io.shamt := io_reg.reg_read_data2(4,0)
      rotater.io.start := true.B

      when(rotater.io.done)
      {
        io_pc.pc_we := true.B
        io_pc.pc_wdata := io_pc.pc + 4.U
        io.stall := STALL_REASON.NO_STALL
        io_reg.reg_write_en := true.B
        io_reg.reg_write_data := rotater.io.result
      }
    }
    is(RISCV_TYPE.rori)
    {
      io.valid := true.B
      io_reg.reg_rd := io.instr(11,7)
      io_reg.reg_rs1 := io.instr(19,15)
      io_reg.reg_rs2 := io.instr(24,20)

      io.stall := STALL_REASON.EXECUTION_UNIT
      rotater.io.input := io_reg.reg_read_data1
      rotater.io.shamt := io.instr(26,20)
      rotater.io.start := true.B

      when(rotater.io.done)
      {
        io_pc.pc_we := true.B
        io_pc.pc_wdata := io_pc.pc + 4.U
        io.stall := STALL_REASON.NO_STALL
        io_reg.reg_write_en := true.B
        io_reg.reg_write_data := shuffler.io.result
      }
    }
    is(RISCV_TYPE.rol)
    {
      io.valid := true.B
      io_reg.reg_rd := io.instr(11,7)
      io_reg.reg_rs1 := io.instr(19,15)
      io_reg.reg_rs2 := io.instr(24,20)

      io.stall := STALL_REASON.EXECUTION_UNIT
      
      rotater.io.input := 32.U - io_reg.reg_read_data1
      rotater.io.shamt := io_reg.reg_read_data2
      rotater.io.start := true.B

      when(rotater.io.done)
      {
        io_pc.pc_we := true.B
        io_pc.pc_wdata := io_pc.pc + 4.U
        io.stall := STALL_REASON.NO_STALL
        io_reg.reg_write_en := true.B
        io_reg.reg_write_data := shuffler.io.result
      }
    }
  }

}
