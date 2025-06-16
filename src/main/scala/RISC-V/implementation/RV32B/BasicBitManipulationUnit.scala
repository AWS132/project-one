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
  

  switch(io.instr_type)
  {
    is(RISCV_TYPE.clz){
      io.valid := true
      leadingZerosCounter.io.input := io_reg.reg_read_data1
      io_reg.reg_rd := io.instr(11,7)
      io_reg.reg_write_en := true
      io_reg.reg_write_data := leadingZerosCounter.io.result


    }
    is(RISCV_TYPE.ctz){

    }
    is(RISCV_TYPE.cpop){

    }
    is(RISCV_TYPE.min){

    }
    is(RISCV_TYPE.minu){

    }
    is(RISCV_TYPE.max){

    }
    is(RISCV_TYPE.maxu){

    }
  }

}
