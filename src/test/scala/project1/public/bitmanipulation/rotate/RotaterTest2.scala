package project1.public.bitmanipulation.rotate

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import bitmanipulation._

class SeqRotateBitTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {
  behavior of "SequentialRotater"

  "SequentialRotater" should "rotate right by 0" in {
    test(new SequentialRotater(32, () => new FixedRotater(32, 1))) { c =>
      c.io.input.poke(1.U(32.W))
      c.io.shamt.poke(0.U(5.W))
      c.io.start.poke(true.B)
      
      // Should complete immediately in the same cycle - no clock step needed
      c.io.result.expect(1.U(32.W))
      c.io.done.expect(true.B)
      
      // Verify it completes in exactly 0 cycles (immediate)
      var cycles = 0
      c.io.start.poke(false.B)
      c.clock.step(1)
      cycles += 1
      
      c.io.start.poke(true.B)
      // Should be done in same cycle
      c.io.done.expect(true.B)
      println(s"Completed 0-bit rotation in 0 cycles (immediate)")
    }
  }

  "SequentialRotater" should "rotate right by 1" in {
    test(new SequentialRotater(32, () => new FixedRotater(32, 1))) { c =>
      c.io.input.poke("h80000001".U(32.W))  // MSB and LSB set
      c.io.shamt.poke(1.U(5.W))
      c.io.start.poke(true.B)
      
      // Count cycles until completion
      var cycles = 0
      while(!c.io.done.peek().litToBoolean) {
        c.clock.step(1)
        cycles += 1
      }
      
      c.io.result.expect("hc0000000".U(32.W))  // LSB moved to MSB-1 position
      c.io.done.expect(true.B)
      
      // Verify it took exactly 1 cycle
      cycles should be (1)
      println(s"Completed 1-bit rotation in exactly $cycles cycle(s)")
    }
  }

  "SequentialRotater" should "rotate right by 4 with 8-bit" in {
    test(new SequentialRotater(8, () => new FixedRotater(8, 1))) { c =>
      c.io.input.poke("h0f".U(8.W))  // 00001111
      c.io.shamt.poke(4.U(3.W))
      c.io.start.poke(true.B)
      
      // Count cycles until completion
      var cycles = 0
      while(!c.io.done.peek().litToBoolean) {
        c.clock.step(1)
        cycles += 1
      }
      
      c.io.result.expect("hf0".U(8.W))  // 11110000
      c.io.done.expect(true.B)
      
      // Verify it took exactly 4 cycles
      cycles should be (4)
      println(s"Completed 4-bit rotation in exactly $cycles cycle(s)")
    }
  }

  "SequentialRotater" should "handle immediate completion for 0 shift" in {
    test(new SequentialRotater(8, () => new FixedRotater(8, 1))) { c =>
      c.io.input.poke("hab".U(8.W))
      c.io.shamt.poke(0.U(3.W))
      c.io.start.poke(false.B)
      
      // Should not be done initially
      c.io.done.expect(false.B)
      
      c.io.start.poke(true.B)
      
      // Should be done immediately in same cycle for 0 shift - no clock step
      c.io.result.expect("hab".U(8.W))
      c.io.done.expect(true.B)
      
      // Verify 0 cycles were needed (immediate completion)
      println("Completed 0-bit rotation immediately (0 cycles)")
    }
  }

  "SequentialRotater" should "rotate right by 7 with 16-bit" in {
    test(new SequentialRotater(16, () => new FixedRotater(16, 1))) { c =>
      c.io.input.poke("h1234".U(16.W))
      c.io.shamt.poke(7.U(4.W))
      c.io.start.poke(true.B)
      
      // Count cycles until completion
      var cycles = 0
      while(!c.io.done.peek().litToBoolean) {
        c.clock.step(1)
        cycles += 1
      }
      
      c.io.done.expect(true.B)
      
      // Verify it took exactly 7 cycles
      cycles should be (7)
      println(s"Completed 7-bit rotation in exactly $cycles cycle(s)")
    }
  }

  "SequentialRotater" should "rotate right by maximum shift amount" in {
    test(new SequentialRotater(8, () => new FixedRotater(8, 1))) { c =>
      c.io.input.poke("h81".U(8.W))  // 10000001
      c.io.shamt.poke(7.U(3.W))  // Maximum shift for 8-bit (2^3 - 1 = 7)
      c.io.start.poke(true.B)
      
      // Count cycles until completion
      var cycles = 0
      while(!c.io.done.peek().litToBoolean) {
        c.clock.step(1)
        cycles += 1
      }
      
      c.io.result.expect("h03".U(8.W))  // Should be 00000011 after 7 right rotations
      c.io.done.expect(true.B)
      
      // Verify it took exactly 7 cycles
      cycles should be (7)
      println(s"Completed maximum 7-bit rotation in exactly $cycles cycle(s)")
    }
  }
}