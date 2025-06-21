package project1.public.bitmanipulation.shuffle

import bitmanipulation.Shuffler
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ShufflerTest2
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {
  behavior of "Shuffler"

  // Helper function to implement the reference C shuffle function
  def shuffle32_stage(src: Long, maskL: Long, maskR: Long, N: Int): Long = {
    val x = src & ~(maskL | maskR)
    x | (((src << N) & maskL) | ((src >> N) & maskR))
  }

  def shfl32(rs1: Long, rs2: Long): Long = {
    var x = rs1
    val shamt = rs2 & 15
    if ((shamt & 8) != 0) x = shuffle32_stage(x, 0x00ff0000L, 0x0000ff00L, 8)
    if ((shamt & 4) != 0) x = shuffle32_stage(x, 0x0f000f00L, 0x00f000f0L, 4)
    if ((shamt & 2) != 0) x = shuffle32_stage(x, 0x30303030L, 0x0c0c0c0cL, 2)
    if ((shamt & 1) != 0) x = shuffle32_stage(x, 0x44444444L, 0x22222222L, 1)
    x & 0xFFFFFFFFL
  }

  def unshfl32(rs1: Long, rs2: Long): Long = {
    var x = rs1
    val shamt = rs2 & 15
    // Reverse order and swap shift directions for unshuffle
    if ((shamt & 1) != 0) x = shuffle32_stage(x, 0x44444444L, 0x22222222L, 1)
    if ((shamt & 2) != 0) x = shuffle32_stage(x,  0x30303030L,0x0c0c0c0cL, 2)
    if ((shamt & 4) != 0) x = shuffle32_stage(x, 0x0f000f00L, 0x00f000f0L, 4)
    if ((shamt & 8) != 0) x = shuffle32_stage(x, 0x00ff0000L,0x0000ff00L,  8)
    x & 0xFFFFFFFFL
  }

  it should "do nothing" in {
    test(new Shuffler(32)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val input = BigInt("AAAAAAAA", 16).U(32.W)
      val unshuffle = 0.U(1.W)
      val pattern = 0.U(4.W)
      val expected = input

      c.io.input.poke(input)
      c.io.pattern.poke(pattern)
      c.io.unshuffle.poke(unshuffle)
      c.io.result.expect(expected)
    }
  }

  it should "shuffle with pattern 1 (swap adjacent bits)" in {
    test(new Shuffler(32)) { c =>
      val input = BigInt("12345678", 16)
      val pattern = 1.U(4.W)
      val unshuffle = 0.U(1.W)
      val expected = shfl32(input.toLong, 1)

      c.io.input.poke(input.U(32.W))
      c.io.pattern.poke(pattern)
      c.io.unshuffle.poke(unshuffle)
      c.io.result.expect(expected.U(32.W))
    }
  }

  it should "shuffle with pattern 2 (swap 2-bit groups)" in {
    test(new Shuffler(32)) { c =>
      val input = BigInt("12345678", 16)
      val pattern = 2.U(4.W)
      val unshuffle = 0.U(1.W)
      val expected = shfl32(input.toLong, 2)

      c.io.input.poke(input.U(32.W))
      c.io.pattern.poke(pattern)
      c.io.unshuffle.poke(unshuffle)
      c.io.result.expect(expected.U(32.W))
    }
  }

  it should "shuffle with pattern 4 (swap 4-bit groups)" in {
    test(new Shuffler(32)) { c =>
      val input = BigInt("12345678", 16)
      val pattern = 4.U(4.W)
      val unshuffle = 0.U(1.W)
      val expected = shfl32(input.toLong, 4)

      c.io.input.poke(input.U(32.W))
      c.io.pattern.poke(pattern)
      c.io.unshuffle.poke(unshuffle)
      c.io.result.expect(expected.U(32.W))
    }
  }

  it should "shuffle with pattern 8 (swap bytes)" in {
    test(new Shuffler(32)) { c =>
      val input = BigInt("12345678", 16)
      val pattern = 8.U(4.W)
      val unshuffle = 0.U(1.W)
      val expected = shfl32(input.toLong, 8)

      c.io.input.poke(input.U(32.W))
      c.io.pattern.poke(pattern)
      c.io.unshuffle.poke(unshuffle)
      c.io.result.expect(expected.U(32.W))
    }
  }

  it should "shuffle with pattern 15 (all stages)" in {
    test(new Shuffler(32)) { c =>
      val input = BigInt("12345678", 16)
      val pattern = 15.U(4.W)
      val unshuffle = 0.U(1.W)
      val expected = shfl32(input.toLong, 15)

      c.io.input.poke(input.U(32.W))
      c.io.pattern.poke(pattern)
      c.io.unshuffle.poke(unshuffle)
      c.io.result.expect(expected.U(32.W))
    }
  }

  it should "shuffle with pattern 3 (swap bits and 2-bit groups)" in {
    test(new Shuffler(32)) { c =>
      val input = BigInt("FEDCBA98", 16)
      val pattern = 3.U(4.W)
      val unshuffle = 0.U(1.W)
      val expected = shfl32(input.toLong, 3)

      c.io.input.poke(input.U(32.W))
      c.io.pattern.poke(pattern)
      c.io.unshuffle.poke(unshuffle)
      c.io.result.expect(expected.U(32.W))
    }
  }

  it should "unshuffle with pattern 1" in {
    test(new Shuffler(32)) { c =>
      val input = BigInt("12345678", 16)
      val pattern = 1.U(4.W)
      val unshuffle = 1.U(1.W)
      val expected = unshfl32(input.toLong, 1)

      c.io.input.poke(input.U(32.W))
      c.io.pattern.poke(pattern)
      c.io.unshuffle.poke(unshuffle)
      c.io.result.expect(expected.U(32.W))
    }
  }

  it should "unshuffle with pattern 8" in {
    test(new Shuffler(32)) { c =>
      val input = BigInt("12345678", 16)
      val pattern = 8.U(4.W)
      val unshuffle = 1.U(1.W)
      val expected = unshfl32(input.toLong, 8)

      c.io.input.poke(input.U(32.W))
      c.io.pattern.poke(pattern)
      c.io.unshuffle.poke(unshuffle)
      c.io.result.expect(expected.U(32.W))
    }
  }

  it should "unshuffle with pattern 15" in {
    test(new Shuffler(32)) { c =>
      val input = BigInt("ABCDEF01", 16)
      val pattern = 15.U(4.W)
      val unshuffle = 1.U(1.W)
      val expected = unshfl32(input.toLong, 15)

      c.io.input.poke(input.U(32.W))
      c.io.pattern.poke(pattern)
      c.io.unshuffle.poke(unshuffle)
      c.io.result.expect(expected.U(32.W))
    }
  }

  it should "be reversible (shuffle then unshuffle)" in {
    test(new Shuffler(32)) { c =>
      val testInputs = Seq(
        BigInt("12345678", 16),
        BigInt("FEDCBA98", 16),
        BigInt("AAAAAAAA", 16),
        BigInt("55555555", 16),
        BigInt("F0F0F0F0", 16),
        BigInt("0F0F0F0F", 16)
      )
      
      val testPatterns = Seq(1, 2, 4, 8, 15, 7, 3, 5)
      
      for (input <- testInputs) {
        for (pattern <- testPatterns) {
          // First shuffle
          c.io.input.poke(input.U(32.W))
          c.io.pattern.poke(pattern.U(4.W))
          c.io.unshuffle.poke(0.U(1.W))
          c.clock.step(1)
          val shuffled = c.io.result.peek()
          
          // Then unshuffle the result
          c.io.input.poke(shuffled)
          c.io.pattern.poke(pattern.U(4.W))
          c.io.unshuffle.poke(1.U(1.W))
          c.clock.step(1)
          val unshuffled = c.io.result.peek()
          
          // Should get back original input
          unshuffled.litValue should equal(input)
        }
      }
    }
  }

  it should "handle edge cases" in {
    test(new Shuffler(32)) { c =>
      // Test with all zeros
      c.io.input.poke(0.U(32.W))
      c.io.pattern.poke(15.U(4.W))
      c.io.unshuffle.poke(0.U(1.W))
      c.io.result.expect(0.U(32.W))
      
      // Test with all ones
      c.io.input.poke(BigInt("FFFFFFFF", 16).U(32.W))
      c.io.pattern.poke(15.U(4.W))
      c.io.unshuffle.poke(0.U(1.W))
      c.io.result.expect(BigInt("FFFFFFFF", 16).U(32.W))
      
      // Test alternating pattern
      c.io.input.poke(BigInt("55555555", 16).U(32.W))
      c.io.pattern.poke(1.U(4.W))
      c.io.unshuffle.poke(0.U(1.W))
      c.io.result.expect(BigInt("33333333", 16).U(32.W))
    }
  }

  it should "match C reference implementation for various inputs" in {
    test(new Shuffler(32)) { c =>
      val testCases = Seq(
        (BigInt("01234567", 16), 1),
        (BigInt("01234567", 16), 2),
        (BigInt("01234567", 16), 4),
        (BigInt("01234567", 16), 8),
        (BigInt("FEDCBA98", 16), 5),
        (BigInt("FEDCBA98", 16), 10),
        (BigInt("FEDCBA98", 16), 15),
        (BigInt("12345678", 16), 7),
        (BigInt("87654321", 16), 9),
        (BigInt("A5A5A5A5", 16), 6)
      )
      
      for ((input, pattern) <- testCases) {
        // Test shuffle
        c.io.input.poke(input.U(32.W))
        c.io.pattern.poke(pattern.U(4.W))
        c.io.unshuffle.poke(0.U(1.W))
        val expected_shuffle = shfl32(input.toLong, pattern.toLong)
        c.io.result.expect(expected_shuffle.U(32.W))
        
        // Test unshuffle
        c.io.input.poke(input.U(32.W))
        c.io.pattern.poke(pattern.U(4.W))
        c.io.unshuffle.poke(1.U(1.W))
        val expected_unshuffle = unshfl32(input.toLong, pattern.toLong)
        c.io.result.expect(expected_unshuffle.U(32.W))
      }
    }
  }
}