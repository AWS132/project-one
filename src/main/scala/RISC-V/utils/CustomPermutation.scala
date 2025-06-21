package RISCV.utils

import scala.collection.mutable

object PermBuilder {

  /** This function takes a mapping for the permutation and returns the list of
    * necessary instructions to implement the permutation.
    *
    * You may assume that the map encodes a valid permutation, i.e., that every
    * destination bit is associated with a unique source bit.
    *
    * You may only write to the register rd.
    *
    * @param rd
    *   The destination register
    * @param rs1
    *   The source register
    * @param perm
    *   A map from representing the permutation, mapping destination bit
    *   positions to source bit positions.
    * @return
    *   A list of strings representing the instructions to implement the
    *   permutation e.g. List("grev x1, x2, 0x01", "grev x1, x2, 0x02", ...)
    */
  
  def shfl(shamt: Int, perm: Map[Int, Int]): Map[Int, Int] = {
    var x = perm
    if ((shamt & 8) != 0) {
      x = x.map { case (ind, _) =>
        val a = if (ind >= 8 && ind < 16) x(ind + 8)
                else if (ind >= 16 && ind < 24) x(ind - 8)
                else x(ind)
        (ind, a)
      }
    }
    if ((shamt & 4) != 0) {
      x = x.map { case (ind, _) =>
        val a = if (ind % 16 > 3 && ind % 16 < 8) x(ind + 4)
                else if (ind % 16 > 7 && ind % 16 < 12) x(ind - 4)
                else x(ind)
        (ind, a)
      }
    }
    if ((shamt & 2) != 0) {
      x = x.map { case (ind, _) =>
        val a = if (ind % 8 == 2 || ind % 8 == 3) x(ind + 2)
                else if (ind % 8 == 4 || ind % 8 == 5) x(ind - 2)
                else x(ind)
        (ind, a)
      }
    }
    if ((shamt & 1) != 0) {
      x = x.map { case (ind, _) =>
        val a = if (ind % 4 == 1) x(ind + 1)
                else if (ind % 4 == 2) x(ind - 1)
                else x(ind)
        (ind, a)
      }
    }
    x
  }

  def unshfl(shamt: Int, perm: Map[Int, Int]): Map[Int, Int] = {
    var x = perm
    if ((shamt & 1) != 0) {
      x = x.map { case (ind, _) =>
        val a = if (ind % 4 == 1) x(ind + 1)
                else if (ind % 4 == 2) x(ind - 1)
                else x(ind)
        (ind, a)
      }
    }
    if ((shamt & 2) != 0) {
      x = x.map { case (ind, _) =>
        val a = if (ind % 8 == 2 || ind % 8 == 3) x(ind + 2)
                else if (ind % 8 == 4 || ind % 8 == 5) x(ind - 2)
                else x(ind)
        (ind, a)
      }
    }
    if ((shamt & 4) != 0) {
      x = x.map { case (ind, _) =>
        val a = if (ind % 16 > 3 && ind % 16 < 8) x(ind + 4)
                else if (ind % 16 > 7 && ind % 16 < 12) x(ind - 4)
                else x(ind)
        (ind, a)
      }
    }
    if ((shamt & 8) != 0) {
      x = x.map { case (ind, _) =>
        val a = if (ind >= 8 && ind < 16) x(ind + 8)
                else if (ind >= 16 && ind < 24) x(ind - 8)
                else x(ind)
        (ind, a)
      }
    }
    x
  }

  def ror(shamt: Int, perm: Map[Int, Int]): Map[Int, Int] = {
    perm.map { case (ind, _) =>
      val a = perm((ind - shamt + 32) % 32)
      (ind, a)
    }
  }

  def grev(shamt: Int, perm: Map[Int, Int]): Map[Int, Int] = {
    var x = perm
    if ((shamt & 1) != 0) {
      x = x.map { case (ind, _) =>
        val a = if (ind % 2 == 0) x(ind + 1) else x(ind - 1)
        (ind, a)
      }
    }
    if ((shamt & 2) != 0) {
      x = x.map { case (ind, _) =>
        val a = if ((ind % 4) < 2) x(ind + 2) else x(ind - 2)
        (ind, a)
      }
    }
    if ((shamt & 4) != 0) {
      x = x.map { case (ind, _) =>
        val a = if ((ind % 8) < 4) x(ind + 4) else x(ind - 4)
        (ind, a)
      }
    }
    if ((shamt & 8) != 0) {
      x = x.map { case (ind, _) =>
        val a = if ((ind % 16) < 8) x(ind + 8) else x(ind - 8)
        (ind, a)
      }
    }
    if ((shamt & 16) != 0) {
      x = x.map { case (ind, _) =>
        val a = if (ind < 16) x(ind + 16) else x(ind - 16)
        (ind, a)
      }
    }
    x
  }

  def hammingDistance(current: Map[Int, Int], target: Map[Int, Int]): Int = {
    (0 until 32).count(i => current(i) != target(i))
  }
  
  private def inversionDistance(cur: Map[Int, Int], tar: Map[Int, Int]): Int = {
    val targetInverse = tar.map(_.swap)
    val relative = cur.toArray.sortBy(_._1).map { case (pos, value) => 
      targetInverse(value) 
    }
    var inversions = 0
    for (i <- relative.indices; j <- (i + 1) until relative.length) {
      if (relative(i) > relative(j)) inversions += 1
    }
    inversions
  }
  
  private def bitPatternDistance(perm1: Map[Int, Int], perm2: Map[Int, Int]): Int = {
    perm1.map { case (destBit, srcBit) =>
      val targetDestBit = perm2.find(_._2 == srcBit).map(_._1)
      targetDestBit match {
        case Some(target) => math.abs(destBit - target)
        case None => 32
      }
    }.sum
  }
  
  private def weightedDistance(perm1: Map[Int, Int], perm2: Map[Int, Int]): Double = {
    val hamming = hammingDistance(perm1, perm2)
    val inversion = inversionDistance(perm1, perm2)
    val bitPattern = bitPatternDistance(perm1, perm2)
    
    0.4 * hamming + 0.3 * inversion + 0.3 * bitPattern
  }

  
  private def calculateScore(current: Map[Int, Int], target: Map[Int, Int], instr: String, imm: Int): Double = {
    val afterApply = applyInstruction(current, instr, imm)
    val currentDistance = weightedDistance(current, target)
    val newDistance = weightedDistance(afterApply, target)
    
  
    val improvement = currentDistance - newDistance
    
    
    val bonus = instr match {
      case "rori" if imm == 1 || imm == 31 => 0.1  // Common rotations
      case "grevi" if imm == 1 || imm == 2 || imm == 4 || imm == 8 || imm == 16 => 0.1  
      case "shfli" | "unshfli" if imm == 1 || imm == 2 || imm == 4 || imm == 8 => 0.1  
      case _ => 0.0
    }
    
    improvement + bonus
  }

  def findCycles(perm: Map[Int, Int]): List[List[Int]] = {
    val visited = mutable.Set[Int]()
    val cycles = mutable.ListBuffer[List[Int]]()
    
    for (start <- 0 until 32 if !visited(start)) {
      val cycle = mutable.ListBuffer[Int]()
      var current = start
      while (!visited(current)) {
        visited += current
        cycle += current
        current = perm(current)
      }
      if (cycle.size > 1) cycles += cycle.toList
    }
    cycles.toList
  }

  def applyInstruction(perm: Map[Int, Int], instr: String, imm: Int): Map[Int, Int] = {
    instr match {
      case "grevi" => grev(imm, perm)
      case "shfli" => shfl(imm, perm)
      case "unshfli" => unshfl(imm, perm)
      case "rori" => ror(imm, perm)
      case _ => perm
    }
  }
  
  def generateCandidates(): List[(String, Int)] = {
    val candidates = mutable.ListBuffer[(String, Int)]()
    
    // Rotation candidates
    for (i <- 1 until 32) {
      candidates += (("rori", i))
    }
  
    val grevPatterns = List(1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14, 15, 16,17,18,19,20,21,22,23, 24,25,26,27,28,29,30, 31)
    for (pattern <- grevPatterns) {
      candidates += (("grevi", pattern))
    }
    
    // Shuffle candidates
    val shufflePatterns = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
    for (pattern <- shufflePatterns) {
      candidates += (("shfli", pattern))
      candidates += (("unshfli", pattern))
    }
    
    candidates.toList
  }

  def wouldReverse(newInstr: String, newImm: Int, previousInstructions: List[(String, Int)]): Boolean = {
    previousInstructions.lastOption match {
      case Some((lastInstr, lastImm)) =>
        (newInstr, lastInstr) match {
          case ("shfli", "unshfli") | ("unshfli", "shfli") => newImm == lastImm
          case ("rori", "rori") => (newImm + lastImm) % 32 == 0
          case ("grevi", "grevi") => newImm == lastImm
          case _ => false
        }
      case None => false
    }
  }

  
  private def checkSimplePatterns(rd:Int,rs:Int,perm: Map[Int, Int]): Option[List[String]] = {

    if ((0 until 32).forall(i => perm(i) == i)) {
      return Some(List())
    }
    
    // Check for simple rotation
    val rotation = (0 until 32).find(r => 
      (0 until 32).forall(i => perm(i) == (i + r) % 32)
    )
    if (rotation.isDefined) {
      val r = rotation.get
      if (r != 0) {
        return Some(List(s"rori x$rd, x$rs, ${32 - r}"))
      }
    }
    
    // Check for simple bit reversal patterns
    if ((0 until 32).forall(i => perm(i) == 31 - i)) {
      return Some(List(s"grevi $rd, $rs, 31"))
    }
    
    None
  }

  def buildPermutation(rd: Int, rs1: Int, perm: Map[Int, Int]): List[String] = {
    // Quick check for simple patterns
    checkSimplePatterns(rd,rs1,perm) match {
      case Some(instructions) => 
        return instructions
      case None => // Continue with general algorithm
    }
    
    val instructions = mutable.ListBuffer[String]()
    val instructionHistory = mutable.ListBuffer[(String, Int)]()
    var currentPerm = (0 until 32).map(i => (i, i)).toMap
    val targetPerm = perm  // This is the target we want to achieve

    val allCandidates = generateCandidates()
    var maxIterations = 50  // Prevent infinite loops
    var iteration = 0
    
    while (hammingDistance(currentPerm, targetPerm) > 0 && iteration < maxIterations) {
      iteration += 1
      
      var bestCandidate: Option[(String, Int)] = None
      var bestScore: Double = -1000.0

      for ((instr, imm) <- allCandidates) {
        if (!wouldReverse(instr, imm, instructionHistory.toList)) {
          val score = calculateScore(currentPerm, targetPerm, instr, imm)
          if (score > bestScore) {
            bestScore = score
            bestCandidate = Some((instr, imm))
          }
        }
      }

      bestCandidate match {
        case Some((instr, imm)) if bestScore > -100.0 => // Only accept reasonable improvements
          currentPerm = applyInstruction(currentPerm, instr, imm)
          val regName = s"x$rd"
          instructions += s"$instr $regName, $regName, $imm"
          instructionHistory += ((instr, imm))
          
        case _ =>
          // No good candidate found, break to avoid infinite loop
          iteration = maxIterations
      }
    }
    
    instructions.toList
  }
}