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

  // Apply an instruction and return the resulting permutation
  def applyInstruction(perm: Map[Int, Int], instr: String, imm: Int): Map[Int, Int] = {
    instr match {
      case "grevi" => grev(imm, perm)
      case "shfli" => shfl(imm, perm)
      case "unshfli" => unshfl(imm, perm)
      case "rori" => ror(imm, perm)
      case _ => perm
    }
  }

  // Calculate score for an instruction based on multiple metrics
  def calculateScore(currentPerm: Map[Int, Int], targetPerm: Map[Int, Int], 
                    instr: String, imm: Int): Double = {
    val resultPerm = applyInstruction(currentPerm, instr, imm)
    
    val currentDistance = hammingDistance(currentPerm, targetPerm)
    val newDistance = hammingDistance(resultPerm, targetPerm)
    val improvement = currentDistance - newDistance
    
    // If no improvement, heavily penalize
    if (improvement <= 0) return -1000.0
    
    // Primary metric: Hamming distance improvement
    var score = improvement * 100.0
    
    // Secondary metric: cycle structure improvement
    val currentCycles = findCycles(currentPerm)
    val newCycles = findCycles(resultPerm)
    val cycleImprovement = currentCycles.map(_.size).sum - newCycles.map(_.size).sum
    score += cycleImprovement * 10.0
    
    // Preference for simpler instructions (tie-breaker)
    val instrCost = instr match {
      case "rori" => 0.1
      case "grevi" => 0.2
      case "shfli" | "unshfli" => 0.3
    }
    score -= instrCost
    
    score
  }

  
  def generateCandidates(): List[(String, Int)] = {
    val candidates = mutable.ListBuffer[(String, Int)]()
    
    for (i <- 1 until 32) {
      candidates += (("rori", i))
    }
    
    val grevPatterns = List(1, 2, 3, 4, 5, 6, 7, 8, 12, 15, 16, 24, 31)
    for (pattern <- grevPatterns) {
      candidates += (("grevi", pattern))
    }
    
    
    val shufflePatterns = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
    for (pattern <- shufflePatterns) {
      if (pattern == 1 || pattern == 2 || pattern == 4 || pattern == 8) {
        candidates += (("shfli", pattern))
      } else {
        candidates += (("shfli", pattern))
        candidates += (("unshfli", pattern))
      }
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

  def buildPermutation(rd: Int, rs1: Int, perm: Map[Int, Int]): List[String] = {
    val instructions = mutable.ListBuffer[String]()
    val instructionHistory = mutable.ListBuffer[(String, Int)]()
    var currentPerm = perm
    val identityPerm = (0 until 32).map(i => (i, i)).toMap 
    

    val allCandidates = generateCandidates()
    var finished = hammingDistance(currentPerm, identityPerm) == 0
    
    while (!finished) {
  
      var bestCandidate: Option[(String, Int)] = None
      var bestScore: Double = 0.0

      for ((instr, imm) <- allCandidates) {
        if (!wouldReverse(instr, imm, instructionHistory.toList)) {
          val score = calculateScore(currentPerm, identityPerm, instr, imm)
          if (score > bestScore) {
            bestScore = score
            bestCandidate = Some((instr, imm))
          }
        }
      }

      bestCandidate match {
        case Some((instr, imm)) =>
          
          currentPerm = applyInstruction(currentPerm, instr, imm)
          val regName = s"x$rd"
          instructions += s"$instr $regName, $regName, $imm"
          instructionHistory += ((instr, imm))
          
          
          finished = hammingDistance(currentPerm, identityPerm) == 0
          
        case None =>
                    finished = true
      }
    }
    
    instructions.reverse.map { instr =>
      val parts = instr.split(" ")
      val instrName = parts(0)
      val reg = parts(1).dropRight(1) // Remove comma
      val regSrc = parts(2).dropRight(1) // Remove comma  
      val imm = parts(3).toInt
      
      instrName match {
        case "grevi" => s"grevi $reg, $regSrc, $imm" 
        case "shfli" => s"unshfli $reg, $regSrc, $imm" 
        case "unshfli" => s"shfli $reg, $regSrc, $imm" 
        case "rori" => s"rori $reg, $regSrc, ${(32 - imm) % 32}" 
        case _ => instr
      }
    }.toList
  }
}