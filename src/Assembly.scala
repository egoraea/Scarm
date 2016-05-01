
import scala.collection.mutable.{ HashMap, Stack }


class Assembly {
  abstract sealed class ARMLine
  case class Add(dr:Int,sr1:Int,sr2:Int) extends ARMLine
  case class ImmAdd(dr:Int,sr:Int,immVal:Int) extends ARMLine
  case class Sub(dr:Int,sr1:Int,sr2:Int) extends ARMLine
  case class ImmSub(dr:Int,sr:Int,immVal:Int) extends ARMLine
  case class Mul(dr:Int,sr1:Int,sr2:Int) extends ARMLine
  case class ImmMul(dr:Int,sr:Int,immVal:Int) extends ARMLine
  case class Div(dr:Int,sr1:Int,sr2:Int) extends ARMLine
  case class ImmDiv(dr:Int,sr:Int,immVal:Int) extends ARMLine
  case class Eor(dr:Int,sr1:Int,sr2:Int) extends ARMLine
  case class ImmEor(dr:Int,sr:Int,immVal:Int) extends ARMLine
  case class And(dr:Int,sr1:Int,sr2:Int) extends ARMLine
  case class ImmAnd(dr:Int,sr:Int,immVal:Int) extends ARMLine
  case class Orr(dr:Int,sr1:Int,sr2:Int) extends ARMLine
  case class ImmOrr(dr:Int,sr:Int,immVal:Int) extends ARMLine
  case class Move(dr:Int,sr:Int) extends ARMLine
  case class ImmMove(dr:Int,Imm:Int) extends ARMLine
  case class Not(dr:Int,sr:Int) extends ARMLine
  case class Lsl(dr:Int,sr1:Int,sr2:Int) extends ARMLine
  case class ImmLsl(dr:Int,sr:Int,Imm:Int) extends ARMLine
  case class Lsr(dr:Int,sr1:Int,sr2:Int) extends ARMLine
  case class ImmLsr(dr:Int,sr:Int,Imm:Int) extends ARMLine
  case class Asr(dr:Int,sr1:Int,sr2:Int) extends ARMLine
  case class ImmAsr(dr:Int,sr:Int,Imm:Int) extends ARMLine
  case class B(Imm:Int) extends ARMLine
  case class BL(Imm:Int) extends ARMLine
  case class BEQ(Imm:Int) extends ARMLine
  case class BLEQ(Imm:Int) extends ARMLine
  case class BNE(Imm:Int) extends ARMLine
  case class BLNE(Imm:Int) extends ARMLine
  case class BLT(Imm:Int) extends ARMLine
  case class BLLT(Imm:Int) extends ARMLine
  case class BLE(Imm:Int) extends ARMLine
  case class BLLE(Imm:Int) extends ARMLine
  case class BGT(Imm:Int) extends ARMLine
  case class BLGE(Imm:Int) extends ARMLine
  case class BVS(Imm:Int) extends ARMLine
  case class BLVC(Imm:Int) extends ARMLine
  case class BMI(Imm:Int) extends ARMLine
  case class BLMI(Imm:Int) extends ARMLine
  case class BPL(Imm:Int) extends ARMLine
  case class BLPL(Imm:Int) extends ARMLine
  case class Cmp(sr1:Int,sr2:Int) extends ARMLine
  case class ImmCmp(sr:Int,Imm:Int) extends ARMLine
  case class Ldr(dr:Int,Imm:Int) extends ARMLine
  case class Str(sr:Int,Imm:Int) extends ARMLine
  case class Halt() extends ARMLine

  // keep track of which line we are on
  var current: Int = 0

  var lines = new HashMap[Int, ARMLine]
  var stack = new Array[Int](4096)
  var regs = new Array[Int](16)
  var flags = new Array[Int](4)

  val r0:Int = 0
  val r1:Int = 1
  val r2:Int = 2
  val r3:Int = 3
  val r4:Int = 4
  val r5:Int = 5
  val r6:Int = 6
  val r7:Int = 7
  val r8:Int = 8
  val r9:Int = 9
  val r10:Int = 10
  val r11:Int = 11
  val r12:Int = 12
  val r13:Int = 13
  val r14:Int = 14
  val r15:Int = 15
  val R0:Int = 0
  val R1:Int = 1
  val R2:Int = 2
  val R3:Int = 3
  val R4:Int = 4
  val R5:Int = 5
  val R6:Int = 6
  val R7:Int = 7
  val R8:Int = 8
  val R9:Int = 9
  val R10:Int = 10
  val R11:Int = 11
  val R12:Int = 12
  val R13:Int = 13
  val R14:Int = 14
  val R15:Int = 15

  private def printRegs(){
    regs.foreach { println }
  }
  /**
   * runtime evaluator of LOLCODE
   */
  private def gotoLine(line: Int) {


    lines(line) match {
      case Add(dr:Int,sr1:Int,sr2:Int) =>  {
        regs(dr) = regs(sr1) + regs(sr2)
        printRegs()
        gotoLine(line+1)
      }
      case ImmAdd(dr:Int,sr:Int,immVal:Int) => {
        regs(dr) = regs(sr) + immVal
        gotoLine(line+1)
      }


      case Halt() =>
      case _ =>
    }
  }
  def $(num:Int):Int = num
    
  def ADD(dr:Int,sr1:Int,sr2:Int)={
     lines(current) = Add(dr,sr1,sr2)   
     current += 1
  }
  def HALT(){
    lines(current) = Halt()
    gotoLine(0)
  }
  /*def ADD(dr:Int,sr1:Int,imm:(Int)=>Int)={
     lines(current) = ImmAdd(dr,sr1,)   
     current += 1
  }*/







}
