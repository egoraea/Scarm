


object LolTest_Visible extends Assembly {
  def main(args: Array[String]): Unit = {
    regs(r1) = 5
    ADD (r0, r1, r2)
    HALT
  }
}
