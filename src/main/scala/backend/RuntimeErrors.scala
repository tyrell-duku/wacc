package backend

package backend

import InstructionSet._
import frontend.Rules._
import scala.collection.mutable.ListBuffer

object RuntimeErrors {

  def throwRuntimeError(): (Label, List[Instruction]) = {
    (
      Label("p_throw_runtime_error"),
      List[Instruction](
        BranchLink(Label("p_print_string")),
        Mov(R0, ImmInt(-1)),
        BranchLink(Label("exit"))
      )
    )
  }
}
