package backend

import java.io.{File, FileWriter}

import backend.InstructionSet.Instruction

object ARMPrinter {
  def execute(waccFileName: String, instrs: List[Instruction]): Unit = {
    val assemblyFileName = waccFileName.replaceFirst(".wacc", ".s")
    val file = new File(assemblyFileName)

    if (!file.createNewFile()) {
      return
    }

    val fileWriter = new FileWriter(file)
    fileWriter.close()
  }
}
