package backend

import java.io.{File, FileWriter}

import backend.InstructionSet._

object ARMPrinter {
  val tab = "\t"
  val tab2 = tab * 2

  def execute(
      waccFileName: String,
      data: List[Data],
      instrs: List[(Label, List[Instruction])]
  ): Unit = {
    val assemblyFileName = waccFileName.replaceFirst(".wacc", ".s")
    val file = new File(assemblyFileName)

    if (!file.createNewFile()) {
      // TODO: error
      return
    }

    val fileWriter = new FileWriter(file)
    if (data.nonEmpty) {
      fileWriter.write(tab + ".data\n\n")
      data.foreach(d => printData(d, fileWriter))
      fileWriter.write("\n")
    }

    fileWriter.write(tab + ".text\n\n" + tab + ".global main\n")
    instrs.foreach((x: (Label, List[Instruction])) =>
      printInstrs(x._1, x._2, fileWriter)
    )
    fileWriter.close()
  }

  def printData(data: Data, fileWriter: FileWriter) {
    val Data(Label(label), s) = data
    fileWriter.write(tab + label + "\n")
    fileWriter.write(tab2 + ".word " + s.length() + "\n")
    fileWriter.write(tab2 + ".ascii \"" + s + "\"" + "\n")
  }

  def printInstrs(
      label: Label,
      instr: List[Instruction],
      fileWriter: FileWriter
  ) {
    fileWriter.write(tab + label + ":\n")
    instr.foreach((i: Instruction) => fileWriter.write(tab2 + i + "\n"))
  }
}
