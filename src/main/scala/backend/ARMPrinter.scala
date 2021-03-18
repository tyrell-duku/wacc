package backend

import java.io.{File, FileWriter}
import backend.IR.InstructionSet._
import scala.util.matching.Regex

object ARMPrinter {
  private val tab = "\t"
  private val tab2 = tab * 2

  /* Convert the IR returned by CodeGenerator into assembly code.
     Writes assembly to .s file of same name. */
  def printARM(
      waccFileName: String,
      data: List[Data],
      instrs: List[(Label, List[Instruction])]
  ): Unit = {
    // Creates new file of same name for .s
    val assemblyFileName = waccFileName.replaceFirst(".wacc", ".s")
    var file = new File(assemblyFileName)
    // If file present delete and create new
    if (!file.createNewFile()) {
      file.delete()
      file = new File(assemblyFileName)
    }
    val fileWriter = new FileWriter(file)
    // Write all data msgs to file
    if (data.nonEmpty) {
      fileWriter.write(tab + ".data\n\n")
      data.foreach(d => printData(d, fileWriter))
      fileWriter.write("\n")
    }
    fileWriter.write(tab + ".text\n\n" + tab + ".global main\n")
    // Write functions to file
    instrs.foreach((x: (Label, List[Instruction])) =>
      printInstrs(x._1, x._2, fileWriter)
    )
    fileWriter.close()
  }

  /* Writes data msg with directives .word & .ascii to file with correct
     tab indentation. */
  private def printData(data: Data, fileWriter: FileWriter): Unit = {
    val Data(Label(label), s) = data
    fileWriter.write(tab + label + ":\n")
    fileWriter.write(tab2 + ".word " + getStringSize(s) + "\n")
    fileWriter.write(tab2 + ".ascii \"" + s + "\"" + "\n")
  }

  /* Return correct string size, accounting for escape characters. */
  private def getStringSize(s: String): Int = {
    val escapeChar = """\\[0btnfr"'\\]""".r
    s.length - escapeChar.findAllIn(s).length
  }

  /* Writes function to file with correct indentation for label & correct
     tab indentation for all corresponding instructions. */
  private def printInstrs(
      label: Label,
      instr: List[Instruction],
      fileWriter: FileWriter
  ): Unit = {
    fileWriter.write(tab + label + ":\n")
    instr.foreach((i: Instruction) => fileWriter.write(tab2 + i + "\n"))
  }
}
