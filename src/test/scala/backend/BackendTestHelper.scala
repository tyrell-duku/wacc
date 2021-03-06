import java.io.File
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.sys.process._
import backend.{ARMPrinter, CodeGenerator}
import frontend.Rules.Program
import frontend.Semantics._
import frontend._
import parsley.Failure
import parsley.Success
import org.scalatest.funsuite.AnyFunSuite
import scala.util.matching.Regex

object BackendTestHelper extends AnyFunSuite {

  def getFilesFrom(path: String): ListBuffer[File] = {
    var files = ListBuffer.empty[File]
    val dir = new File(path)
    for (f <- dir.listFiles) {
      if (f.isFile()) {
        files += f
      } else {
        files ++= getFilesFrom(f.getPath())
      }
    }
    files
  }

  private def createExecutable(file: File): Unit = {
    val fName = file.getPath()
    s"java -jar target/scala-2.13/wacc.jar $fName".!
    val name = file.getName().replaceAll(".wacc", "")
    s"arm-linux-gnueabi-gcc -o $name -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $name.s".!
    val armFile = new File(file.getName().replaceAll(".wacc", ".s"))
    armFile.delete()
  }

  def createOutputFiles(file: File): (File, File, ProcessBuilder) = {
    createExecutable(file)
    val name = file.getName().replaceAll(".wacc", "")
    val output = new File(s"$name.out")
    val inputString = readFile(getFilePath(file, "wacc_inputs"))
    val command =
      if (inputString.isEmpty()) {
        s"qemu-arm -L /usr/arm-linux-gnueabi/ $name" #> output
      } else {
        (s"printf '$inputString'" #| s"qemu-arm -L /usr/arm-linux-gnueabi/ $name" #> output)
      }
    (file, output, command)
  }

  private def readFile(file: File): String = {
    Source.fromFile(file).getLines().mkString("\n")
  }

  /* Generates the target file in the given directory. */
  private def getFilePath(file: File, dir: String): File = {
    new File(
      file
        .getPath()
        .replace("wacc_examples/valid", dir)
        .replace(".wacc", ".txt")
    )
  }

  private def getExitCodeFile(file: File): File = {
    getFilePath(file, "wacc_exitcodes")
  }

  private def getExpectedFile(file: File): File = {
    getFilePath(file, "wacc_expected")
  }

  def checkExitCode(file: File, command: ProcessBuilder): Boolean = {
    val expExitCode = readFile(getExitCodeFile(file)).toInt
    val exitCode = command.!
    val name = file.getName().replaceAll(".wacc", "")
    val exeFile = new File(name)
    exeFile.delete()
    expExitCode == exitCode
  }

  def checkStdOut(file: File, outputFile: File): Boolean = {
    val expOut = getExpectedFile(file)
    var outLines = readFile(outputFile)
    val expLines = readFile(expOut)
    outputFile.delete()
    // Replace register addresses
    val addr = "0x[0-9a-z]+".r
    outLines = addr replaceAllIn (outLines, "#addr#")
    outLines.equals(expLines)
  }
}
