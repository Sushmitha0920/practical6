import scala.io.StdIn.readLine

object StudentRecords {
    def getStudentInfo(): (String, Int, Int, Double, Char) = {
        val name = readLine("Enter student name : ")
        val marks = readLine("Enter student marks : ").toInt
        val totalMarks = readLine("Enter total possible marks : ").toInt

        val (isValid, errorMessage) = validateInput(name, marks, totalMarks)

        if (isValid) {
            val percentage = (marks.toDouble / totalMarks) * 100
            val grade = if (percentage >= 90) 'A'
                else if (percentage >= 75) 'B'
                else if (percentage >= 50) 'C'
                else 'D'

            (name, marks, totalMarks, percentage, grade)

        }else{
            println(errorMessage.getOrElse("Invalid input."))
            getStudentInfo()
        }
    }

    def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
        val (name, marks, totalMarks, percentage, grade) = record

        println(s"Name: $name")
        println(s"Marks: $marks / $totalMarks")
        println(s"Percentage: ${percentage}%.2f".format(percentage))
        println(s"Grade: $grade")
    }

    def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.isEmpty) {
      (false, Some("Name cannot be empty."))
    } else if (marks < 0 || totalMarks <= 0) {
      (false, Some("Marks and total marks must be positive."))
    } else if (marks > totalMarks) {
      (false, Some("Marks cannot exceed total marks."))
    } else {
      (true, None)
    }
  }

  def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
    var studentInfo: (String, Int, Int, Double, Char) = null
    var valid = false

    while (!valid) {
      studentInfo = getStudentInfo()
      val (name, marks, totalMarks, percentage, grade) = studentInfo
      val (isValid, errorMessage) = validateInput(name, marks, totalMarks)
      if (isValid) {
        valid = true
      } else {
        println(errorMessage.getOrElse("Invalid input. Please try again."))
      }
    }

    studentInfo
  }

  def main(args: Array[String]): Unit = {
    val studentInfo = getStudentInfoWithRetry()
    printStudentRecord(studentInfo)
  }
}
