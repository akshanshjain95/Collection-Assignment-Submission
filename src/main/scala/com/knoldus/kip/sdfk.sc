import com.knoldus.kip.RamDatabase.{marksList, studentList}
import com.knoldus.kip.models.{Gender, Marks, ScoreCard}

import scala.collection.immutable.Map

val listOfScoreCard = for{
  s <- studentList
  studentId = s.id
  listOfMarks = marksList.filter(_.studentId == studentId)
  marksMap = computeMarksMap(listOfMarks)
  perc = (marksMap.foldLeft(0.0)(_+_._2))/5
} yield (new ScoreCard(studentId, marksMap, perc.toFloat))

def computeMarksMap(markses: List[Marks]): Map[Long, Float] = {
  val mapp = markses.groupBy(_.subjectId)
  val mapp1 = mapp.keys.toList
  val mapp3 = mapp1.map(_.toLong)
  val mapp4 = mapp.map(_._2.map(_.marksObtained)).flatten
  mapp3.zip(mapp4).toMap
}

 val a = studentList.map(_.name)

val x = a.zip(listOfScoreCard)

val y = x.groupBy(_._1).mapValues(_.map(_._2))

val mapp = for {
  l <- y
} yield {
  if(l._2.length == 1)
    (l._1 , l._2(0))
  else
    (l._1, l._2)
}





























def generateScorecards: Map[String, AnyRef] = {

  val listOfNames = studentList.map(_.name)


  def computeMarksMap(markses: List[Marks]): Map[Long, Float] = {
    val mapp = markses.groupBy(_.subjectId)
    val mapp1 = mapp.keys.toList
    val mapp3 = mapp1.map(_.toLong)
    val mapp4 = mapp.map(_._2.map(_.marksObtained)).flatten
    mapp3.zip(mapp4).toMap
  }

  val listOfScoreCard = for{
    s <- studentList
    studentId = s.id
    listOfMarks = marksList.filter(_.studentId == studentId)
    marksMap = computeMarksMap(listOfMarks)
    perc = (marksMap.foldLeft(0.0)(_+_._2))/5
  } yield (new ScoreCard(studentId, marksMap, perc.toFloat))


  val listOfNameScoreCard = listOfNames.zip(listOfScoreCard)
  val groupByMap = listOfNameScoreCard.groupBy(_._1).mapValues(_.map(_._2))
  val studentScoreCardMap = for {
    l <- groupByMap
  } yield {
    if(l._2.length == 1)
      (l._1 , l._2(0))
    else
      (l._1, l._2)
  }
  studentScoreCardMap
}

generateScorecards


def getScorecardsByName(name: String): List[ScoreCard] = {
  val studentScoreCardMap = generateScorecards

  val valueInMap = studentScoreCardMap(name)

  valueInMap match {
    case x: List[ScoreCard] => x
    case x:ScoreCard => List(x)
    case _ => throw new IllegalArgumentException
  }
}

getScorecardsByName("Shubham")

getScorecardsByName("Jassi")

val l12 = for {
  l <- studentList
  if(l.gender == Gender.MALE)
} yield getScorecardsByName(l.name)

l12.flatten