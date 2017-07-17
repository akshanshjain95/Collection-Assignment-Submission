package com.knoldus.kip.service

import com.knoldus.kip.models.{Gender, Marks, ScoreCard, Student}
import com.knoldus.kip.RamDatabase._

trait CollectionAssignment {

  //Collection Based - Assignment 1
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
    } yield (ScoreCard(studentId, marksMap, perc.toFloat))

    val listOfNameScoreCard = listOfNames.zip(listOfScoreCard)
    val groupByMap = listOfNameScoreCard.groupBy(_._1).mapValues(_.map(_._2))
    val studentScoreCardMap = for {
      l <- groupByMap
    } yield {
      if(l._2.length == 1) {
        (l._1, l._2(0))
      }
      else {
        (l._1, l._2)
      }
    }
    studentScoreCardMap
  }

  def getScorecardsByName(name: String): List[ScoreCard] = {
    val studentScoreCardMap = generateScorecards

    val valueInMap = studentScoreCardMap(name)

    valueInMap match {
      case x: List[ScoreCard] => x.sortBy(_.studentId)
      case x:ScoreCard => List(x)
      case _ => throw new Exception("No data found")
    }
  }

  //Collection Based - Assignment 2
  def getScoreCardByGender: (List[ScoreCard], List[ScoreCard]) = {
    val listOfMaleScoreCard = for {
      l <- studentList
      if(l.gender == Gender.MALE)
    } yield getScorecardsByName(l.name)

    val listOfFemaleScoreCard = for {
      l <- studentList
      if(l.gender == Gender.FEMALE)
    } yield getScorecardsByName(l.name)

    (listOfMaleScoreCard.flatten, listOfFemaleScoreCard.flatten)
  }

  def getScoreCardsWithHigherPercentage: (List[ScoreCard], List[ScoreCard]) = {
    val (maleScoreCard,femaleScoreCard)  =  getScoreCardByGender

    (maleScoreCard.filter(_.percentage >= 50) , femaleScoreCard.filter(_.percentage >= 50) )
  } //Internally calls getScoreCardByGender

  def getSimilarPercentageBwGroups: List[((String, ScoreCard), (String, ScoreCard))] = {
    def getStudentName(st_Id : Long):String = studentList.find(_.id==st_Id).get.name

    val (maleScoreCard,femaleScoreCard)  =  getScoreCardByGender

    maleScoreCard.flatMap(x => femaleScoreCard.filter(_.percentage==x.percentage).
      map(y=> ((getStudentName(x.studentId),x),(getStudentName(y.studentId),y))))
  }

  def femalePercentageNotInMales: List[(String, ScoreCard)] = {
    def getStudentName(st_Id : Long):String = studentList.find(_.id==st_Id).get.name
    val (maleScoreCard,femaleScoreCard)  =  getScoreCardByGender


    for( fs <- femaleScoreCard if !maleScoreCard.exists(_.percentage==fs.percentage))
      yield (getStudentName(fs.studentId), fs)
  }

}
