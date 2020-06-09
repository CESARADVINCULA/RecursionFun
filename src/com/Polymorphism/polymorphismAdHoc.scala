package com.Polymorphism

object polymorphismAdHoc {

  /**
   * Author: Cesar Advincula
   * Polymorphism, is a class can have many difference forms
   * and have a different behaviors in difference classes as well
   * Then scala have 3 mechanisms:
   *    subtype
   *    parametric
   *    ad-hoc
   */


  class CommonProfessionalSkills {
    val english: String = "AdvancedLevel"
    def IQ: Int = 144
  }

  /**
   * This example AD-HOC Polymorphism
   */

  trait Skills[T] {
    def getSkills(t: T): String
  }

  object Skills {
    implicit object corporateFinance extends Skills[CommonProfessionalSkills] {
      override def getSkills(t: CommonProfessionalSkills) = "CorporateFinance"
    }
    implicit object informationTechnology extends Skills[NewProfessionalSkills] {
      override def getSkills(s: NewProfessionalSkills) = "InformationTechnology"
    }
    object designThinking extends Skills[CommonProfessionalSkills] {
      override def getSkills(t: CommonProfessionalSkills) = "DesignThinking"
    }
  }

  //printSkills is the ad-hoc polymorphic function
  def printSkills[T](t: T)(implicit o: Skills[T]): Unit = {
    println(o.getSkills(t))
  }

  //A new class Defined
  class NewProfessionalSkills

  def main(args: Array[String]): Unit = {
    import Skills._

    val commonProfessionalSkills = new CommonProfessionalSkills
    printSkills(commonProfessionalSkills)

    val newProfessionalSkills = new NewProfessionalSkills
    printSkills(newProfessionalSkills)

    printSkills(commonProfessionalSkills)(designThinking)

  }

}
