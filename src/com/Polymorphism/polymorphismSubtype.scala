package com.Polymorphism

object polymorphismSubtype {

  /**
   * Author: Cesar Advincula
   * Polymorphism, is a class can have many difference forms
   * and have a different behaviors in difference classes as well
   * Then scala have 3 mechanisms:
   *    subtype
   *    parametric
   *    ad-hoc
   */

  /**
   * This example Subtype Polimorphism
   */

  trait Professional {
    final val isEdible = true
    var name: String
    def hoursOfWork: Int
  }

  class ITProfessional extends Professional {
    var name: String = "DataArchitect"
    def hoursOfWork: Int = 8
  }

  class Doctor extends Professional {
    var name: String = "Pediatrian"
    def hoursOfWork: Int = 20
  }

  def giveInformationProfessionalChange(  p : Professional  ) : Professional ={
    p.name="NameWasChange"
    p
  }

  def giveInforProf(  p : Professional  ) : Professional ={
    p
  }

  def main(args: Array[String]): Unit = {
    val itProfessional = new ITProfessional
    val doctor = new Doctor
    println("itProfessional turns into  Professional - override a variable " + giveInformationProfessionalChange(itProfessional).name )
    println("itProfessional turns into  Professional " + giveInforProf(itProfessional).name )
    println("******************")

    println("isEdible ITProfessional : " +itProfessional.isEdible )
    println("isEdible Doctor : " +doctor.isEdible )
    println("hoursOfWork ITProfessional : " + itProfessional.hoursOfWork )
    println("hoursOfWork Doctor : " + doctor.hoursOfWork )
  }
}
