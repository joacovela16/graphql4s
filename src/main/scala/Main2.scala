object Main2 {

  trait A {
    type R
    val value: R

    def areSame(x: this.R): Boolean = value == x
  }


  def main(args: Array[String]): Unit = {

    val a = new A {
      override type R = Int
      override val value: R = 2
    }


    println(a.areSame(2))

  }

}
