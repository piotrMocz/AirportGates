package airportgates.data

class Plane(val id: Int,
            val sizeCategory: Category)


sealed trait Category
case object Small extends Category
case object Medium extends Category
case object Large extends Category