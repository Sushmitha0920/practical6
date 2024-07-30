object InventoryManagement {

    var inventory1 : Map[Int, (String, Int, Double)] = Map(
        101 -> ("Chair", 10, 1000.0),
        102 -> ("Table", 5, 2000.0),
        103 -> ("Cupboard", 3, 5000.0),
    )

    var inventory2 : Map[Int, (String, Int, Double)] = Map(
        102 -> ("Table", 7, 2000.0),
        104 -> ("Knife", 15, 500.0),
        105 -> ("Bottle", 20, 400.0),
    )

    def displayProductNames(inventory: Map[Int, (String, Int, Double)]): Iterable[String] = {
        inventory.values.map(_._1)
    }

    def calculateTotal(inventory: Map[Int, (String, Int, Double)]): Double = {
        inventory.values.map { case (_, quantity, price) => quantity * price }.sum
    }

    def isEmpty(inventory: Map[Int, (String, Int, Double)]): Boolean = {
        inventory.isEmpty
    }

    def mergeInventories(inventory1: Map[Int, (String, Int, Double)], inventory2: Map[Int, (String, Int, Double)]): Map[Int, (String, Int, Double)] = {
         inventory2.foldLeft(inventory1) {
            case (acc, (id, (name, quantity, price))) =>
                acc.get(id) match {
                    case Some((_, existingQuantity, _)) =>
                        acc + (id -> (name, existingQuantity + quantity, price))
                    case None =>
                        acc + (id -> (name, quantity, price))
                }
        }
    }

    def checkProductExists(inventory: Map[Int, (String, Int, Double)], productId: Int): Unit = {
        inventory.get(productId) match {
            case Some((name, quantity, price)) =>
                println(s"Product ID: $productId, Name: $name, Quantity: $quantity, Price: $price")
            case None =>
                println(s"Product with ID $productId does not exist.")
        }
    }

    def main(args: Array[String]): Unit = {

        println("Product names in first inventory : ")
        println(displayProductNames(inventory1))

        println("\nProduct names in second inventory : ")
        println(displayProductNames(inventory2))

        println("\nIs first inventory empty : " + isEmpty(inventory1))

        println("\nIs second inventory empty : " + isEmpty(inventory2))

        val mergedInventory = mergeInventories(inventory1, inventory2)
        println("\nMerged Inventory : "+ mergedInventory)

        checkProductExists(inventory1, 101)
    }
}

