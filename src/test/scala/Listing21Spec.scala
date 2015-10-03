import org.scalatest._
import com.actualadam.fpis.ch2.listing.Listing21

class Listing21Spec extends FlatSpec with Matchers {
    "Listing21" should "do absolute values" in {
        Listing21.formatAbs(-4) should be (4)
    }
}
