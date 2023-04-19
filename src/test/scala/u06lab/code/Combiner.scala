package u06lab.code

import org.junit.Test
import org.junit.Assert.*
import u06lab.solution.*

class CombinerTests {
  @Test
  def testFunctions() = {
    val f: Functions = FunctionsImpl
    assertEquals(60.1, f.sum(List(10.0, 20.0, 30.1)), 0.001)
    assertEquals(0.0, f.sum(List()), 0.001)
    assertEquals("abc", f.concat(Seq("a", "b", "c")))
    assertEquals("", f.concat(Seq()))
    assertEquals(3, f.max(List(-10, 3, -5, 0)))
    assertEquals(Integer.MIN_VALUE, f.max(List()))
  }
  
  @Test
  def testCombiner() = {
    val f: Functions = FunctionsImpl
    assertEquals(60.1, f.combiner(List(10.0, 20.0, 30.1), CombinerSum), 0.001)
    assertEquals(0.0, f.combiner(List(), CombinerSum), 0.001)
    assertEquals("abc", f.combiner(Seq("a", "b", "c"), CombinerConcat))
    assertEquals("", f.combiner(Seq(), CombinerConcat))
    assertEquals(3, f.combiner(List(-10, 3, -5, 0), CombinerMax))
    assertEquals(Integer.MIN_VALUE, f.combiner(List(), CombinerMax))   
  }
}
