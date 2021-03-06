/*
 * This Java source file was generated by the Gradle 'init' task.
 */
package the_tyranny_of_the_rocket_equation;

import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;

public class AppTest {
    @Test public void fuelForMass12ShouldBe2() {
        App app = new App();
        assertEquals(2, app.fuelForMass(12L).longValue());
    }

    @Test public void fuelForMass100756ShouldBe33583() {
        App app = new App();
        assertEquals(33583L, app.fuelForMass(100756L).longValue());
    }

    @Test public void fuelForMassListShouldEqualSum() {
        App app = new App();
        List<Long> input = new ArrayList<>(Arrays.asList(12L, 14L));
        assertEquals(4L, app.fuelForMassList(input).longValue());
    }

    @Test public void fuelForMassRec14ShouldBe2() {
        App app = new App();
        assertEquals(2, app.fuelForMassRec(14L).longValue());
    }

    @Test public void fuelForMassRec1969ShouldBe966() {
        App app = new App();
        assertEquals(966, app.fuelForMassRec(1969L).longValue());
    }
}
