package de.unistuttgart.ims.drama.data;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import org.junit.Test;

public class TestUtil {
	@Test
	public void testToArray() {
		List<List<Integer>> test = new LinkedList<List<Integer>>();
		test.add(Arrays.asList(1, 2, 3));
		Object[][] r = Util.toArray(test);
		assertEquals(3, r.length);
		assertEquals(1, r[0].length);

		test.add(Arrays.asList(3, 4, 5));
		r = Util.toArray(test);
		assertEquals(3, r.length);
		assertEquals(2, r[0].length);
	}
}
