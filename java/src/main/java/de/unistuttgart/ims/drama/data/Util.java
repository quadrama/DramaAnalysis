package de.unistuttgart.ims.drama.data;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

public class Util {
	public static int countLines(InputStream is) throws IOException {
		try {
			byte[] c = new byte[1024];
			int count = 0;
			int readChars = 0;
			boolean empty = true;
			while ((readChars = is.read(c)) != -1) {
				empty = false;
				for (int i = 0; i < readChars; ++i) {
					if (c[i] == '\n') {
						++count;
					}
				}
			}
			return (count == 0 && !empty) ? 1 : count;
		} finally {
			is.close();
		}
	}

	public static <T> T[][] toArray(List<List<T>> list) {
		System.err.println("converting array ... ");
		T[][] r = (T[][]) new Object[list.get(0).size()][list.size()];

		for (int i = 0; i < list.size(); i++) {
			for (int j = 0; j < list.get(i).size(); j++) {
				r[j][i] = list.get(i).get(j);
			}
		}
		return r;
	}
}
