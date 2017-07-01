package de.unistuttgart.ims.drama.data;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

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
		final int rowN = list.size();
		final int colN = list.get(0).size();
		System.err.println("converting array ... ");
		final T[][] r = (T[][]) new Object[colN][rowN];

		Iterator<List<T>> iterator = list.iterator();
		for (int i = 0; i < rowN; i++) {
			List<T> l = iterator.next();
			for (int j = 0; j < colN; j++) {
				r[j][i] = l.get(j);
			}
		}
		return r;
	}

	public static <T> void writeCSV(List<List<T>> table, OutputStream os, int limit) throws IOException {
		CSVPrinter p = new CSVPrinter(new OutputStreamWriter(os), CSVFormat.DEFAULT);
		int line = 0;
		for (Object s : table) {
			if (limit > 0 && line++ > limit)
				continue;
			if (s instanceof String) {
				p.printRecord(((String) s).trim());
			} else if (s instanceof Iterable) {
				p.printRecord((Iterable<Object>) s);
			} else {
				p.printRecord(s);
			}
		}
		p.flush();
		p.close();
	}
}
