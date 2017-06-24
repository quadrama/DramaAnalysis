package de.unistuttgart.ims.drama.data;

import java.io.IOException;

import org.apache.uima.UIMAException;
import org.xml.sax.SAXException;

public class Devel {

	public static void main(String[] args) throws ClassNotFoundException, UIMAException, SAXException, IOException {
		DataLoader dl = new DataLoader("/Users/reiterns/Documents/QuaDramA/Data");
		System.out.println(dl.getAnnotations(new String[] { "tc:tc0623", "tg:rksp.0" },
				de.unistuttgart.ims.drama.api.Author.class.getName(), null));
		System.out.println(dl.getAnnotations(new String[] { "tc:tc0623" },
				de.unistuttgart.ims.drama.api.Speaker.class.getName(), null));
	}

}
