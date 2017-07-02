package de.unistuttgart.ims.drama.data;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.apache.uima.UIMAException;
import org.apache.uima.jcas.cas.TOP;
import org.xml.sax.SAXException;

import de.tudarmstadt.ukp.dkpro.core.api.segmentation.type.Token;
import de.unistuttgart.ims.drama.api.Author;
import de.unistuttgart.ims.drama.api.Translator;
import de.unistuttgart.ims.drama.api.Utterance;

public class Devel {

	@SuppressWarnings("unchecked")
	public static void main(String[] args) throws ClassNotFoundException, UIMAException, SAXException, IOException {
		DataLoader dl = new DataLoader("/Users/reiterns/Documents/QuaDramA/Data");

		Class<? extends TOP>[] classes = new Class[] { Author.class, Translator.class };
		String[] ids = new String[] { "tc:tc0623", "tg:rksp.0", "tg:vndf.0" };

		System.out.println(dl.getAnnotations(ids, classes));
		System.exit(1);
		FileWriter fw = new FileWriter(new File("target/test.csv"));
		fw.write(dl.getAnnotations(new String[] { "tg:rksp.0", "tc:tc0623" }, Utterance.class, Token.class));
		fw.flush();
		fw.close();
	}

}
