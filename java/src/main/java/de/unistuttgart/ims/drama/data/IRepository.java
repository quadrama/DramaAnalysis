package de.unistuttgart.ims.drama.R;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;

import org.apache.uima.UIMAException;
import org.apache.uima.jcas.JCas;
import org.xml.sax.SAXException;

public interface IRepository {

	JCas getJCas(String id) throws UIMAException, SAXException, IOException;

	Iterator<String> getIds();

	String[] getCollections();

	InputStream getCollection(String collection) throws FileNotFoundException;

	boolean existsId(String id);

}
