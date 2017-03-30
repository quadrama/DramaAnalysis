package de.unistuttgart.ims.drama.data;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.logging.Logger;

import org.apache.commons.configuration2.CombinedConfiguration;
import org.apache.commons.configuration2.INIConfiguration;
import org.apache.commons.configuration2.tree.OverrideCombiner;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.uima.UIMAException;
import org.apache.uima.cas.impl.XmiCasDeserializer;
import org.apache.uima.fit.factory.JCasFactory;
import org.apache.uima.jcas.JCas;
import org.apache.uima.jcas.tcas.Annotation;
import org.xml.sax.SAXException;

import de.unistuttgart.ims.uimautil.CoNLLExport;

public class DataLoader implements IRepository {
	File rootDirectory;
	String defaultNamespace = "tg";
	String idSeparator = ":";
	CombinedConfiguration config;

	static Logger logger = Logger.getLogger(DataLoader.class.getPackage().getName());

	public DataLoader(String xmiDirectoryName) {
		rootDirectory = new File(xmiDirectoryName);
		loadConfig();
	}

	private void loadConfig() {
		INIConfiguration defaultConfig = new INIConfiguration();
		INIConfiguration serverConfig = new INIConfiguration();

		InputStream is = null;
		try {
			// reading of default properties from inside the war
			is = getClass().getResourceAsStream("/project.properties");
			if (is != null) {
				defaultConfig.read(new InputStreamReader(is, "UTF-8"));
				// defaults.load();
			}
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			IOUtils.closeQuietly(is);
		}

		try {
			// reading additional properties in seperate file, as specified
			// in the context
			is = new FileInputStream(new File(rootDirectory, "settings.properties"));
			serverConfig.read(new InputStreamReader(is, "UTF-8"));

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			IOUtils.closeQuietly(is);
		}

		config = new CombinedConfiguration(new OverrideCombiner());
		config.addConfiguration(serverConfig);
		config.addConfiguration(defaultConfig);

	}

	@Override
	public String toString() {
		return rootDirectory.getAbsolutePath();
	}

	public JCas getJCas(String id) throws UIMAException, SAXException, IOException {
		JCas jcas = JCasFactory.createJCas();
		try {
			XmiCasDeserializer.deserialize(getXmiStream(id), jcas.getCas(), true);
		} catch (SAXException e) {
			logger.severe("XMI Parsing Error for id " + id);
			throw e;
		}
		return jcas;
	}

	public Iterator<String> getIds() {
		final Iterator<File> fIter = FileUtils.iterateFiles(getXmiDirectory(), new String[] { "xmi" }, true);
		return new Iterator<String>() {

			public boolean hasNext() {
				return fIter.hasNext();
			}

			public String next() {
				File f = fIter.next();

				return f.getParentFile().getName() + idSeparator + f.getName().substring(0, f.getName().length() - 4);
			}
		};

	}

	public String[] getCollections() {
		return getCollectionsDirectory().list(new FilenameFilter() {

			public boolean accept(File dir, String name) {
				return !name.startsWith(".");
			}
		});
	}

	public InputStream getCollection(String collection) throws FileNotFoundException {
		return new FileInputStream(getCollectionFile(collection));
	}

	public boolean existsId(String id) {
		return getFile(id).exists();
	}

	private File getFile(String id) {
		String[] idp = id.split(idSeparator, 2);
		if (idp.length == 1) {
			idp = (String[]) ArrayUtils.addAll(new String[] { "tg" }, idp);
		}

		String path = StringUtils.join(idp, File.separatorChar);
		if (!path.endsWith(".xmi"))
			path = path + ".xmi";
		return new File(getXmiDirectory(), path);
	}

	private File getXmiDirectory() {
		return new File(rootDirectory, getXmiDirectoryName());
	}

	private String getXmiDirectoryName() {
		return "xmi";
	}

	private String getCollectionsDirectoryName() {
		return "collections";
	}

	private InputStream getXmiStream(String id) throws FileNotFoundException {
		return new FileInputStream(getFile(id));
	}

	private File getCollectionsDirectory() {
		return new File(rootDirectory, getCollectionsDirectoryName());
	}

	private File getCollectionFile(String collectionName) {
		return new File(getCollectionsDirectory(), collectionName);
	}

	public Object[][] getAnnotations(String dramaId, String annotationClassName, String coveredAnnotationClassName)
			throws ClassNotFoundException, UIMAException, SAXException, IOException {

		Class<? extends Annotation> annotationClass;
		annotationClass = (Class<? extends Annotation>) Class.forName(annotationClassName);

		Class<? extends Annotation> coveredAnnotationClass = null;
		if (coveredAnnotationClassName != null)
			coveredAnnotationClass = (Class<? extends Annotation>) Class.forName(coveredAnnotationClassName);

		logger.info("getAnnotations(" + dramaId + "," + annotationClassName + "," + coveredAnnotationClassName + ")");
		CoNLLExport exporter = new CoNLLExport();
		exporter.init(config, null, annotationClass, coveredAnnotationClass);

		String[] dramaIds;
		if (dramaId.contains(",")) {
			dramaIds = dramaId.split(",");
		} else {
			dramaIds = new String[] { dramaId };
		}
		for (String s : dramaIds) {
			JCas jcas;
			jcas = getJCas(s);

			exporter.convert(jcas);

		}
		return Util.toArray(exporter.getResult());
	}

	public Object[][] getListOfSets() {

		String[] coll = getCollections();
		Object[][] r = new Object[2][];
		r[0] = coll;
		r[1] = new Object[coll.length];
		// r.add(new Object[] { "id", "length" });
		for (int i = 0; i < coll.length; i++) {
			String file = coll[i];
			try {
				r[1][i] = new Integer(Util.countLines(getCollection(file)));
			} catch (IOException e) {
				r[1][i] = new Integer(0);
				e.printStackTrace();
			}
		}

		return r;
	}

	public String[] getCollectionEntries(String tagsList) {

		Set<String> ret = new HashSet<String>();

		String[] tags = tagsList.split(",");
		Set<String> selectedIds = new HashSet<String>();
		for (String t : tags) {
			try {
				selectedIds.addAll(IOUtils.readLines(getCollection(t), "UTF-8"));
			} catch (IOException e) {
				e.printStackTrace();
			}

		}
		for (String id : selectedIds) {
			if (existsId(id)) {
				ret.add(id);

			}
		}
		return ret.toArray(new String[ret.size()]);

	}
}
