package de.unistuttgart.ims.drama.data;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
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
import org.apache.uima.jcas.cas.TOP;
import org.apache.uima.jcas.tcas.Annotation;
import org.xml.sax.SAXException;

import de.unistuttgart.ims.uimautil.TreeBasedTableExport;

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
			is = new FileInputStream(new File(System.getProperty("user.dir"), "settings.properties"));
			serverConfig.read(new InputStreamReader(is, "UTF-8"));
		} catch (FileNotFoundException e) {
			logger.info("No config file found, using default values. "
					+ new File(System.getProperty("user.dir"), "settings.properties").getAbsolutePath());
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

	public String[] getAllIds() {
		Iterator<String> i = this.getIds();
		ArrayList<String> arr = new ArrayList<String>();
		while (i.hasNext()) {
			arr.add(i.next());
		}
		return arr.toArray(new String[arr.size()]);

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

	public String getAnnotations(String[] dramaIds, Class<? extends TOP>... classes)
			throws ClassNotFoundException, UIMAException, SAXException, IOException {
		return getAnnotations(dramaIds, -1, classes);
	}

	public String getAnnotations(String[] dramaIds, int limit, Class<? extends TOP>... classes)
			throws ClassNotFoundException, UIMAException, SAXException, IOException {
		JCas jcas = JCasFactory.createJCas();
		TreeBasedTableExport exporter = new TreeBasedTableExport(config, jcas.getTypeSystem());
		for (int i = 0; i < classes.length; i++)
			exporter.addAnnotationType(classes[i]);

		ByteArrayOutputStream boas = new ByteArrayOutputStream();
		boolean header = true;
		for (String s : dramaIds) {

			jcas = getJCas(s);

			Util.writeCSV(exporter.convert(jcas, header), boas, limit);
			header = false;
		}
		return new String(boas.toByteArray());

	}

	public String getAnnotations(String[] dramaIds, String annotationClassName, String coveredAnnotationClassName)
			throws ClassNotFoundException, UIMAException, SAXException, IOException {
		return getAnnotations(dramaIds, annotationClassName, coveredAnnotationClassName, -1);
	}

	@SuppressWarnings("unchecked")
	public String getAnnotations(String[] dramaIds, String annotationClassName, String coveredAnnotationClassName,
			int limit) throws ClassNotFoundException, UIMAException, SAXException, IOException {

		Class<?> cl1 = Class.forName(annotationClassName);

		Class<?> cl2 = null;
		if (coveredAnnotationClassName != null)
			cl2 = Class.forName(coveredAnnotationClassName);

		if (!TOP.class.isAssignableFrom(cl1)) {
			logger.warning("Class " + annotationClassName + " does not inherit from TOP.");
			return null;
		}
		Class<? extends TOP> annotationClass = (Class<? extends TOP>) cl1;

		Class<? extends Annotation> coveredAnnotationClass = null;
		if (cl2 != null && Annotation.class.isAssignableFrom(cl2))
			coveredAnnotationClass = (Class<? extends Annotation>) cl2;

		logger.fine("getAnnotations(" + ArrayUtils.toString(dramaIds) + "," + annotationClassName + ","
				+ coveredAnnotationClassName + ")");
		return getAnnotations(dramaIds, limit, annotationClass, coveredAnnotationClass);

	}

	protected List<List<Object>> getAnnotations(JCas jcas, Class<? extends TOP>... classes) throws IOException {
		TreeBasedTableExport exporter = new TreeBasedTableExport(config, jcas.getTypeSystem());
		for (int i = 0; i < classes.length; i++)
			exporter.addAnnotationType(classes[i]);
		return exporter.convert(jcas, true);

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
