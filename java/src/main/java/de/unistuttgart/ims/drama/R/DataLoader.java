package de.unistuttgart.ims.drama.R;

import java.io.File;

public class DataLoader {
	File xmiDirectory;

	public DataLoader(String xmiDirectoryName) {
		xmiDirectory = new File(xmiDirectoryName);
	}

	@Override
	public String toString() {
		return xmiDirectory.getAbsolutePath();
	}
}
