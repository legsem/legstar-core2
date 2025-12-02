package com.legstar.base.utils;

import java.io.File;

public class FilenameUtils {
	
	/**
	 * Return the file name without the extension.
	 */
	public static String getBaseName(File f) {
		if (f == null) {
			return null;
		} else {
			String name = f.getName();
			int pos = name.lastIndexOf('.');
			return pos == -1 ? name : name.substring(0, pos);
		}
	}

	/**
	 * Return the file extension or empty string if no extension found.
	 */
	public static String getExtension(File f) {
		if (f == null) {
			return null;
		} else {
			String name = f.getName();
			int pos = name.lastIndexOf('.');
			return pos == -1 ? "" : name.substring(pos + 1);
		}
	}

}
