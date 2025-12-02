package com.legstar.base.utils;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

public class FileUtils {
	
	/**
	 * Recursively list all files in a folder.
	 * 
	 * @param folder parent folder
	 * @return a list of files excluding any directory
	 */
	public static List<File> listFilesRecursive(File folder) {
		List<File> res = new ArrayList<>();
		if (folder != null && folder.isDirectory()) {
			for (File f : folder.listFiles()) {
				addFileToList (res, f);
			}
		}
		return res;
	}

	private static void addFileToList(List<File> list, File file) {
		if (file.isDirectory()) {
			list.addAll(listFilesRecursive(file));
		} else {
			list.add(file);
		}
	}
	
	/**
	 * Write a string to a file.
	 * <p>
	 * If the file exists, it is overwritten.
	 * 
	 * @param file the file to create
	 * @param str the string to use as file content
	 * @param cs the character set
	 * @throws IOException
	 */
	public static void writeStringToFile(File file, String str, Charset cs) throws IOException {
		if (file.getParentFile() != null) {
			forceMkdir(file.getParentFile());
		}
		Files.write(file.toPath(), str.getBytes(cs));
	}

	public static void writeStringToFile(File file, String str) throws IOException {
		writeStringToFile(file, str, StandardCharsets.UTF_8);
	}

	public static void writeStringToFile(File file, String str, String charsetName) throws IOException {
		writeStringToFile(file, str, Charset.forName(charsetName));
	}
	
	/**
	 * Read a file as a string.
	 * 
	 * @param file the file to read
	 * @param cs the character set
	 * @return the file's content as a string in the requested character set
	 * @throws IOException
	 */
	public static String readFileToString(File file, Charset cs) throws IOException {
		byte[] bytes = Files.readAllBytes(file.toPath());
		return new String(bytes, cs);
	}

	public static String readFileToString(File file) throws IOException {
		return readFileToString(file, StandardCharsets.UTF_8);
	}

	/**
	 * Creates a directory and all intermediary, non-existant, directories.
	 * 
	 * @param outputDir the directory to create
	 */
	public static void forceMkdir(File outputDir) {
		if (outputDir != null && !outputDir.exists()) {
			outputDir.mkdirs();
		}
	}

}
