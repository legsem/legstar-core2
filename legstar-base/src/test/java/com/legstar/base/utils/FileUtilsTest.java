package com.legstar.base.utils;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import org.junit.Test;

public class FileUtilsTest {
	
	@Test
	public void testListFilesRecursive() {
		try {
			File f1 = File.createTempFile("simple", ".txt");
			f1.deleteOnExit();
			assertTrue(FileUtils.listFilesRecursive(f1).isEmpty());
			File f2 = Files.createTempDirectory("f2").toFile();
			f2.deleteOnExit();
			assertTrue(FileUtils.listFilesRecursive(f2).isEmpty());
			File f3 = Files.createTempFile(f2.toPath(), "f3_", ".txt").toFile();
			f3.deleteOnExit();
			assertTrue(FileUtils.listFilesRecursive(f2).get(0).getName().startsWith("f3_"));
			File f4 = Files.createTempDirectory(f2.toPath(), "f4").toFile();
			f4.deleteOnExit();
			File f5 = Files.createTempFile(f4.toPath(), "f5_", ".txt").toFile();
			f5.deleteOnExit();
			assertTrue(FileUtils.listFilesRecursive(f4).get(0).getName().startsWith("f5_"));
			assertTrue(FileUtils.listFilesRecursive(f2).get(0).getName().startsWith("f3_"));
			assertTrue(FileUtils.listFilesRecursive(f2).get(1).getName().startsWith("f5_"));
			
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		
	}
}
