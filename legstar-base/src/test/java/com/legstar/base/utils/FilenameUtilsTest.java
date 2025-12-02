package com.legstar.base.utils;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;

import org.junit.Test;

public class FilenameUtilsTest {
	
	@Test
	public void testGetBaseName() {
		try {
			File file = File.createTempFile("test", ".cob");
			file.deleteOnExit();
			String basename = FilenameUtils.getBaseName(file);
			assertEquals(file.getName(), basename + ".cob");
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	@Test
	public void testGetExtension() {
		try {
			File tempCobolFile = File.createTempFile("test", ".cob");
			tempCobolFile.deleteOnExit();
			assertEquals("cob", FilenameUtils.getExtension(tempCobolFile));
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

}
