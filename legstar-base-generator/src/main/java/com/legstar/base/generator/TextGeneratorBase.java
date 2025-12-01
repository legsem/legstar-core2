package com.legstar.base.generator;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

public class TextGeneratorBase {
	
	public byte[] resourceToBytes(String location) {
		try (InputStream is = getClass().getResourceAsStream(location);
				ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
	        byte[] buffer = new byte[8192];
	        int read;
	        while ((read = is.read(buffer, 0, buffer.length)) >= 0) {
	        	baos.write(buffer, 0, read);
	        }
			return baos.toByteArray();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	public String resourceToString(String location, Charset cs) {
		byte[] bytes =  resourceToBytes(location);
		return new String(bytes, cs);
	}

	public String resourceToString(String location) {
		return resourceToString(location, StandardCharsets.UTF_8);
	}
}
