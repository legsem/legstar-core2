package com.legsem.legstar.context;

import java.nio.charset.Charset;

public abstract class AbstractCobolContext implements CobolContext {

    private final String hostCharsetName;

    public AbstractCobolContext(String hostCharsetName) {
        if (!Charset.isSupported(hostCharsetName)) {
            throw new IllegalArgumentException("The charset name "
                    + hostCharsetName
                    + " is not supported by your java environment."
                    + " You might miss lib/charsets.jar in your jre.");
        }
        this.hostCharsetName = hostCharsetName;
    }

    public String getHostCharsetName() {
        return hostCharsetName;
    }

}
