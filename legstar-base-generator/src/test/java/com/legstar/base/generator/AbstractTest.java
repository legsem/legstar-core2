package com.legstar.base.generator;

import java.io.File;

import static org.junit.Assert.*;

import org.apache.commons.io.Charsets;
import org.apache.commons.io.FileUtils;
import org.junit.Rule;
import org.junit.rules.TestName;

public class AbstractTest {

    /** This means references should be created instead of compared to results. */
    private boolean createReferences = false;

    public static final File TEST_XSD_FOLDER = new File("src/test/xsds");

    @Rule
    public TestName name = new TestName();

    /** Generated classes Reference folder. */
    public static final File SRC_REF_DIR = new File(
            "src/test/resources/references");

    /**
     * Check a string result against a reference file content (One ref file per
     * test case).
     * <p/>
     * If reference needs to be created, it is created rather than used for
     * comparison.
     * 
     * @param resultText the result text
     * @param refFileName a reference file name
     * @throws Exception if IO fails
     */
    public void check(final String resultText, String refFileName)
            throws Exception {
        File refFile = new File(SRC_REF_DIR, name.getMethodName() + "/"
                + refFileName);
        if (isCreateReferences()) {
            FileUtils.writeStringToFile(refFile, resultText, Charsets.UTF_8);
        } else {
            String referenceText = FileUtils.readFileToString(refFile,
                    Charsets.UTF_8);
            assertEquals(referenceText, resultText);
        }

    }

    /**
     * @return true if references should be created instead of compared to
     *         results
     */
    public boolean isCreateReferences() {
        return createReferences;
    }

    public void setCreateReferences(boolean createReferences) {
        this.createReferences = createReferences;
    }

}
