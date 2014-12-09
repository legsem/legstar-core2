package com.legstar.base.utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

/**
 * Converts an XML namespace to a java package name and vice versa.
 * <p/>
 * Code mostly comes from com.sun.xml.bind.api.impl.NameConverter
 */
public class NamespaceUtils {

    private final static Set < String > keywords;
    static {
        Set < String > s = new HashSet < String >();
        String[] kws = { "abstract", "continue", "for", "new", "switch",
                "assert", "default", "if", "package", "synchronized",
                "boolean", "do", "goto", "private", "this", "break", "double",
                "implements", "protected", "throw", "byte", "else", "import",
                "public", "throws", "case", "enum", "instanceof", "return",
                "transient", "catch", "extends", "int", "short", "try", "char",
                "final", "interface", "static", "void", "class", "finally",
                "long", "strictfp", "volatile", "const", "float", "native",
                "super", "while",
                // literals
                "null", "true", "false" };
        for (String kw : kws)
            s.add(kw);
        keywords = Collections.unmodifiableSet(s);
    }

    private NamespaceUtils() {
    }

    /**
     * Create a java package name out of an XML namespace.
     * 
     * @param nsUri the namespace URI
     * @return null if unable to derive a package name
     */
    public static String toPackageName(String nsUri) {

        if (nsUri == null || nsUri.length() == 0) {
            return null;
        }

        // remove scheme and :, if present
        // spec only requires us to remove 'http' and 'urn'...
        int idx = nsUri.indexOf(':');
        String scheme = "";
        if (idx >= 0) {
            scheme = nsUri.substring(0, idx);
            if (scheme.equalsIgnoreCase("http")
                    || scheme.equalsIgnoreCase("urn"))
                nsUri = nsUri.substring(idx + 1);
        }

        // tokenize string
        List < String > tokens = tokenize(nsUri, "/: ");
        if (tokens.size() == 0) {
            return null;
        }

        // remove trailing file type, if necessary
        if (tokens.size() > 1) {
            // for uri's like "www.foo.com" and "foo.com", there is no trailing
            // file, so there's no need to look at the last '.' and substring
            // otherwise, we loose the "com" (which would be wrong)
            String lastToken = tokens.get(tokens.size() - 1);
            idx = lastToken.lastIndexOf('.');
            if (idx > 0) {
                lastToken = lastToken.substring(0, idx);
                tokens.set(tokens.size() - 1, lastToken);
            }
        }

        // tokenize domain name and reverse. Also remove :port if it exists
        String domain = tokens.get(0);
        idx = domain.indexOf(':');
        if (idx >= 0)
            domain = domain.substring(0, idx);
        ArrayList < String > r = reverse(tokenize(domain,
                scheme.equals("urn") ? ".-" : "."));
        if (r.get(r.size() - 1).equalsIgnoreCase("www")) {
            // remove leading www
            r.remove(r.size() - 1);
        }

        // replace the domain name with tokenized items
        tokens.addAll(1, r);
        tokens.remove(0);

        // iterate through the tokens and apply xml->java name algorithm
        for (int i = 0; i < tokens.size(); i++) {

            // get the token and remove illegal chars
            String token = tokens.get(i);
            token = removeIllegalIdentifierChars(token);

            // this will check for reserved keywords
            if (keywords.contains(token.toLowerCase())) {
                token = '_' + token;
            }

            tokens.set(i, token.toLowerCase());
        }

        // concat all the pieces and return it
        return combine(tokens, '.');
    }

    /**
     * Create an XML namespace out of a java package name.
     * 
     * @param packageName
     * @return null if unable to derive a namespace
     */
    public static String toNamespace(String packageName) {

        if (packageName == null || packageName.length() == 0) {
            return null;
        }
        List < String > tokens = reverse(tokenize(packageName, "."));
        
        StringBuilder buf = new StringBuilder("http://");
        for (int i = 1; i < tokens.size(); i++) {
            buf.append(tokens.get(i));
            buf.append(".");
        }
        if (buf.charAt(buf.length() - 1) == '.') {
            buf.deleteCharAt(buf.length() - 1);
            buf.append("/");
        }
        buf.append(tokens.get(0));
        return buf.toString();
       
    }

    private static ArrayList < String > tokenize(String str, String sep) {
        StringTokenizer tokens = new StringTokenizer(str, sep);
        ArrayList < String > r = new ArrayList < String >();

        while (tokens.hasMoreTokens())
            r.add(tokens.nextToken());

        return r;
    }

    private static <T> ArrayList < T > reverse(List < T > a) {
        ArrayList < T > r = new ArrayList < T >();

        for (int i = a.size() - 1; i >= 0; i--)
            r.add(a.get(i));

        return r;
    }

    private static String removeIllegalIdentifierChars(String token) {
        StringBuilder newToken = new StringBuilder(token.length() + 1);
        for (int i = 0; i < token.length(); i++) {
            char c = token.charAt(i);
            if (i == 0 && !Character.isJavaIdentifierStart(c)) {
                newToken.append('_');
            }
            if (!Character.isJavaIdentifierPart(c)) {
                newToken.append('_');
            } else {
                newToken.append(c);
            }
        }
        return newToken.toString();
    }

    private static String combine(List < String > r, char sep) {
        StringBuilder buf = new StringBuilder(r.get(0).toString());

        for (int i = 1; i < r.size(); i++) {
            buf.append(sep);
            buf.append(r.get(i));
        }

        return buf.toString();
    }
}
