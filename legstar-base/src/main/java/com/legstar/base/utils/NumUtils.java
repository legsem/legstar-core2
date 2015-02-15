package com.legstar.base.utils;

public class NumUtils {

    public static int longToInt(long l) {
        if (l < Integer.MIN_VALUE || l > Integer.MAX_VALUE) {
            throw new IllegalArgumentException(l
                    + " cannot be cast to int");
        }
        return (int) l;
    }
}
