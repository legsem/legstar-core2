package com.legstar.base.converter;

import com.legstar.base.FromHostResult;

/**
 * Converts mainframe data to some kind of object.
 *
 * @param <T> the output object type
 */
public interface Cob2ObjectConverter<T> {

    /**
     * Converts the mainframe data into an object.
     * 
     * @param hostData a byte array holding mainframe data
     * @return a result containing the produced object and how many bytes were
     *         actually processed in hostData
     */
    FromHostResult < T > convert(byte[] hostData);

    /**
     * Converts the mainframe data starting at the specified position into an
     * object.
     * 
     * @param hostData a byte array holding mainframe data
     * @param start where to start in the mainframe byte array
     * @return a result containing the produced object and how many bytes were
     *         actually processed in hostData
     */
    FromHostResult < T > convert(byte[] hostData, int start);

}
