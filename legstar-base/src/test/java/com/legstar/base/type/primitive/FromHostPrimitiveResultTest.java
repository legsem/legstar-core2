package com.legstar.base.type.primitive;

import static org.junit.Assert.*;

import org.junit.Test;

import com.legstar.base.utils.HexUtils;

public class FromHostPrimitiveResultTest {

    @Test
    public void testMessageFormat1() {
        assertEquals(
                "msg. Position is 0",
                FromHostPrimitiveResult.formatMessage("msg",
                        HexUtils.decodeHex(""), 0, 0, 0));
    }

    @Test
    public void testMessageFormat2() {
        assertEquals(
                "msg. Error at offset 1 : [0xC1^C2]",
                FromHostPrimitiveResult.formatMessage("msg",
                        HexUtils.decodeHex("C1C2"), 0, 1, 2));
    }

    @Test
    public void testMessageFormat3() {
        assertEquals(
                "msg. Error at offset 1 : [0xC0->C1C2]",
                FromHostPrimitiveResult.formatMessage("msg",
                        HexUtils.decodeHex("C0C1C2"), 1, 1, 2));
    }

    @Test
    public void testMessageFormat4() {
        assertEquals(
                "msg. Error at offset 1 : [0xC0->C1C2<-C3]",
                FromHostPrimitiveResult.formatMessage("msg",
                        HexUtils.decodeHex("C0C1C2C3"), 1, 1, 2));
    }

    @Test
    public void testMessageFormat5() {
        assertEquals(
                "msg. Error at offset 16 : [0xF1F2F3F4F5F6F7F8F1F2F3F4F5F6F7F8->C1C2<-C3]",
                FromHostPrimitiveResult.formatMessage("msg",
                        HexUtils.decodeHex("F1F2F3F4F5F6F7F8F1F2F3F4F5F6F7F8C1C2C3"), 16, 16, 2));
    }

    @Test
    public void testMessageFormat6() {
        assertEquals(
                "msg. Error at offset 17 : [0xF1F2F3F4F5F6F7F8F1F2F3F4F5F6F7F8->C1C2<-C3]",
                FromHostPrimitiveResult.formatMessage("msg",
                        HexUtils.decodeHex("F0F1F2F3F4F5F6F7F8F1F2F3F4F5F6F7F8C1C2C3"), 17, 17, 2));
    }

    @Test
    public void testMessageFormat7() {
        assertEquals(
                "msg. Error at offset 0 : [0xC1C2<-F1F2F3F4F5F6F7F8F1F2F3F4F5F6F7F8]",
                FromHostPrimitiveResult.formatMessage("msg",
                        HexUtils.decodeHex("C1C2F1F2F3F4F5F6F7F8F1F2F3F4F5F6F7F8"), 0, 0, 2));
    }

    @Test
    public void testMessageFormat8() {
        assertEquals(
                "msg. Error at offset 0 : [0xC1C2<-F1F2F3F4F5F6F7F8F1F2F3F4F5F6F7F8]",
                FromHostPrimitiveResult.formatMessage("msg",
                        HexUtils.decodeHex("C1C2F1F2F3F4F5F6F7F8F1F2F3F4F5F6F7F8F9"), 0, 0, 2));
    }
}
