<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:cb="http://www.legsem.com/legstar/xml/cobol-binding-1.0.1.xsd" xmlns:jaxb="http://java.sun.com/xml/ns/jaxb">
    <xsl:template match="@*|node()">
        <xsl:copy>
            <xsl:apply-templates select="@*|node()"/>
        </xsl:copy>
    </xsl:template>

    <!-- Change type from xsd:string to xsd:hexBinary -->
    <xsl:template match="*/xsd:restriction[../..//cb:cobolElement/@cobolName='S-BINARY']">
        <xsl:copy>
            <xsl:copy-of select="@*"/>
            <xsl:attribute name="base">hexBinary</xsl:attribute>
            <xsl:copy-of select="node()"/>
        </xsl:copy>
    </xsl:template>
    <!--  Change the cobol type to OCTET_STREAM_ITEM -->
    <xsl:template match="*/cb:cobolElement[@cobolName='S-BINARY']">
        <xsl:copy>
            <xsl:copy-of select="@*"/>
            <xsl:attribute name="type">OCTET_STREAM_ITEM</xsl:attribute>
        </xsl:copy>
    </xsl:template>

</xsl:stylesheet>