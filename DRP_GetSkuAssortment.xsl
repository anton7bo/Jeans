<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
 <xsl:template match="/">

 <HTML>
  <xsl:for-each select="//Sku">
    <p>
	execute procedure jns$upgrade_sku(
     '<xsl:value-of select="ID" />',
     '<xsl:value-of select="Name" />');
    </p>
    <p>
	commit;
    </p>
  </xsl:for-each>
 </HTML>
 </xsl:template>
</xsl:stylesheet> 