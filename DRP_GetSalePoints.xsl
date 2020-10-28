<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
 <xsl:template match="/">

 <HTML>
  <xsl:for-each select="//SalePoint">
    <p>
	execute procedure jns$upgrade_salepoint(
     '<xsl:value-of select="ID" />',
     '<xsl:value-of select="Name" />',
     '<xsl:value-of select="Adress" />'); 
    </p>
    <p>
	commit;
    </p>
  </xsl:for-each>
 </HTML>
 </xsl:template>
</xsl:stylesheet> 