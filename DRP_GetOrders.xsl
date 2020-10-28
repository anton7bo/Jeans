<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template match="/">

  <HTML>
  <xsl:for-each select="//Order">
    <p>
	execute procedure jns$upgrade_order(
     '<xsl:value-of select="OrderID" />',
     '<xsl:value-of select="substring-before( concat (translate(translate(DateTime, 'T', ' '), 'Z', ''), '.'), '.')" />',  
     '<xsl:value-of select="substring-before( concat (translate(translate(DateDelivery, 'T', ' '), 'Z', ''), '.'), '.')" />',  
     '<xsl:value-of select="SalePointID" />',
     '<xsl:value-of select="SalePointName" />',
      <xsl:choose>
         <xsl:when test='Comments'>
	      '<xsl:value-of select="Comments" />',  
	 </xsl:when>
         <xsl:otherwise>null,</xsl:otherwise>
      </xsl:choose>
      <xsl:choose>
         <xsl:when test='PaidNumber'>
	      '<xsl:value-of select="PaidNumber" />',  
	 </xsl:when>
         <xsl:otherwise>null,</xsl:otherwise>
      </xsl:choose>
      <xsl:choose>
         <xsl:when test='PaidSum'>
	      '<xsl:value-of select="translate(PaidSum, ',', '.')" />',  
	 </xsl:when>
         <xsl:otherwise>null,</xsl:otherwise>
      </xsl:choose>
     '<xsl:value-of select="UserID" />',
     '<xsl:value-of select="UserName" />',
     '<xsl:value-of select="WareHouseID" />');
    </p>

	<xsl:for-each select="OrderPositions/OrderPosition">
	<p>
		select l_id from jns$upgrade_orderposition(
	      '<xsl:value-of select="../../OrderID" />',
	      '<xsl:value-of select="SkuID" />',
	      '<xsl:value-of select="SkuName" />',
	      '<xsl:value-of select="UnitID" />',
	      '<xsl:value-of select="Num" />');  

		<xsl:for-each select="PromoIDs">
		<p>
			select l_id from jns$upgrade_orderposition_promo(
		      '<xsl:value-of select="../../../OrderID" />',
		      '<xsl:value-of select="../SkuID" />',
		      '<xsl:value-of select="PromoID" />');  
		</p>
	</xsl:for-each>
	</p>
	</xsl:for-each>
    <p>
	commit;
    </p>
  </xsl:for-each>
  </HTML>
  </xsl:template>
</xsl:stylesheet>