<t:transform version="1.0"
	     xmlns="http://www.w3.org/1999/xhtml"
	     xmlns:p="http://prosith.hoverview.org"
	     xmlns:t="http://www.w3.org/1999/XSL/Transform">
  <t:output method="html"/>
  <t:template match="p:project-site">
    <html>
      <head>
	<title><t:value-of select="@title"/></title>
	<link rel="stylesheet" href="/tabpanel/css/tabs.css" type="text/css"/>
	<script type="text/javascript" src="/tabpanel/js/tabs.js">
	  //bollhav
	</script>
      </head>
      <body>
	<span>This is the project site for <t:value-of select="@title"/></span>
	<div class="tabs">
	  <div class="tab-titles">
	    <t:for-each select="p:section">
	      <a class="tab-title" onclick="selectTab(event)">
		<t:value-of select="@title"/>
	      </a>
	    </t:for-each>
	  </div>
	  <t:for-each select="p:section">
	    <div class="tab-content">
	      <iframe src="{@href}" style="border:0px" width="100%" height="500px"/>
	    </div>
	  </t:for-each>
	</div>
      </body>
    </html>
  </t:template>
</t:transform>