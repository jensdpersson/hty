<t:transform version="1.0"
	     xmlns:t="http://www.w3.org/1999/XSL/Transform">
  <t:template match="/">
    <html>
      <head>
	<title>Step 2</title>
	<link rel="stylesheet" type="text/css" href="../khaki.css"/>
      </head>
      <body>
	<div class="box">
	  Second case: Updating the document to a new revision. <br/>
	  <form action="" method="post">
	    <input type="hidden" name='[' value="topic"/>
	    <input type="text" placeholder="content" name='"' value="{topic}"/>
	    <input type="hidden" name=']' value="topic"/>
	    <input type="submit" value="Send"/>
	  </form>
	</div>
      </body>
    </html>
  </t:template>

</t:transform>