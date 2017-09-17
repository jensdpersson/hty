<t:transform version="1.0"
             xmlns:t="http://www.w3.org/1999/XSL/Transform">
      <t:template match="/dir">
        <html>
             <head>
                 <title>Testsuite <t:value-of select="@name"/></title>
                 <style type="text/css">
                    html, body, iframe {
                      height: 100%;
                    }

                    ul {
                      float:left; width: 20%; border-right:2px solid black;
                      height: 100%;
                      margin: 0px;
                      padding: 10px;
                    }

                    iframe {
                      height:100%; width:75%; float:left;
                      border: 0px;
                    }
                 </style>
              </head>
              <body>

                    <ul>
                          <t:apply-templates/>
                    </ul>

                   <iframe name="content">

                   </iframe>

              </body>
       </html>
     </t:template>
     <t:template match="file">
           <li>
             <a href="{@name}" target="content"><t:value-of select="@name"/></a>
           </li>
     </t:template>
     <t:template match="dir">
     </t:template>
</t:transform>
