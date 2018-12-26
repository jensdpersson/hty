<t:transform version="1.0"
             xmlns:t="http://www.w3.org/1999/XSL/Transform">
      <t:template match="testsuite">
          <html>
               <head>
                   <title>Testsuite <t:value-of select="@name"/></title>
                   <style type="text/css">
                       div.resultface {
                            margin: 5px;
                            padding: 5px;
                            background: white;
                            color: black;
                       }
                       .fail { background: orange;}
                       .error { background: red;}
                       .skip { background: yellow;}
                   </style>
                </head>
                <body>
                    <div class="resultface">
                        <div>
                            <span>Testsuite <t:value-of select="@name"/> has <t:value-of select="@tests"/> tests, of these </span>
                            <span class="fail"><t:value-of select="@failures"/> failed, </span>
                            <span class="error"><t:value-of select="@errors"/> got errors and </span>
                            <span class="skip"><t:value-of select="@skipped"/> were skipped.</span>
                        </div>
                        <table>
                            <tr>
               <th>testcase</th>
               <th>time</th>
               <th>info</th>
           </tr>
                            <t:apply-templates/>
                        </table>
                     </div>
                </body>
         </html>
     </t:template>
     <t:template match="testcase[error]">
           <tr class="error">
               <td><t:value-of select="@name"/>::<t:value-of select="@description"/></td>
               <td><t:value-of select="@time"/></td>
               <td><t:value-of select="."/></td>
           </tr>
     </t:template>
     <t:template match="testcase">
           <tr>
               <td><t:value-of select="@name"/>::<t:value-of select="@description"/></td>
               <td><t:value-of select="@time"/></td>
               <td><t:value-of select="."/></td>
           </tr>
     </t:template>
</t:transform>
