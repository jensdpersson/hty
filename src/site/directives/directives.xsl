<t:transform
  version="1.0"
  xmlns:t="http://www.w3.org/1999/XSL/Transform">
  <t:template match="/directives">
    <html>
      <head>
        <title>Hty configuration directives</title>
      </head>
      <body>
        <t:apply-templates/>
      </body>
    </html>
  </t:template>
  <t:template match="directive[@uri]">
    <t:apply-templates select="document(@uri)"/>
  </t:template>
  <t:template match="directive">
    <table border="1">
      <tr>
        <th>
          <t:value-of select="@name"/>
        </th>
        <th>
          <t:value-of select="@type"/>
        </th>
        <td>
          <t:value-of select="desc"/>
        </td>
      </tr>
      <t:for-each select="param">
        <tr>
          <th>param</th>
          <td><t:value-of select="@pos"/></td>
          <td><t:value-of select="."/></td>
        </tr>
      </t:for-each>
      <t:for-each select="content">
        <tr>
          <th>content</th>
          <td><t:value-of select="@type"/></td>
          <td><t:value-of select="."/></td>
        </tr>
      </t:for-each>
      <t:for-each select="status">
        <tr>
          <th>status</th>
          <td><t:value-of select="@code"/></td>
          <td><t:value-of select="."/></td>
        </tr>
      </t:for-each>
    </table>
  </t:template>
</t:transform>
