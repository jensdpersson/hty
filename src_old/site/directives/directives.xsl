<t:transform
  version="1.0"
  xmlns:t="http://www.w3.org/1999/XSL/Transform">
  <t:template match="/directives">
    <html>
      <head>
        <title>Hty configuration directives</title>
        <style>
          ul {
            list-style-type: none;
            margin: 0;
            padding: 0;
            text-align: center;
          }
          li {
            padding: 5px;
            font-family: sans-serif;
            border-bottom: 1px dotted black;
          }
          li a {
            text-decoration: none;
          }
          body, html {
            margin:0px;
            padding:0px;
            height: 100%;
            overflow: auto;
          }
          .nav {
            font-family: sans-serif;
            border-right: 2px black solid;
            width: 15%;
            height: 100%;
            position: fixed;
            overflow: auto;
          }
          .con {
            margin-left: 20%;
          }
          .directive {
            margin: 10px;
          }
        </style>
      </head>
      <body>
        <div class="nav">
          <ul>
            <t:apply-templates mode="nav"/>
          </ul>
        </div>
        <div class="con">
          <t:apply-templates/>
        </div>
      </body>
    </html>
  </t:template>

  <t:template match="directive[@uri]" mode="nav">
    <t:apply-templates select="document(@uri)" mode="nav"/>
  </t:template>

  <t:template match="directive" mode="nav">
    <li>
      <a href="#{@name}"><t:value-of select="@name"/></a>
      (<t:value-of select="@type"/>)
    </li>
  </t:template>

  <t:template match="directive[@uri]">
    <t:apply-templates select="document(@uri)"/>
  </t:template>

  <t:template match="directive">
    <div class="directive">
    <h3>
      <a name="{@name}"><t:value-of select="@name"/></a>
      (<t:value-of select="@type"/>)
    </h3>

    <span>
      <t:value-of select="desc"/>
    </span>

    <fieldset>
      <legend>Prefix parts</legend>
      <t:for-each select="param">
          <t:value-of select="@pos"/> : <t:value-of select="."/><br/>
      </t:for-each>
      </fieldset>
      <fieldset>
        <legend>Allowed content</legend>
      <t:for-each select="content">
          <t:value-of select="@type"/> : <t:value-of select="."/><br/>
      </t:for-each>
      </fieldset>
      <fieldset>
        <legend>Return statuses</legend>
        <t:for-each select="status">
          <t:value-of select="@code"/> : <t:value-of select="."/><br/>
        </t:for-each>
    </fieldset>
  </div>
  </t:template>

  <t:template match="directive-old">
    <table border="1">
      <tr>
        <th>
          <a name="{@name}"><t:value-of select="@name"/></a>
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
