<t:transform version="1.0"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:p="http://prosith.hoverview.org"
    xmlns:t="http://www.w3.org/1999/XSL/Transform">
    <t:output method="html"/>
    <t:template match="p:project-site">
        <html>
            <head>
                <title><t:value-of select="@title"/></title>
                <link rel="stylesheet" href="prosith.css" type="text/css"/>
                <script type="text/javascript">
                    function choose_content(){
                        var choices = document.getElementById('content-choices');
                        var option = choices.options[choices.selectedIndex];
                        var iframe = document.getElementById('con');
                        iframe.src = option.value;
                    }
                </script>
            </head>
            <body onload="choose_content()">
                <div class="nav">
                    <span>This is the project site for <t:value-of select="@title"/></span>
                    <select id="content-choices" onchange="choose_content()">
                        <t:for-each select="p:section">
                            <option value="{@href}">
                                <t:value-of select="@title"/>
                            </option>
                        </t:for-each>
                    </select>
                </div>
                <div class="con">
                    <iframe name="con" id="con"/>
                </div>
            </body>
        </html>
    </t:template>
</t:transform>