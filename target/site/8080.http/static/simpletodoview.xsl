<?xml version="1.0" encoding="UTF-8"?>
<t:stylesheet xmlns:t="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <t:output method="html"/>
    <t:template match="/">
        <html>
            <head>
                <title>Todo</title>
                <style type="text/css">
                    div {
                        border: solid 1px goldenrod;
                        //background: skyblue;
                        margin:5px;
                        padding: 5px;
                        font-family: sans-serif;
                    }
                    div.ok {
                        background: lightseagreen;
                        border: solid 1px seagreen;
                        max-height: 2em;
                        text-decoration: line-through;
                        overflow-y: hidden;
                    }
                </style>
            </head>
            <body>
                <t:apply-templates/>
            </body>
        </html>
    </t:template>

    <t:template match="todo[not(descendant-or-self::todo[not(done)])]">
        <div class="ok">
            <t:value-of select="text()[1]"/>
            <t:apply-templates/>
        </div>
    </t:template>

    <t:template match="todo">
        <div>
            <t:value-of select="text()[1]"/>
            <t:apply-templates/>
        </div>
    </t:template>

    <!--t:template match="done">
        <div class="ok">
           <t:value-of select="."/>
        </div>
    </t:template-->

    <t:template match="text()"/>

</t:stylesheet>
