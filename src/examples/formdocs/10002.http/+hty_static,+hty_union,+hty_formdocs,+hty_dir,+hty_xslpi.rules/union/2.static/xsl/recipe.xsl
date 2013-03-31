<t:transform version="1.0"
			 xmlns:t="http://www.w3.org/1999/XSL/Transform">
	<t:template match="/dir">
		<html>
			<head>
				<title></title>
			</head>
			<body>
				<t:for-each select="file">
					<div>
						<a target="form" href="/data/{.}">
							<t:value-of select="."/>
						</a>		
					</div>
				</t:for-each>
			</body>
		</html>
	</t:template>
	<t:template match="/recipe">
		<html>
	<head>
		<script type="text/javascript">
			function setAction(elm){
				//var action = elm.getAttribute('action');
				//if(action.charAt(action.length) != '/'){
				//	action = action + '/';
				//}
				var title = document.getElementById('title');
				var action = title.value;
				elm.setAttribute('action', '/data/' + action + ".xml");
			}
		</script>
	</head> 
	<body>
		<form method="post" onsubmit="setAction(this)">
		    <input type="hidden" name="[" value="recipe" />
			<input type="text" id="title" name="title" value="{title}"/>
				
			<input type="submit" value="Save" />
			<br />
			<textarea name="body" cols="40" rows="10">
				<t:value-of select="body"/>
			</textarea>
			<input type="hidden" name="]" value="recipe" />
		</form>
	</body>
</html>
</t:template>
</t:transform>