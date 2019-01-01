
function selectTab(evt){
    evt = evt || window.event;
    var elm = evt.target;
    var titles = elm.parentNode;
    var children = titles.childNodes;
    var pos = 0;
    var counter = 0;
    for(var index = 0 ; index < children.length; index++){
	var child = children.item(index);
	if(child == elm){
	    pos = counter;
	} 
	if(child.getAttribute){
	    counter++;
	    if(child.getAttribute('class') == 'tab-title-selected'){
		child.setAttribute('class', 'tab-title');
	    }
	}
    }
    elm.setAttribute('class', 'tab-title-selected');
    var tabpanel = titles.parentNode;
    children = tabpanel.childNodes;
    counter = 0;
    for(var index = 0; index < children.length; index++){
	var child = children.item(index);
	if(child.getAttribute){
	    var currentClass = child.getAttribute('class');
	    if(currentClass == 'tab-content-selected'){
		if(counter != pos){
		    child.setAttribute('class', 'tab-content');
		}
		counter++;
	    } else if(currentClass == 'tab-content'){
		if(counter == pos){
		    child.setAttribute('class', 'tab-content-selected');
		}
		counter++;
	    }
	}
    }
    
}