Ext.define("Ext.ux.Canvas",
	   {extend: 'Ext.Component', 
	    value: null,
	    initComponent:function() {
		Ext.ux.Canvas.superclass.initComponent.call(this);
	    },
	    onRender:function(ct, position) {
		this.el = document.createElement('canvas');
		this.el.id = this.getId();
		Ext.ux.Canvas.superclass.onRender.call(this,ct,position);
	    },
	    on:function(eventName, fn, scope, o) { 
		var id = this.getId();
		var widget = document.getElementById(id);
		if(widget.addEventListener) {
		    widget.addEventListener(eventName, fn, false)
		} else {
		    widget.attachEvent('on' + event, fn)}
	    }
	   });
Ext.ComponentMgr.registerType('Canvas', Ext.ux.Canvas);
