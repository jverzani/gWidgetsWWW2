Ext.define('Ext.ux.GGoogleVis', {
    
    extend: 'Ext.panel.Panel',

    alias: 'widget.ggooglevis',
    
    requires: ['Ext.window.MessageBox'],
    initComponent : function(){
        
        this.addEvents(
            /**
             * @event apiready
             * Fires when the vis is ready for interaction
             * @param {GGoogleVis} this
             */
            'apiready'
        );
        
        Ext.ux.GGoogleVis.superclass.initComponent.call(this);        

        if (window.google){
          this.on('afterrender', this.apiReady, this);
        }else{
          window.gmapapiready = Ext.Function.bind(this.apiReady,this);
          this.buildScriptTag('https://www.google.com/jsapi');
        }

    },
    apiReady : function(){
        
        if (this.rendered){

          Ext.defer(function(){            
              // here
          }, 200,this); // Ext.defer
          
        } else {
          this.on('afterrender', this.apiReady, this);
        }
    },
    // private
    afterRender : function(){
        
        var wh = this.ownerCt.getSize();
        Ext.applyIf(this, wh);
        
        Ext.ux.GGoogleVis.superclass.afterRender.call(this);

    },
    // private
    buildScriptTag: function(filename, callback) {
        var script  = document.createElement('script'),
        head        = document.getElementsByTagName("head")[0];
        script.type = "text/javascript";
        script.src  = filename;    
        
        return head.appendChild(script);
    },
    // private
    onResize : function(w, h){
        
        Ext.ux.GGoogleVis.superclass.onResize.call(this, w, h);

    },
    // private
    setSize : function(width, height, animate){
        
        Ext.ux.GGoogleVis.superclass.setSize.call(this, width, height, animate);
        
    },

});