
"Javascript needed to set up the local pages. Uses whisker to render";

var session_id = null;
var base_url = "{{base_url}}/gwappAJAX/";
var proxy_url = base_url + "runProxy";
var html_proxy_url = base_url + "runHtmlProxy";
var file_url = base_url + "fileUploadProxy";
var jrpc_url = base_url + 'runJRpc';

"Call back into R to run a handler. Returns javascript.";
callRhandler = function(id, signal, value) {
  data = {id: id, signal:signal, value: value, session_id: session_id};
  Ext.Ajax.request({
    url: base_url + "runHandler",
    jsonData:data,
    success:function(response) {
      $.globalEval(response.responseText);
    },
    failure:function(response) {
      Ext.example.msg("Error:", response.text,3); 
    }
  })
};

"Synchronize widget with server's value";
transportFun = function(id, value) {
   data = {id: id, session_id: session_id, param: value};
   Ext.Ajax.request({
     url:base_url + "runTransport",
     jsonData:data,
     success:function(response) {
       $.globalEval(response.responseText);
     },
     failure:function(response) {
       Ext.example.msg("Error:", response.text,3);
     }
   })
};

transportFunJQuery=function(id, value) {
  data = {id: id, session_id: session_id,  param: value };
  $.ajax({
    url:base_url + "runTransport",
    data: data,
    dataType:'script',		  
    cache: false,
    type:'GET'
  });
};


"Make a simple callback into gWidgetsWWW2. Returns a script -- not json!";
jRpc =  function(obj, meth, params, callback) {
    data = {id: obj, session_id: session_id, meth:meth, value: params};
   Ext.Ajax.request({
url:base_url + "runRpc" ,
jsonData:data,
success:callback
})
};

jRpcJQuery =  function(obj, meth, params, callback) {
  data = {id: obj, session_id: session_id, meth:meth, value: params};
  $.ajax({
    url:base_url + "runRpc",
    data: data,
    dataType:'script',		  
    cache: false,
    type:'GET'
  });
};



"Callback into R process an exported JsonRPCObject";
json_rpc = function(obj_name, meth_name, params, callback, url) {
    if(!url) {
	url = "JSON_RPC"
    };

    Ext.Ajax.request({
	url: "{{base_url}}/" + url,
	jsonData: [{
	    obj:obj_name,
	    meth:meth_name,
	    params: params,
	}],
	success:callback
    });
};


"Call to create the GUI";
createGUI = function(app_url) {
  $.ajax(app_url,{
        dataType:'script',
        data: {session_id: session_id},
        cache: false,
        type: "GET"
     })
};

"Will poll server for changes. Turn on in gwindow object";
listen = function() {
    listening = false;
    if (listening)
        return;
    
    listening = true;
    $.ajax({
	data: {session_id: session_id},
	dataType: 'script',
        type: 'GET',
        url:  base_url + "runComet",
        async: true,
        cache: true,
        timeout: 10000,
        ifModified: true,
        
        success: function(data) {
            listening = false;
            setTimeout(listen, 1000);
        },

        error: function(XMLHttpRequest, textStatus, errorThrown) {
            listening = false;
            if (textStatus == 'timeout') {
                listen()
            } else {
                setTimeout(listen, 10000);
            }
        },
    });
};

"Clean up sessions once we are done with the page. Called in create_GUI";
close_session = function(id) {
  $.ajax({url: base_url + "closeSession", 
          type:'GET',
          data: {session_id: id}, 
          timeout:1000
        });
};

