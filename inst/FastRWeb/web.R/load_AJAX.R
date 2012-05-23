##' load AJAX functions

run <- function(...) {
  txt <- "
var session_id = null;
var base_url = '/cgi-bin/R/gwappAJAX/';
var proxy_url = base_url + 'runProxy';
var html_proxy_url = base_url + 'runHtmlProxy';
var file_url = base_url + 'fileUploadProxy';
var jrpc_url = base_url + 'runJRpc';

'Call back into R to run a handler. Returns javascript.';
callRhandlerJQ = function(id, signal, value) {
  data = {id: id, signal:signal, value: value, session_id: session_id};
  $.ajax({
    url: base_url + 'runHandler',
    method:'GET',
    cache:false,
    dataType:'script',
    data:data
  })
};

callRhandler = function(id, signal, value) {
  data = {id: id, signal:signal, value: value, session_id: session_id};
   Ext.Ajax.request({
     url:base_url + 'runHandler',
     jsonData:data,
     success:function(response) {
       $.globalEval(response.responseText);
     },
     failure:function(response) {
       Ext.example.msg('Error:', response.text,3);
     }
   })
};



transportFun = function(id, value) {
   data = {id: id, session_id: session_id, param: Ext.JSON.encode(value)};
   Ext.Ajax.request({
     url:base_url + 'runTransport',
     jsonData:data,
     success:function(response) {
       $.globalEval(response.responseText);
     },
     failure:function(response) {
       Ext.example.msg('Error:', response.text,3);
     }
   })
};


'Make a simple callback into gWidgetsWWW2. Returns a script -- not json!';
jRpc =  function(obj, meth, params, callback) {
    data = {id: obj, session_id: session_id, meth:meth, value: params};
   Ext.Ajax.request({
url:jrpc_url,
jsonData:data,
success:callback
})
};


'Callback into R process an exported JsonRPCObject';
json_rpc = function(obj_name, meth_name, params, callback, url) {
    if(!url) {
	url = base_url + 'jsonRPCHandler';
    };

    Ext.Ajax.request({
	url: url,
	jsonData: [{
	    obj:obj_name,
	    meth:meth_name,
	    params: params,
	}],
	success:callback
    });
};


'Call to create the GUI';
createGUI = function(app_url, session_id, app) {
  $.ajax(app_url,{
        dataType:'script',
        data: {session_id: session_id, app: app},
        cache: false,
        type: 'GET'
     })
};

'Will poll server for changes. Turn on in gwindow object';
listen = function() {
    listening = false;
    if (listening)
        return;
    
    listening = true;
    $.ajax({
	data: {session_id: session_id},
	dataType: 'script',
        type: 'GET',
        url:  base_url + 'runComet',
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

'Clean up sessions once we are done with the page';
close_session = function(id) {
  $.ajax({url: base_url + 'closeSession', 
          type:'GET',
          data: {session_id:id}
        });
};
"

  out(txt)
  done()
}
