## authenticate framework

##' Base class for providing authentification
##'
##' The \code{\link{load_app}} call allows one to specify a subclass
##' of \code{Authenticator} to perform authentification before a user
##' can access a main app. This subclass should provide at a minimum a
##' method \code{is_valid_user} with arguments \code{user} and
##' \code{pwd} to check for a valid user. The default implementation
##' is wide open. The source code provides a simple example.
##' @exportClass Authenticator
##' @name Authenticator-class
Authenticator <- setRefClass("Authenticator",
                             fields=list(
                               app_name="character",
                               last_message="character"
                               ),
                             methods=list(
                               initialize=function(app_name="",...) {
                                 initFields(app_name=app_name,
                                            last_message="")

                                 
                                 callSuper(...)
                               },
                               is_valid_cookie=function(cookie) {
                                 FALSE
                               },
                               is_valid_user=function(user, pwd, ...) {
                                 "Return logical if a valid user for the given role"
                                 if(!is.null(user)) {
                                   TRUE
                                 } else {
                                   FALSE
                                 }
                               },
                               clear_message=function() last_message <<- "",
                               create_login=function(session_id) {
                                 "Create login form. If successful redirect to appname"
                                 tpl <- "
var simple = Ext.create('Ext.form.Panel', {
        url:'{{app_url}}',
        id:'gWidgetsLoginForm',
        frame:true,
        title: '{{title}}',
        bodyStyle:'padding:5px 5px 0',
        width: 350,
        fieldDefaults: {
            msgTarget: 'side',
            labelWidth: 75
        },
        defaultType: 'textfield',
        defaults: {
            anchor: '100%'
        },

        items: [{
            fieldLabel: 'User Name',
            name: 'user_name',
            allowBlank:false
        },{
            fieldLabel: 'Password',
            name: 'password',
            inputType: 'password'
        },
        {
            fieldLabel: ' ',
            xtype: 'label',
            html: '<em>{{last_message}}</em>'
        }
],

        buttons: [{
            text: 'Submit',
            handler: function() {
                var formValues = this.up('form').getForm().getValues();
                formValues.session_id = '{{session_id}}';
                Ext.Ajax.request({
                   url:'{{app_url}}',
                   jsonData:formValues,
                   success:function(response) {
                     $.globalEval(response.responseText);
                   }
                });
            }
        }]
    });

    simple.render(document.body);

"
                                 out <- whisker.render(tpl,
                                                       list(app_url="/custom/test",
                                                            title="Authenticate",
                                                            session_id=session_id,
                                                            last_message=escapeSingleQuote(last_message)
                                                            )
                                                       )
                                 return(out)
                               }
                             ))


## example subclass
## put in a spouse
AuthenticateCartoons <- setRefClass("AuthenticateCartoons",
                                contains="Authenticator",
                                fields=list(
                                  passwords="data.frame"
                                  ),
                                methods=list(
                                  initialize=function(...) {
                                    txt <- "Fred Wilma
Barney Betty
Wilma Fred
Betty Barney
Homer Marge
Marge Homer
"
                                    
                                    passwords <<- data.frame(do.call("rbind",strsplit(strsplit(txt, "\\n")[[1]], " ")), stringsAsFactors=FALSE)
                                    passwords$lookup <<- paste(passwords[[1]], passwords[[2]], sep="")
                                    callSuper(...)
                                  },
                                  is_valid_user=function(user, pwd, ...) {
                                    "Do we match a row in the data frame?"
                                    out <- paste(user, pwd, sep="") %in% passwords$lookup
                                    if(out) {
                                      clear_message()
                                    } else {
                                      if(user == "")
                                        clear_message()
                                      else
                                        last_message <<- ifelse(is.na(match(user, passwords[[1]])),  "No such user", "Password incorrect")
                                    }
                                    out
                                  }
                                  ))
                                
