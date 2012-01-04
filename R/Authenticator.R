## authenticate framework

Authenticator <- setRefClass("Authenticator",
                             fields=list(
                               app_name="character",
                               role="character",
                               type="character" # subclass?
                               ),
                             methods=list(
                               initialize=function(app_name="",...) {
                                 initFields(app_name=app_name)

                                 
                                 callSuper(...)
                               },
                               is_valid_cookie=function(cookie) {
                                 FALSE
                               },
                               is_valid_user=function(user, pwd, ...) {
                                 "Return logical if a valid user for the given role"
                                 if(!is.null(user))
                                   TRUE
                                 else
                                   FALSE
                               },
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
        }],

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
                                                            session_id=session_id
                                                            )
                                                       )
                                 return(out)
                               }
                             ))


## example subclass
## we only authenticate if user matches "John"
AuthenticateJohn <- setRefClass("AuthenticateJohn",
                                contains="Authenticator",
                                methods=list(
                                  is_valid_user=function(user, pwd, ...) {
                                    user == "John"
                                  }
                                  ))
                                
