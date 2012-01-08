// from documentation for ext.form.field.spinner
Ext.define('Ext.ux.CustomSpinner', {
    extend: 'Ext.form.field.Spinner',
    alias: 'widget.customspinner',

    // override onSpinUp (using step isn't neccessary)
 onSpinUp: function() {
        var me = this;
        if (!me.readOnly) {
            var val = me.step; // set the default value to the step value
            if(me.getValue() !== '') {
                val = parseInt(me.getValue()); // gets rid of " Pack"
            }
            me.setValue(Math.min(val + me.step, me.maxValue));
        }
    },
  onSpinDown: function() {
        var val, me = this;
        if (!me.readOnly) {
            if(me.getValue() !== '') {
                val = parseInt(me.getValue()); // gets rid of " Pack"
            }
            me.setValue(Math.max(val - me.step, me.minValue));
        }
    }
});
