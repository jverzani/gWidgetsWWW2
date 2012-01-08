// column formats for gtable. Based on types
// text is red or black depending
gtableNumeric = function(val) { 
    return '<span style="color:red">' + val + '</span>';
}
    
gtableInteger = function(val) { 
    return '<span style="color:red">' + val  + '</span>';
};

gtableLogical = function(val) { 
    return '<span style="color:black">' + val + '</span>';
};

gtableIcon = function(val, metaData, record, rowIndex, columnIndex, store) { 
    metaData.style = 'background-repeat:no-repeat'
    return "<img src='" + val + "' />";
}

gtableDate = function(val) {
    return '<span style="color:red">' + val + '</span>';
}
