$(document).ready(function() {
  $('#predictions').hide();
  $('#waiting').hide();
  $('#predictionform').submit(function() {
    submitPrediction();
    return false;
  });
})

function submitPrediction() {
    var jsonQuery = {   "time"     : $('#time').val()
                      , "distance" : $('#distance').val() 
                      , "resthr"   : $('#resthr').val() 
                      , "maxhr"    : $('#maxhr').val()
                      , "weight"   : $('#weight').val()
		      , "kouros"   : $('#kouros').is(':checked') ? "kouros" 
		      : "normal"
                    }; 
    $('#waiting').show();
    $.post( "hs/ultra.cgi"
	      , JSON.stringify(jsonQuery) 
	      , function(result) {
	           showPrediction(result);
			   $('#waiting').hide();
	        }
	      , "json");
}

function showPrediction(result) {
    if (result[0].hasOwnProperty('error')) {
 	$('#result').html(result[0].error);
	$('#result').reload(true);
	$('#predictions').hide();
	return;
    }
    var i = 0; 
    $('#result').html("<hr>Ennuste perustuen aikaan: " + result[0].time +" matkalle "+ result[0].distance +".");
    $('#predictions').hide();
    var rowCount = $('#predictions tr').length;
    for (i = 0; i < rowCount -1; i++) {
	$('#predictions tr:last').remove()
    }
    for (i = 1; i < result.length; i++) {
	$('#predictions tr:last').after("<tr><td>" 
					+ result[i].name     + "</td><td>" 
					+ result[i].time     + "</td><td>" 
					+ result[i].distance + "</td><td>"
					+ result[i].speed    + "</td><td>" 
					+ result[i].power    + "</td><td>"
					+ result[i].hr       + "</td><td>"
					+ result[i].kcal     + "</td><td>"
					+ result[i].co2       + "</td></tr>"
				       );
    }
    $('#predictions').show();
}
