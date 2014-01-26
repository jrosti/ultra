$(document).ready(function() {
  $('#predictions').hide();
  $('#waiting').hide();
  $('#predictionform').submit(function() {
    submitPrediction();
    return false;
  });

  // validation
  var timeProperty = textFieldValue($("#time"))
    .map(reValidator(/^\d+:\d+($|:\d+$|:\d+\.\d+$)/));
  timeProperty.map(validation("Syötä aika muodossa hh:mm tai hh:mm:ss[.tt] ", ok))
    .assign($("#timeV"), "text");

  var distanceProperty = textFieldValue($("#distance")).map(isNumber);
  distanceProperty.map(validation("Syötä matka metreinä", function(x) { return x + " m"}))
    .assign($("#distanceV"), "text");

  var defaultV = validation("Syötä numero", ok);

  var resthrProperty = textFieldValue($("#resthr")).map(isNumber);
  var maxhrProperty = textFieldValue($("#maxhr")).map(isNumber);

  var weightProperty = textFieldValue($("#weight")).map(isNumber);

  resthrProperty.map(defaultV).assign($("#resthrV"), "text");
  maxhrProperty.map(defaultV).assign($("#maxhrV"), "text");
  weightProperty.map(defaultV).assign($("#weightV"), "text");

  var basicProperty = timeProperty.filter(notNull).and(distanceProperty.filter(notNull));
  basicProperty.throttle(300).subscribe(submitPrediction);
  var basicAndHrProperty = basicProperty.and(resthrProperty.filter(notNull)).and(maxhrProperty.filter(notNull));
  basicAndHrProperty.throttle(300).subscribe(submitPrediction);
  basicAndHrProperty.and(weightProperty.filter(notNull)).throttle(300).subscribe(submitPrediction);
})

function notNull(val) {
  return val !== null;
}

function ok(_) {
  return "ok";
}

function reValidator(re) {
  return function(x) {
    var m = x.match(re);
    return m !== null ? m[0] : null;
  }
}

function validation(errorText, successFunction) {
  return function(val) {
    if (val === null) {
      return errorText;
    } else {
      return successFunction(val);
    }
  }
}

function isNumber(x) {
  return reValidator(/\d+/)(x);
}

function textFieldValue(textField) {
  function value() {
    return textField.val();
  }
  var keyups = textField.asEventStream("keyup");
  var changes = textField.asEventStream("change");
  return keyups.merge(changes).map(value).toProperty(value());
}

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
  $.post( "/hs/rest/prediction"
    , JSON.stringify(jsonQuery)
    , function(result) {
      $('#waiting').hide();
      showPrediction(result);
    }
    , "json");
}

function showPrediction(result) {
  $('#result').show();
  if (result[0].hasOwnProperty('error')) {
    $('#result').html(result[0].error);
    //$('#result').reload(true);
    $('#predictions').hide();
    return;
  }
  var i = 0;
  $('#result').html("<hr>Ennuste perustuen aikaan: " + result[0].time +" matkalle "+ result[0].distance +".");
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
      + result[i].kcal     + "</td></tr>");
  }
  $('#predictions').show();
}
