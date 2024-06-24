$(document).ready(function() {
  $("#home-link").click(function() {
    $("#home").show();
    $("#about").hide();
    $("#click").prop("checked", false); // Ferme le menu
  });
  $("#about-link").click(function() {
    $("#home").hide();
    $("#about").show();
    $("#click").prop("checked", false); // Ferme le menu
  });
  $("#home").show();
  $("#about").hide();
});

Shiny.addCustomMessageHandler('shakeButton', function(message) {
  $('#' + message.id).addClass('animate__animated animate__shakeX');
  setTimeout(function() {
    $('#' + message.id).removeClass('animate__animated animate__shakeX');
  }, 1000);
});

Shiny.addCustomMessageHandler('highlight-square', function(message) {
  $('#' + message.id).addClass('highlighted');
});

Shiny.addCustomMessageHandler('removeHighlight', function(message) {
  $('#' + message.id).removeClass('highlighted');
});

Shiny.addCustomMessageHandler('handlePrediction', function(message) {
  if (message.risk_label === "Risque 0") {
    $('#green-square').addClass('highlighted');
    $('#orange-square').removeClass('highlighted');
    $('#triangle-container-green').show();
    $('#advice-box-green').show();
    $('#triangle-container-orange').hide();
    $('#advice-box-orange').hide();
  } else if (message.risk_label === "Risque 2") {
    $('#orange-square').addClass('highlighted');
    $('#green-square').removeClass('highlighted');
    $('#triangle-container-orange').show();
    $('#advice-box-orange').show();
    $('#triangle-container-green').hide();
    $('#advice-box-green').hide();
  } else {
    $('#green-square').removeClass('highlighted');
    $('#orange-square').removeClass('highlighted');
    $('#triangle-container-green').hide();
    $('#advice-box-green').hide();
    $('#triangle-container-orange').hide();
    $('#advice-box-orange').hide();
  }
});

Shiny.addCustomMessageHandler('updateHistory', function(message) {
  $('.delete-btn').click(function() {
    Shiny.setInputValue('delete_row', this.id, {priority: "event"});
  });
});
