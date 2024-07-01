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

Shiny.addCustomMessageHandler('showModal', function(message) {
  $('#modalOverlay').show();
  $('#cuveModal').show();
});

Shiny.addCustomMessageHandler('hideModal', function(message) {
  $('#modalOverlay').hide();
  $('#cuveModal').hide();
});

$(document).on('click', '#submit_cuve_id', function() {
  $('#modalOverlay').hide();
  $('#cuveModal').hide();
});

$(document).on('click', '#cancel_cuve_id', function() {
  $('#modalOverlay').hide();
  $('#cuveModal').hide();
});

// Code pour le bouton de retour en haut

$(window).scroll(function() {
  if ($(this).scrollTop() > 200) { // Afficher le bouton après avoir fait défiler 200px
    $('#backToTop').fadeIn();
  } else {
    $('#backToTop').fadeOut();
  }
});

$('#backToTop').click(function() {
  $('html, body').animate({scrollTop: 0}, 800); // 800ms pour l'animation de défilement
  return false;
});

