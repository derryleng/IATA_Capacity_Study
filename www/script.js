var dimension = [0, 0];

$(document).on("shiny:connected", function() {
  dimension[0] = $(".content-wrapper").width();
  dimension[1] = $(".content-wrapper").height();
  Shiny.onInputChange("dimension", dimension);
});

var rtime;
var timeout = false;
var delta = 100;

$(window).resize(function() {
  rtime = new Date();
  if (timeout === false) {
    timeout = true;
    setTimeout(resizeend, delta);
  }
  if ($(window).width() < 768) {
    Shiny.onInputChange("legend", false);
  } else {
    Shiny.onInputChange("legend", true);
  }
});

function resizeend() {
  if (new Date() - rtime < delta) {
    setTimeout(resizeend, delta);
  } else {
    timeout = false;
    dimension[0] = $(".content-wrapper").width();
    dimension[1] = $(".content-wrapper").height();
    Shiny.onInputChange("dimension", dimension);
  }
}