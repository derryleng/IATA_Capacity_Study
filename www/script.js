var dimension = [0, 0];

$(document).on("shiny:connected", function(e) {
  dimension[0] = $(".content-wrapper").width();
  dimension[1] = $(".content-wrapper").height();
  Shiny.onInputChange("dimension", dimension);
});

$(window).resize(function(e) {
  dimension[0] = $(".content-wrapper").width();
  dimension[1] = $(".content-wrapper").height();
  Shiny.onInputChange("dimension", dimension);
});
