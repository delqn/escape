$('button').on('click', function(event) {
  window.console.log(this);
  window.location.href = $(this).attr("href");
});