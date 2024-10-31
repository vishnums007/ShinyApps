$(document).on('shiny:value', function(event) {
  // Scroll down after model update
  if (event.target.id === 'nursery_add_conf_data') {
    window.scrollTo(0,document.body.scrollHeight);
  }
});
$(document).on('shiny:value', function(event) {
  // Scroll down after model update
  if (event.target.id === 'adult_add_conf_data') {
    window.scrollTo(0,document.body.scrollHeight);
  }
});