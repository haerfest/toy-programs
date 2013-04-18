$(document).ready(function () {

    //
    // Show all events that occurred at or right before #rec_time_micros.
    //
    var showEventsAtTime = function (rec_time_micros) {
        $("#slider").slider("disable");

        $.getJSON('api/get-events-at-time',
                  {'rec_time_micros': rec_time_micros},
                  function (data) {
                      for (var index in data) {
                          var id    = "image" + data[index].asset_management_id;
                          var image = $("#" + id);
                
                          if (image.length === 0) {
                              var div = $("<div/>");
                              image   = $("<img/>", {"id": id, "style": "width:100%"});
                              
                              image.appendTo(div);
                              div.appendTo(images);
                          }

                          image.attr('src', 'data:image/jpeg;base64,' + data[index].image);
                          image.attr('hidden', false);
                      }
                      $("#slider").slider("enable");
                  });
    };

    //
    // Fetch time range and initialize time slider.
    //
    $.getJSON('/api/get-time-range', function (data) {
        var minimum = data.minimum;
        var maximum = data.maximum;
        
        $("#slider_min_value").text(new Date(minimum / 1E3));
        $("#slider_max_value").text(new Date(maximum / 1E3));
        $("#slider_value").text(new Date(minimum / 1E3));
 
        $("#slider").slider({ min: minimum ,
                              max: maximum,
                              slide: function (event, ui) {
                                  $("#slider_value").text(new Date(ui.value / 1E3));
                              },
                              change: function (event, ui) {
                                  showEventsAtTime(ui.value);
                              }});
    });

    var regnum          = $("#regnum");
    var rec_time_micros = $("#rec_time_micros");
    var prev            = $("#prev");
    var next            = $("#next");
    var images          = $("#images");

    prev.click(function () {
        $.get('api/prev-event', {'rec_time_micros': rec_time_micros.val()}, function (data) {
            rec_time_micros.empty();
            rec_time_micros.append($('<option></option>').attr("value", data.rec_time_micros).text(data.title));
            rec_time_micros.trigger('change');
        });
    });

    next.click(function () {
        $.get('api/next-event', {'rec_time_micros': rec_time_micros.val()}, function (data) {
            rec_time_micros.empty();
            rec_time_micros.append($('<option></option>').attr("value", data.rec_time_micros).text(data.title));
            rec_time_micros.trigger('change');
        });
    });


    regnum.keydown(function (e) {
        var code = e.which;

        if (code === 13) {
            e.preventDefault();
            regnum.attr('disabled', true);
            
            $.getJSON('api/find-regnum', {'regnum': regnum.val()}, function (data) {
                rec_time_micros.empty();
                $.each(data, function () {
                    rec_time_micros.append($('<option></option>').attr("value", this.rec_time_micros).text(this.title));
                });
                rec_time_micros.trigger('change');

                regnum.removeAttr('disabled');
                rec_time_micros.removeAttr('disabled');
            });
        }
    });
});