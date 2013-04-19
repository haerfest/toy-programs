$(function () {

    //
    // Shortcuts for various elements.
    //
    var regnum          = $("#regnum");
    var rec_time_micros = $("#rec_time_micros");
    var images          = $("#images");
    var slider          = $("#slider");

    //
    // Get a reference to the 2D context of the canvas and its dimensions.
    //
    var canvas    = $("#canvas")[0],
        ctx       = canvas.getContext("2d"),
        width     = canvas.width,
        height    = canvas.height,
        width_m   = 2 * 24,
        height_m  = 2 * 6,
        alpr_m    = 12,
        marker_px = 7;

    var drawTarmac = function () {

        ctx.save();
 
        //
        // Fill it with a tarmac-like color.
        //
        ctx.fillStyle = "#333";
        ctx.fillRect(0, 0, width, height);

        //
        // Draw the ALPR markers.
        //
        ctx.strokeStyle = "yellow";
        ctx.beginPath();
        ctx.moveTo(width / 2 - width * alpr_m / width_m, 0);
        ctx.lineTo(width / 2 - width * alpr_m / width_m, height);
        ctx.moveTo(width / 2 + width * alpr_m / width_m, 0);
        ctx.lineTo(width / 2 + width * alpr_m / width_m, height);
        ctx.stroke();

        //
        // Prepare to draw the axis.
        //
        ctx.strokeStyle = "#777";
        ctx.lineWidth = 1;
        ctx.beginPath();

        //
        // Draw horizontal axis.
        //
        ctx.moveTo(0, height / 2);
        ctx.lineTo(width, height / 2);

        //
        // Draw horizontal markers.
        //
        for (var x = 0; x <= width / 2; x += width / width_m) {
            ctx.moveTo(width / 2 - x, height / 2 - marker_px / 2);
            ctx.lineTo(width / 2 - x, height / 2 + marker_px / 2);
            ctx.moveTo(width / 2 + x, height / 2 - marker_px / 2);
            ctx.lineTo(width / 2 + x, height / 2 + marker_px / 2);
        }
        
        //
        // Draw vertical axis.
        //
        ctx.moveTo(width / 2, 0);
        ctx.lineTo(width / 2, height);

        //
        // Draw vertical markers.
        //
        for (var y = 0; y <= height / 2; y += height / height_m) {
            ctx.moveTo(width / 2 - marker_px / 2, height / 2 - y);
            ctx.lineTo(width / 2 + marker_px / 2, height / 2 - y);
            ctx.moveTo(width / 2 - marker_px / 2, height / 2 + y);
            ctx.lineTo(width / 2 + marker_px / 2, height / 2 + y);
        }
        ctx.stroke();

        //
        // Axis legendas.
        //
        ctx.font = "10px Arial";
        ctx.fillStyle = "#999";
        
        ctx.textBaseline = "top";    
        ctx.textAlign = "right";
        ctx.fillText("x +" + (width_m / 2), width, height / 2 + marker_px / 2);
        ctx.textAlign = "left";
        ctx.fillText("-" + (width_m / 2), 0, height / 2 + marker_px / 2);

        ctx.textAlign = "right";
        ctx.textBaseline = "top";
        ctx.fillText("y +" + (height_m / 2), width / 2 - marker_px / 2, 0);
        ctx.textBaseline = "bottom";
        ctx.fillText("-" + (height_m / 2), width / 2 - marker_px / 2, height);

        ctx.restore();
    };

    drawTarmac();

    //
    // Set the canvas origin to the center.
    //
    ctx.translate(width / 2, height / 2);

    //
    // Scale it to be able to use physical units (meters) and
    // orient the x-axis horizontally increasing to the right, and
    // the y-axis vertically increasing upwards.
    //
    ctx.scale(width / width_m , -height / height_m);

    //
    // Draw a red circle in the center.
    //
    ctx.strokeStyle = "red";
    ctx.lineWidth = 0.1;
    ctx.beginPath();
    ctx.arc(0, 0, 0.1, 0, 2 * Math.PI, true);
    ctx.stroke();

    //
    // And a cyan one at x = 10m and y = 4 m.
    //
    ctx.strokeStyle = "cyan";
    ctx.beginPath();
    ctx.arc(10, 4, 0.1, 0, 2 * Math.PI, true);
    ctx.stroke();

    //
    // Show all events that occurred at or right before #rec_time_micros.
    //
    var showEventsAtTime = function (rec_time_micros) {
        slider.slider("disable");

        $.getJSON('api/get-events-at-time',
                  {'rec_time_micros': rec_time_micros},
                  function (data) {
                      for (var index in data) {
                          var asset_id = "image" + data[index].asset_management_id;
                          var image    = $("#" + asset_id);
                
                          if (image.length === 0) {
                              var div = $("<div/>");
                              image   = $("<img/>", {"id": asset_id, "style": "width:100%"});
                              
                              image.appendTo(div);
                              div.appendTo(images);
                          }

                          image.attr('src', "api/get-image/" + data[index].id);
                          image.attr('hidden', false);
                      }
                      slider.slider("enable");
                  });
    };

    //
    // Update the slider if the user selects a new time from the drop-down box.
    //
    rec_time_micros.change(function() {
        if (this.value) {
            slider.slider("value", this.value);
        }
    });
    
    //
    // Fetch time range and initialize time slider.
    //
    $.getJSON('/api/get-time-range', function (data) {
        var minimum = data.minimum;
        var maximum = data.maximum;
        
        $("#slider_min_value").text(new Date(minimum / 1E3));
        $("#slider_max_value").text(new Date(maximum / 1E3));
        $("#slider_value").text(new Date(minimum / 1E3));
 
        slider.slider({ min: minimum ,
                        max: maximum,
                        slide: function (event, ui) {
                            $("#slider_value").text(new Date(ui.value / 1E3));
                        },
                        change: function (event, ui) {
                            $("#slider_value").text(new Date(ui.value / 1E3));
                            showEventsAtTime(ui.value);
                        }});
    });

    regnum.keydown(function (e) {
        var code = e.which;

        if (code === 13) {
            e.preventDefault();
            regnum.attr("disabled", true);
            slider.slider("disable");
            
            $.getJSON('api/find-regnum', {'regnum': regnum.val()}, function (data) {
                rec_time_micros.empty();
                $.each(data, function () {
                    rec_time_micros.append($('<option></option>').attr("value", this.rec_time_micros).text(new Date(this.rec_time_micros / 1E3)));
                });
                regnum.attr("disabled", false);
                slider.slider("enable");
                rec_time_micros.trigger("change");
            });
        }
    });
});