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
    var canvas       = $("#canvas")[0],
        ctx          = canvas.getContext("2d"),
        width        = canvas.width,
        height       = canvas.height,
        width_m      = 2 * 24,
        height_m     = 2 * 12,
        alpr_m       = 12,
        marker_px    = 7,
        tracks       = [],
        track_colors = ["gray", "white"];

    var plotTarmac = function () {

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

    var setCanvasOrientationForTrackPlotting = function () {
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
    };

    var plotTrackEndpoints = function (start_x, start_y, end_x, end_y) {
        ctx.fillStyle = track_colors[0];
        ctx.beginPath();
        ctx.arc(start_x, start_y, 0.4, 0, 2 * Math.PI, true);
        ctx.fill();

        ctx.fillStyle = track_colors[1];
        ctx.beginPath();
        ctx.arc(end_x, end_y, 0.4, 0, 2 * Math.PI, true);
        ctx.fill();
    };

    var plotTrackSimple = function (track) {
        ctx.save();
        setCanvasOrientationForTrackPlotting();

        //
        // Draw a simple line between start and end points, which are resident
        // in memory.
        //
        var grad = ctx.createLinearGradient(track.start_x, track.start_y, track.end_x, track.end_y);

        grad.addColorStop(0, track_colors[0]);
        grad.addColorStop(1, track_colors[1]);

        ctx.strokeStyle = grad;            
        ctx.lineWidth   = 0.2;

        ctx.beginPath();
        ctx.moveTo(track.start_x, track.start_y);
        ctx.lineTo(track.end_x, track.end_y);
        ctx.stroke();

        plotTrackEndpoints(track.start_x, track.start_y, track.end_x, track.end_y);
        ctx.restore();
    };

    var plotTrackDetailed = function (track, now_msec) {
        //
        // We have to fetch more detailed track data first.
        //
        $.getJSON("/api/get-track/" + track.id,
                  function (positions) {
                      ctx.save();
                      setCanvasOrientationForTrackPlotting();

                      for (var i = 1; i < positions.length; i += 1) {
                          var percentage = 0.5 + i / (positions.length / 0.5),
                              intensity  = Math.floor(percentage * 255);

                          ctx.beginPath();
                          ctx.strokeStyle = "rgb(" + intensity + "," + intensity + "," + intensity + ")";

                          if (positions[i].time <= now_msec) {
                              ctx.lineWidth = 0.2;
                          } else {
                              ctx.lineWidth = 0.1;
                          }

                          ctx.moveTo(positions[i-1].x, positions[i-1].y);
                          ctx.lineTo(positions[i].x, positions[i].y);

                          ctx.stroke();
                      }

                      plotTrackEndpoints(track.start_x, track.start_y, track.end_x, track.end_y);
                      ctx.restore();
                  });
    };
 
    //
    // Show all events that occurred at or right before #rec_time_micros.
    //
    var showEventsAtTime = function (rec_time_micros) {
        slider.slider("disable");

        $.getJSON('api/get-images-at-time',
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

    var showTracksAtTime = function (now_msec, detailed) {
        var visibility_msec = 1 * 1E3,
            threshold_msec  = now_msec - visibility_msec;

        //
        // Clear the tarmac.
        //
        plotTarmac();

        //
        // Plot those tracks that still have a visible point. Don't plot
        // tracks that haven't started yet.
        //
        for (var t = 0; t < tracks.length; t +=1) {
            var track = tracks[t];

            if (track.start_time <= now_msec && track.end_time > threshold_msec) {
                if (detailed) {
                    plotTrackDetailed(track, now_msec);
                } else {
                    plotTrackSimple(track);
                }
            }
        }

        ctx.restore();
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
        var minimum = data.minimum / 1E3;
        var maximum = data.maximum / 1E3;
        
        $("#slider_min_value").text(new Date(minimum));
        $("#slider_max_value").text(new Date(maximum));
        $("#slider_value").text(new Date(minimum));
 
        slider.slider({min: minimum ,
                       max: maximum,
                       slide: function (event, ui) {
                           $("#slider_value").text(new Date(ui.value));
                           showTracksAtTime(ui.value, false);
                       },
                       change: function (event, ui) {
                           $("#slider_value").text(new Date(ui.value));
                           showTracksAtTime(ui.value, true);
                       }});
    });

    //
    // Fetch all track.
    //
    $.getJSON("/api/get-tracks",
              function (data) {
                  tracks = data;
                  plotTarmac();
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