$(document).ready(function () {

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

    rec_time_micros.change(function () {
        if (this.value) {
            $.getJSON('api/event-at-time', {'rec_time_micros': this.value}, function (data) {
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
            });
        }
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