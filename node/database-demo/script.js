$(document).ready(function () {

    var regnum          = $("#regnum");
    var rec_time_micros = $("#rec_time_micros");
    var image           = $("#image");

    rec_time_micros.attr('disabled', true);
    rec_time_micros.change(function () {
        $.get('api/event-at-time', {'rec_time_micros': this.value}, function (data) {
            image.attr('src', 'data:image/jpeg;base64,' + data);
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
                    rec_time_micros.append($('<option></option>').text(this));
                });

                regnum.removeAttr('disabled');
                rec_time_micros.removeAttr('disabled');
            });
        }
    });
});