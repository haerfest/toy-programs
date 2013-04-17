$(document).ready(function () {

    var regnum          = $("#regnum");
    var rec_time_micros = $("#rec_time_micros");
    var image           = $("#image");

    rec_time_micros.attr('disabled', true);
    rec_time_micros.change(function () {
        if (this.value) {
            $.get('api/event-at-time', {'rec_time_micros': this.value}, function (data) {
                image.attr('src', 'data:image/jpeg;base64,' + data);
                image.attr('hidden', false);
            });
        } else {
            image.attr('src', '');
            image.attr('hidden', true);
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