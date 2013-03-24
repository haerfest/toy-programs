$(document).ready(function () {
    $.ajax({
        url: "api/getFileList",
        success: function (data) {
            var files = JSON.parse(data);
            $("#filelist").append(
                files.map(
                    function (file) {
                        return $('<li></li>').append(file);
                    })
            );
        }
    });
});
