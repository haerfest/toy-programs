$(document).ready(function () {
    $.ajax({
        url: "api/getFileList",
        success: function (data) {
            var list = JSON.parse(data);

            $("#filelist").append(list.join('<br>'));
        }
    });
});
