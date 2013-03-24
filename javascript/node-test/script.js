$(document).ready(function () {
    $.ajax({
        url: 'api/getFileList',
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

    $.ajax({
        url: 'api/getNameList',
        success: function (data) {
            var names = JSON.parse(data);
            $("#namelist").append(
                names.map(
                    function (name) {
                        return $('<li></li>').append(name);
                    })
            );
        }
    });
});
