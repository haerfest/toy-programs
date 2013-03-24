$(document).ready(function () {
    $.ajax({
        url: 'api/getFileList',
        success: function (files) {
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
        success: function (names) {
            $("#namelist").append(
                names.map(
                    function (name) {
                        return $('<li></li>').append(name);
                    })
            );
        }
    });
});
