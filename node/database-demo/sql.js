$(function () {
    //
    // Handle the user typing a registration number.
    //
    $("#query").keydown(function (e) {
        var code = e.which;

        if (code === 13) {
            e.preventDefault();
            $("#query").attr("disabled", true);

            $.getJSON("api/query",
                      {"query": $("#query").val()},
                      function (rows) {
                          $("#result").empty();
                          $("#query").removeAttr("disabled");
                          
                          var table = $("<table/>"),
                              got_header = false;
                          
                          rows.forEach(function (row) {
                              var tr = $("<tr/>"),
                                  key;

                              if (!got_header) {
                                 for (key in row) {
                                      var th = $("<th/>", {text: key});
                                      th.appendTo(tr);
                                  }
                                  tr.appendTo(table);
                                  tr = $("<tr/>");

                                  got_header = true;
                              }

                              for (key in row) {
                                  var a  = (key === "image" ? $("<a/>", {text: "image", href: "/api/get-trigger-image/" + row.image, target: "_blank"}) : null),
                                      td = (a ? $("<td/>") : $("<td/>", {text: row[key]}));

                                  if (a) {
                                      a.appendTo(td);
                                  }
                                  
                                  td.appendTo(tr);
                              }

                              tr.appendTo(table);
                          });

                          table.appendTo("#result");
                      });
        }
    });
});