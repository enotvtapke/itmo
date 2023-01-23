window.notify = function (message) {
    $.notify(message, {
        position: "right bottom",
        className: "success"
    });
}

window.ajax = function ({url, data, success}) {
    $.ajax({
        type: "POST",
        url,
        dataType: "json",
        data,
        success: function (response) {
            success(response);
            if (response["redirect"]) {
                location.href = response["redirect"];
            }
        }
    });
}
