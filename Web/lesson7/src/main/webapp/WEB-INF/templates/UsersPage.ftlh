<#-- @ftlvariable name="user" type="ru.itmo.wp.model.domain.User" -->
<#import "commons.ftlh" as c>

<@c.page>
    <div class="users datatable">
        <div class="caption">User</div>
        <table>
            <thead>
            <tr>
                <th>Id</th>
                <th>Login</th>
                <#if user?? && user.admin == true>
                <th>Admin</th>
                </#if>
            </tr>
            </thead>
            <tbody>
            <template>
                <tr>
                    <td class="user-id"></td>
                    <td class="user-login"></td>
                    <#if user?? && user.admin == true>
                    <td class="user-admin">
                        <input class="enable-disable-button" type="checkbox">
                    </td>
                    </#if>
                </tr>
            </template>
            <tr class="noData">
                <td colspan="3">
                    No data
                </td>
            </tr>
            </tbody>
        </table>
    </div>

    <script>
        $(function () {
            const $noData = $(".users .noData");
            $noData.find("td").text("Loading...");
            ajax({
                url: "",
                data: {
                    action: "findAll"
                },
                success: function(response) {
                    response["users"].forEach(function(user) {
                        $noData.hide();
                        const $tr = $($(".users template").prop("content")).clone();
                        $tr.find(".user-id").text(user["id"]);
                        $tr.find(".user-login").text(user["login"]);
                        $tr.find("tr").attr("data-userId", user["id"]);
                        const $button = $tr.find(".enable-disable-button");
                        $button.attr("data-admin", user["admin"]);
                        $button.attr("data-userId", user["id"]);
                        if (user["admin"]) {
                            $button.attr("checked", true);
                        } else {
                            $button.attr("checked", false);
                        }
                        $(".users tbody").append($tr);
                    });
                    $(".users td .enable-disable-button").click(function() {
                        const $button = $(this);
                        $button.attr("disabled", true);
                        const admin = $button.attr("data-admin") === "true";
                        ajax({
                            data: {
                                action: "updateAdmin",
                                userId: $button.attr("data-userId"),
                                admin: !admin
                            },
                            success: function (response) {
                                if (response["error"]) {
                                    notify(response["error"]);
                                    //$error.text(response["error"]);
                                } else {
                                    notify("Admin for " + $button.attr("data-userId") + " changed to " + !admin);
                                    $button.attr("data-admin", !admin);
                                    if (admin) {
                                        $button.attr("checked", true);
                                    } else {
                                        $button.attr("checked", false);
                                    }
                                    $button.attr("disabled", false);
                                }
                            }
                        });
                    });
                    $(".users tr td:not(.user-admin)").click(function() {
                        ajax({
                            data: {
                                action: "findUser",
                                userId: $(this).parent().attr("data-userId")
                            },
                            success: function (response) {
                                notify(response["user"].creationTime);
                            }
                        });
                    });
                }
            });

        })
    </script>
</@c.page>
