<#-- @ftlvariable name="user" type="ru.itmo.wp.model.domain.User" -->
<#import "commons.ftlh" as c>

<@c.page>
    <div class="form-box article-form">
        <div class="header">New article</div>
        <div class="body">
            <form method="post" action="">
                <input type="hidden" name="action" value="save"/>
                <div class="field">
                    <div class="name">
                        <label for="article-title">Title</label>
                    </div>
                    <div class="value">
                        <input name="title" id="article-title"/>
                    </div>
                </div>
                <div class="field">
                    <div class="name">
                        <label for="text">Text</label>
                    </div>
                    <div class="value">
                        <textarea name="text" id="text" rows="5"></textarea>
                    </div>
                </div>
                <div class="error"></div>
                <div class="button-field">
                    <input type="submit" value="Enter">
                </div>
            </form>
        </div>
    </div>

    <script>
        $(function () {
            $(".article-form form").submit(function () {
                const userId = ${user.id};
                const title = $(this).find("input[name='title']").val();
                const text = $(this).find("textarea[name='text']").val();
                const $error = $(this).find(".error");
                const button = $(this).find("input[type='submit']");
                button.attr("disabled", true);

                ajax({
                    url: "/article",
                    data: {
                        action: "save",
                        userId,
                        title,
                        text
                    },
                    success: function (response) {
                        if (response["error"]) {
                            $error.text(response["error"]);
                        } else {
                            $error.text("");
                            notify("Article saved")
                        }
                        button.attr("disabled", false);
                    }
                });

                return false;
            });
        })
    </script>
</@c.page>
