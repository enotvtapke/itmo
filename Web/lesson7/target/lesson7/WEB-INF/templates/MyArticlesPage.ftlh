<#-- @ftlvariable name="articles" type="ru.itmo.wp.model.domain.Article[]" -->
<#import "commons.ftlh" as c>

<@c.page>
    <div class="datatable">
        <div class="caption">Article</div>
        <table>
            <thead>
            <tr>
                <th>Id</th>
                <th>Title</th>
                <th>Hidden</th>
            </tr>
            </thead>
            <tbody>
            <#list articles as article>
            <tr>
                <td>${article.id}</td>
                <td>${article.title}</td>
                <td>
                    <input class="show-hide-button" type="button" data-articleId="${article.id}" value="<#if article.hidden == true>Show<#else>Hide</#if>">
                </td>
            </tr>
            </#list>
            </tbody>
        </table>
    </div>

    <script>
        $(function () {
            $(".show-hide-button").on("click", function () {
                const $button = $(this);
                const hidden = ($(this).attr("value") === "Show");
                const articleId = $(this).attr("data-articleId");
                //const articleUserId = $(this).attr("data-userId");
                //const $error = $(this).find(".error");
                console.log(articleId);
                ajax({
                    url: "",
                    data: {
                        action: "updateHidden",
                        articleId: articleId,
                        //userId: articleUserId,
                        hidden: !hidden
                    },
                    success: function (response) {
                        if (response["error"]) {
                            notify(response["error"]);
                            //$error.text(response["error"]);
                        } else {
                            if (hidden) {
                                $button.val("Hide");
                                notify("Article shown!");
                            } else {
                                $button.val("Show");
                                notify("Article hidden!");
                            }
                        }
                    }
                });

                return false;
            });
        })
    </script>
</@c.page>
