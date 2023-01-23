<#import "commons.ftlh" as c>

<@c.page>
    <div class="articles">
        <div class="noData">No data</div>
        <template>
            <article>
                <div class="title"></div>
                <div class="information">By <span></span>, 2 days ago, translation</div>
                <div class="body"></div>
                <ul class="attachment">
                    <li>Announcement of <a href="#">Codeforces Round #510 (Div. 1)</a></li>
                    <li>Announcement of <a href="#">Codeforces Round #510 (Div. 2)</a></li>
                </ul>
                <div class="footer">
                    <div class="left">
                        <img src="img/voteup.png" title="Vote Up" alt="Vote Up"/>
                        <span class="positive-score">+173</span>
                        <img src="img/votedown.png" title="Vote Down" alt="Vote Down"/>
                    </div>
                    <div class="right">
                        <img src="img/date_16x16.png" title="Publish Time" alt="Publish Time"/>
                        2 days ago
                        <img src="img/comments_16x16.png" title="Comments" alt="Comments"/>
                        <a href="#">68</a>
                    </div>
                </div>
            </article>
        </template>
    </div>

    <script>
        $(function () {
            const $noData = $(".articles .noData");
            $noData.text("Loading...");
            ajax({
                url: "/article",
                data: {
                    action: "findAllNotHiddenWithAuthors"
                },
                success: function(response) {
                    $noData.hide();
                    response["articles"].forEach(function(articleWithAuthor) {
                        const $article = $($(".articles template").prop("content")).clone();
                        const article = articleWithAuthor["article"];
                        const author = articleWithAuthor["author"];
                        $article.find(".title").text(article["title"]);
                        $article.find(".information span").text(author);
                        $article.find(".body").text(article["text"]);
                        $(".articles").append($article);
                        // ajax({
                        //     url: "/users",
                        //     data: {
                        //         action: "findUser",
                        //         userId: article.userId
                        //     },
                        //     success: function(user) {
                        //         const $article = $($(".articles template").prop("content")).clone();
                        //         $article.find(".title").text(article["title"]);
                        //         $article.find(".information span").text(user.user.login);
                        //         $article.find(".body").text(article["text"]);
                        //         $(".articles").append($article);
                        //     }
                        // });
                    });
                }
            });

        })
    </script>
</@c.page>
