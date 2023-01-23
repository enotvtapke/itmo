<#-- @ftlvariable name="uri" type="java.lang.String" -->
<#macro page>
    <!DOCTYPE html>
    <html lang="ru">
    <head>
        <meta charset="UTF-8">
        <title>Codeforces</title>
        <link rel="stylesheet" href="/css/normalize.css">
        <link rel="stylesheet" href="/css/style.css">
        <link rel="icon" href="/favicon.ico">
    </head>
    <body>
    <@header/>
    <div class="middle">
        <@sidebar/>
        <main>
            <#nested/>
        </main>
    </div>
    <@footer/>
    </body>
    </html>
</#macro>

<#macro header>
    <header>
        <a href="#" class="logo"><img alt="Codeforces" title="Codeforces"
                                      src="/img/codeforces-logo-with-telegram.png"></a>
        <div class="languages">
            <a href="#"><img alt="In English" title="In English"
                             src="/img/gb.png"></a>
            <a href="#"><img alt="In Russian" title="In Russian"
                             src="/img/ru.png"></a>
        </div>
        <div class="enter-or-register-box">
            <#if user??>
                <@userlink user true/>
                |
                <a href="/logout">Logout</a>
            <#else>
                <a href="/enter">Enter</a>
                |
                <a href="/register">Register</a>
            </#if>
        </div>
        <nav>
            <#assign items=[
            {"href": "/index", "name": "Home"},
            {"href": "/misc/help", "name": "Help"},
            {"href": "/contests", "name": "Contests"},
            {"href": "/users", "name": "Users"}
            ]/>
            <ul>
                <#list items as item>
                    <li>
                        <a href="${item.href}" <#if uri == item.href>class="currentTab"</#if>>${item.name}</a>
                    </li>
                </#list>
            </ul>
        </nav>
    </header>
</#macro>

<#macro sidebar>
    <aside>
        <#list posts as p>
            <@sidebarPost p/>
        </#list>
    </aside>
</#macro>

<#macro sidebarPost post>
    <section>
        <div class="header">Post #${post.id}</div>
        <div class="body"><@trimToSize post.text 250/></div>
        <div class="footer">
            <a href="/post?post_id=${post.id}">View all</a>
        </div>
    </section>
</#macro>

<#macro footer>
    <footer>
        2010-2021 &copy; by Mike Mirzayanov
    </footer>
</#macro>

<#macro userlink user nameOnly=false>
<#-- @ftlvariable name="user" type="ru.itmo.web.lesson4.model.User" -->
    <a <#if !nameOnly>style="color: ${user.color}"</#if> href="/user?handle=${user.handle}" class="user-link">${user.name}</a>
</#macro>

<#macro trimToSize text size>
    <#if (text?length > size)>
        ${text?substring(0, size)}&hellip;
    </#if>
</#macro>

<#macro post post short=true>
    <article>
        <div class="title"><a class="title-anchor" href="/post?post_id=${post.id}">${post.title}</a></div>
        <#assign u = findBy(users, "id", post.userId)!/>
        <div class="information">By <@userlink u/>, 2 days ago, translation</div>
        <div class="body">
            <#if short && (post.text?length > 250)>
                <@trimToSize post.text 250/>
            <#else>
                ${post.text}
            </#if>
        </div>
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
</#macro>

<#function findBy items key value>
    <#list items as item>
        <#if item[key] == value>
            <#return item/>
        </#if>
    </#list>
</#function>

<#function next items item>
    <#if items?seq_index_of(item) < items?size - 1>
        <#return items[items?seq_index_of(item) + 1]/>
    </#if>
</#function>

<#function prev items item>
    <#if items?seq_index_of(item) != 0>
        <#return items[items?seq_index_of(item) - 1]/>
    </#if>
</#function>
