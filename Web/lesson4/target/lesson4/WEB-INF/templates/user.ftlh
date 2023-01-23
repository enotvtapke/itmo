<#-- @ftlvariable name="handle" type="java.lang.String" -->
<#-- @ftlvariable name="user_id" type="java.lang.Long" -->

<#import "./common.ftlh" as c>

<@c.page>
    <#if handle??>
        <#assign u = c.findBy(users, "handle", handle)!/>
    <#elseif user_id?? && user_id?is_number>
        <#assign u = c.findBy(users, "id", user_id)!/>
    </#if>

    <#if u?has_content>
        <div class="user">
            <div class="user-info">
                <h2><@c.userlink u/></h2>
                <ul>
                    <li>id: ${u.id}</li>
                    <li>handle: ${u.handle}</li>
                    <li>name: ${u.name}</li>
                </ul>
                <a class="user-posts-link" href="/posts?user_id=${u.id}">Posts: ${posts?filter(p -> p["userId"] == u.id)?size}</a>
            </div>
            <img class="user-image" src="/img/profile-image.jpg" alt="Profile image">
        </div>
        <div class="user-next-prev-link">
            <#assign prevUser = c.prev(users, u)!/>
            <#if prevUser?has_content>
                <a class="prev-user" href="/user?handle=${prevUser.handle}">←</a>
            <#else>
                <span class="prev-user inactive">←</span>
            </#if>
            <#assign nextUser = c.next(users, u)!/>
            <#if nextUser?has_content>
                <a class="next-user" href="/user?handle=${nextUser.handle}">→</a>
            <#else>
                <span class="next-user inactive">→</span>
            </#if>
        </div>
    <#else>
        No such user
    </#if>
</@c.page>
