<#-- @ftlvariable name="user_id" type="java.lang.Long" -->
<#import "./common.ftlh" as c>

<@c.page>
    <#if user_id?? && user_id?is_number>
        <#assign user_posts = posts?filter(p -> p["userId"] == user_id)/>
        <#list posts?filter(p -> p["userId"] == user_id) as p>
            <@c.post p/>
        </#list>
    </#if>
    <#if !user_posts?? || user_posts?size == 0>
        <div>
            No posts
        </div>
    </#if>
</@c.page>