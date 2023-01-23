<#-- @ftlvariable name="post" type="ru.itmo.web.lesson4.model.Post" -->
<#-- @ftlvariable name="post_id" type="java.lang.Long" -->

<#import "./common.ftlh" as c>

<@c.page>
    <#assign post = c.findBy(posts, "id", post_id)!/>
    <#if post?has_content>
        <@c.post post=post short=false/>
    <#else>
        No such post
    </#if>
</@c.page>
