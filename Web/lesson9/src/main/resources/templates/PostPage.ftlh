<#-- @ftlvariable name="comment" type="ru.itmo.wp.domain.Comment" -->
<#import "/spring.ftl" as spring>
<#import "commons.ftlh" as c>

<@c.page>
    <#if post??>
        <@c.post post=post/>
        <#if user??>
            <div class="comment form">
                <div class="header">Leave comment</div>
                <div class="body">
                    <form method="post" action="">
                        <input type="hidden" name="action" value="comment"/>
                        <div class="field">
                            <div class="name">
                                <label for="text">Text</label>
                            </div>
                            <div class="value">
                                <textarea autofocus id="text" name="text">${comment.text!}</textarea>
                            </div>
                            <@c.error "comment.text"/>
                        </div>
                        <div class="button-field">
                            <input type="submit" value="Leave comment">
                        </div>
                    </form>
                </div>
            </div>
        </#if>
        <#list post.comments as comment>
            <@c.comment comment=comment/>
        </#list>
    <#else>
        No such post
    </#if>
</@c.page>

