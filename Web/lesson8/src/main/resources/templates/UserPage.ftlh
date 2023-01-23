<#import "commons.ftlh" as c>

<@c.page>
    <#if accountUser??>
        <div class="user-login">${accountUser.login}</div>
        <div class="user-id">${accountUser.id}</div>
        <div class="user-creationTime">${accountUser.creationTime}</div>
    <#else>
        No such user
    </#if>
</@c.page>
