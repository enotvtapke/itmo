<#import "commons.ftlh" as c>

<@c.page>
    <div class="users datatable">
        <div class="caption">User</div>
        <table>
            <thead>
            <tr>
                <th>Id</th>
                <th>Login</th>
                <th>Created</th>
                <th>Disabled</th>
            </tr>
            </thead>
            <tbody>
            <#if users?size==0>
                <tr class="noData">
                    <td colspan="3">
                        No data
                    </td>
                </tr>
            <#else>
                <#list users as user>
                <tr>
                    <td class="id">${user.id?c}</td>
                    <td class="login">${user.login}</td>
                    <td class="creationTime">${user.creationTime}</td>
                    <td class="user-disabled">
                        <form method="post" action="">
                            <input type="hidden" name="action" value="disable">
                            <input type="hidden" name="id" value="${user.id?c}">
                            <input type="hidden" name="disabled" value="<#if user.disabled>0<#else>1</#if>">
                            <input class="enable-disable-button" type="submit" value="<#if user.disabled>Enable<#else>Disable</#if>">
                        </form>
                    </td>
                </tr>
                </#list>
            </#if>
            </tbody>
        </table>
    </div>
</@c.page>
