package ru.itmo.wp.web.page;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.service.UserService;

public abstract class Page {
    protected final UserService userService = new UserService();

    public void before(HttpServletRequest request, Map<String, Object> view) {
        view.put("userCount", userService.count());
        view.put("user", request.getSession().getAttribute("user"));
        view.put("message", request.getSession().getAttribute("message"));
    }

    User getUser(HttpServletRequest request) {
        return (User) request.getSession().getAttribute("user");
    }

    void setUser(HttpServletRequest request, User user) {
        request.getSession().setAttribute("user", user);
    }

    void setMessage(HttpServletRequest request, String message) {
        request.getSession().setAttribute("message", message);
    }

    public void action(HttpServletRequest request, Map<String, Object> view) {

    }

    public void after(HttpServletRequest request, Map<String, Object> view) {

    }
}
