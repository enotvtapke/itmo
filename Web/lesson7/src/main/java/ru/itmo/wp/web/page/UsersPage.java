package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.service.UserService;
import ru.itmo.wp.web.annotation.Json;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

/** @noinspection unused*/
public class UsersPage {
    private final UserService userService = new UserService();

    private void action(HttpServletRequest request, Map<String, Object> view) {
        // No operations.
    }

    private void findAll(HttpServletRequest request, Map<String, Object> view) {
        view.put("users", userService.findAll());
    }

    private void findUser(HttpServletRequest request, Map<String, Object> view) {
        view.put("user",
                userService.find(Long.parseLong(request.getParameter("userId"))));
    }

    private void updateAdmin(HttpServletRequest request, Map<String, Object> view) {
        User user = userService.find(((User) request.getSession().getAttribute("user")).getId());
        if (user.isAdmin()) {
            long userId = Long.parseLong(request.getParameter("userId"));
            if (userService.find(userId) == null) {
                view.put("error", "Wrong user");
            } else {
                boolean admin = Boolean.parseBoolean(request.getParameter("admin"));
                userService.updateAdminById(userId, admin);
            }
        } else {
            view.put("error", "You are not an admin");
        }
    }
}
