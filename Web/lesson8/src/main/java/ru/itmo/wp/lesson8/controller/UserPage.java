package ru.itmo.wp.lesson8.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import ru.itmo.wp.lesson8.domain.User;
import ru.itmo.wp.lesson8.service.UserService;

import javax.servlet.http.HttpSession;
import java.util.Optional;

@Controller
public class UserPage extends Page {
    private final UserService userService;

    public UserPage(UserService userService) {
        this.userService = userService;
    }

    @GetMapping(path = {"/user/{id}"})
    public String getUser(@PathVariable String id, Model model) {
        try {
            User user = userService.findById(Long.parseLong(id));
            model.addAttribute("accountUser", user);
            return "UserPage";
        } catch (NumberFormatException e) {
            return "UserPage";
        }
    }
}
