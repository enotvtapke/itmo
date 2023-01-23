package ru.itmo.wp.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.*;
import ru.itmo.wp.domain.Comment;
import ru.itmo.wp.domain.Post;
import ru.itmo.wp.domain.Role;
import ru.itmo.wp.security.AnyRole;
import ru.itmo.wp.security.Guest;
import ru.itmo.wp.service.PostService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class PostPage extends Page {
    private final PostService postService;

    public PostPage(PostService postService) {
        this.postService = postService;
    }

    @Guest
    @GetMapping(path = {"/post/{id}"})
    public String postGet(@PathVariable String id, Model model) {
        try {
            Post post = postService.findById(Long.parseLong(id));
            model.addAttribute("post", post);
            model.addAttribute("comment", new Comment());
            return "PostPage";
        } catch (NumberFormatException e) {
            return "PostPage";
        }
    }

    @PostMapping(path = {"/post/{id}"})
    public String writeCommentPost(@PathVariable String id,
                            Model model,
                            @Valid @ModelAttribute("comment") Comment comment,
                            BindingResult bindingResult,
                            HttpSession httpSession) {
        try {
            Post post = postService.findById(Long.parseLong(id));
            if (post == null) {
                return "redirect:/";
            }
            model.addAttribute("post", post);
            if (bindingResult.hasErrors()) {
                return "PostPage";
            }
            comment.setUser(getUser(httpSession));
            postService.writeComment(post, comment);
            putMessage(httpSession, "You leave new comment");

            return "redirect:/post/" + post.getId();
        } catch (NumberFormatException e) {
            return "PostPage";
        }
    }

    //    @PostMapping(path = {"/leaveComment"})
//    public String postPost(Model model,
//                           @Valid @ModelAttribute("comment") Comment comment,
//                           BindingResult bindingResult,
//                           HttpSession httpSession) {
//        Post post = (Post) model.getAttribute("post");
//        if (post == null) {
//            return "redirect:/";
//        }
//        model.addAttribute("post", post);
//        if (bindingResult.hasErrors()) {
//            return "redirect:/post/" + post.getId();
//        }
//        try {
//            comment.setUser(getUser(httpSession));
//        } catch (Throwable e) {
//            putMessage(httpSession, "Sasi hacker");
//            return "redirect:/post/" + post.getId();
//        }
//        postService.writeComment(post, comment);
//        putMessage(httpSession, "You leave new comment");
//
//        return "redirect:/post/" + post.getId();
//    }
}
