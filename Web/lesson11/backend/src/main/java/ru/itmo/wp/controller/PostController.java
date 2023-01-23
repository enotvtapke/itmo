package ru.itmo.wp.controller;

import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.*;
import ru.itmo.wp.domain.Post;
import ru.itmo.wp.domain.User;
import ru.itmo.wp.exception.ValidationException;
import ru.itmo.wp.form.PostForm;
import ru.itmo.wp.form.validator.PostFormWriteValidator;
import ru.itmo.wp.service.JwtService;
import ru.itmo.wp.service.PostService;
import ru.itmo.wp.service.UserService;

import javax.validation.Valid;
import java.util.List;

@RestController
@RequestMapping("/api/1")
public class PostController {
    private final PostService postService;
    private final UserService userService;
    private final JwtService jwtService;
    private final PostFormWriteValidator postFormWriteValidator;

    public PostController(PostService postService, UserService userService, JwtService jwtService, PostFormWriteValidator postFormWriteValidator) {
        this.postService = postService;
        this.userService = userService;
        this.jwtService = jwtService;
        this.postFormWriteValidator = postFormWriteValidator;
    }

    @InitBinder("postForm")
    public void initBinder(WebDataBinder binder) {
        binder.addValidators(postFormWriteValidator);
    }


    @GetMapping("posts")
    public List<Post> findPosts() {
        return postService.findAll();
    }

    @PostMapping("posts")
    public Post writePostPost(@RequestBody @Valid PostForm postForm,
                              BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new ValidationException(bindingResult);
        }

        Post post = new Post();

        post.setText(postForm.getText());
        post.setTitle(postForm.getTitle());
        userService.writePost(jwtService.find(postForm.getJwt()), post);

        return post;
    }
}
