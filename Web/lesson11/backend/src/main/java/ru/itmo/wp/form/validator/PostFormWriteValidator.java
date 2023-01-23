package ru.itmo.wp.form.validator;

import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import ru.itmo.wp.form.PostForm;
import ru.itmo.wp.service.JwtService;

@Component
public class PostFormWriteValidator implements Validator {
    private final JwtService jwtService;

    public PostFormWriteValidator(JwtService jwtService) {
        this.jwtService = jwtService;
    }

    public boolean supports(Class<?> clazz) {
        return PostForm.class.equals(clazz);
    }

    public void validate(Object target, Errors errors) {
        if (!errors.hasErrors()) {
            PostForm postForm = (PostForm) target;
            if (jwtService.find(postForm.getJwt()) == null) {
                errors.reject("only-for-logged-users", "Only for logged users");
            }
        }
    }
}
