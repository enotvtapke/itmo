package ru.itmo.wp.form.validator;

import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import ru.itmo.wp.form.PostForm;

@Component
public class PostFormWriteValidator implements Validator {

    public boolean supports(Class<?> clazz) {
        return PostForm.class.equals(clazz);
    }

    public void validate(Object target, Errors errors) {
        if (!errors.hasErrors()) {
            PostForm postForm = (PostForm) target;
            if (!postForm.getTags().matches("([A-Za-z]|\\s)*")) {
                errors.rejectValue("tags", "tag.only-latin-letters", "tag should contain only latin letters");
            }
        }
    }
}
