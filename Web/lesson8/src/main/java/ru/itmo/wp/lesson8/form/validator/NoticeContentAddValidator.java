package ru.itmo.wp.lesson8.form.validator;

import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import ru.itmo.wp.lesson8.form.NoticeContent;

@Component
public class NoticeContentAddValidator implements Validator {
    public boolean supports(Class<?> clazz) {
        return NoticeContent.class.equals(clazz);
    }

    public void validate(Object target, Errors errors) {
        if (!errors.hasErrors()) {
            NoticeContent noticeForm = (NoticeContent) target;
        }
    }
}
