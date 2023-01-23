package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Talk;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.service.TalkService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Collections;
import java.util.List;
import java.util.Map;

@SuppressWarnings({"unused", "RedundantSuppression"})
public class TalksPage extends Page {
    private final TalkService talkService = new TalkService();

    @Override
    public void before(HttpServletRequest request, Map<String, Object> view) {
        super.before(request, view);
        User user = getUser(request);
        if (user == null) {
            setMessage(request, "Only for logged users");
            throw new RedirectException("/index");
        }
        view.put("users", userService.findAll());
        view.put("talks", talkService.findAllBySourceUserIdOrTargetUserId(user.getId(), user.getId()));
    }

    private void talk(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        User sourceUser = getUser(request);
        long sourceUserId = sourceUser.getId();
        long targetUserId = Long.parseLong(request.getParameter("targetUserId"));
        String text = request.getParameter("text");
        Talk talk = new Talk();
        talk.setSourceUserId(sourceUserId);
        talk.setTargetUserId(targetUserId);
        talk.setText(text);
        talkService.validateTalk(talk);
        talkService.save(talk);
        setMessage(request, "Talk successfully saved!");
        throw new RedirectException("/talks");
    }
}
