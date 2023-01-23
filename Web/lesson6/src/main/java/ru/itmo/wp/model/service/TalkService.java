package ru.itmo.wp.model.service;

import com.google.common.base.Strings;
import ru.itmo.wp.model.domain.Talk;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.repository.TalkRepository;
import ru.itmo.wp.model.repository.UserRepository;
import ru.itmo.wp.model.repository.impl.TalkRepositoryImpl;
import ru.itmo.wp.model.repository.impl.UserRepositoryImpl;

import java.util.List;

public class TalkService {
    private final TalkRepository talkRepository = new TalkRepositoryImpl();
    private final UserRepository userRepository = new UserRepositoryImpl();

    public void validateTalk(Talk talk) throws ValidationException {
        if (Strings.isNullOrEmpty(talk.getText())) {
            throw new ValidationException("Text is required");
        }
        //(user.getEmail().length() - user.getEmail().replace("@", "").length())
        if (talk.getText().length() > 255) {
            throw new ValidationException("Text should be shorter than 255 characters");
        }

        if (userRepository.find(talk.getSourceUserId()) == null) {
            throw new ValidationException("Invalid source user");
        }
        if (userRepository.find(talk.getTargetUserId()) == null) {
            throw new ValidationException("Invalid target user");
        }
    }

    public List<Talk> findAllBySourceUserIdOrTargetUserId(long sourceUserId, long targetUserId) {
        return talkRepository.findAllBySourceUserIdOrTargetUserId(sourceUserId, targetUserId);
    }
    public void save(Talk talk) {
        talkRepository.save(talk);
    }
}
