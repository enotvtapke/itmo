package ru.itmo.wp.lesson8.service;

import org.springframework.stereotype.Service;
import ru.itmo.wp.lesson8.domain.Notice;
import ru.itmo.wp.lesson8.form.NoticeContent;
import ru.itmo.wp.lesson8.repository.NoticeRepository;

import java.util.List;

@Service
public class NoticeService {
    private final NoticeRepository noticeRepository;

    public NoticeService(NoticeRepository noticeRepository) {
        this.noticeRepository = noticeRepository;
    }

    public List<Notice> findAll() {
        return noticeRepository.findAllByOrderByIdDesc();
    }

    public Notice notice(NoticeContent noticeContent) {
        Notice notice = new Notice();
        notice.setContent(noticeContent.getContent());
        noticeRepository.save(notice);
        return notice;
    }

//    public NoticeService(UserRepository userRepository) {
//        this.userRepository = userRepository;
//    }
//
//    public User register(UserCredentials userCredentials) {
//        User user = new User();
//        user.setLogin(userCredentials.getLogin());
//        userRepository.save(user);
//        userRepository.updatePassword(user.getId(), userCredentials.getLogin(), userCredentials.getPassword());
//        return user;
//    }
//
//    public boolean isLoginVacant(String login) {
//        return userRepository.countByLogin(login) == 0;
//    }
//
//    public User findByLoginAndPassword(String login, String password) {
//        return login == null || password == null ? null : userRepository.findByLoginAndPassword(login, password);
//    }
//
//    public User findById(Long id) {
//        return id == null ? null : userRepository.findById(id).orElse(null);
//    }
//
//    public List<User> findAll() {
//        return userRepository.findAllByOrderByIdDesc();
//    }
}
