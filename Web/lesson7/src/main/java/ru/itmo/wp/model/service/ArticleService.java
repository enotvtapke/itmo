package ru.itmo.wp.model.service;

import com.google.common.base.Strings;
import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.domain.ArticleWithAuthor;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.repository.ArticleRepository;
import ru.itmo.wp.model.repository.UserRepository;
import ru.itmo.wp.model.repository.impl.ArticleRepositoryImpl;
import ru.itmo.wp.model.repository.impl.UserRepositoryImpl;

import java.util.LinkedList;
import java.util.List;

/** @noinspection UnstableApiUsage*/
public class ArticleService {
    private final ArticleRepository articleRepository = new ArticleRepositoryImpl();
    private final UserRepository userRepository = new UserRepositoryImpl();

    public void validateArticle(Article article) throws ValidationException {
        if (Strings.isNullOrEmpty(article.getTitle()) || article.getTitle().matches("\\s*")) {
            throw new ValidationException("Title is required");
        }
        if (Strings.isNullOrEmpty(article.getText()) || article.getText().matches("\\s*")) {
            throw new ValidationException("Text is required");
        }
    }

    public void save(Article article) {
        articleRepository.save(article);
    }

    public void updateHiddenById(long id, boolean hidden) {
        articleRepository.updateHiddenById(id, hidden);
    }

    public List<Article> findAll() {
        return articleRepository.findAll();
    }

    public List<ArticleWithAuthor> findAllNotHiddenWithAuthors() {
        List<Article> articles = articleRepository.findAllNotHidden();
        List<ArticleWithAuthor> articlesWithAuthor = new LinkedList<>();
        for (Article article : articles) {
            ArticleWithAuthor articleWithAuthor = new ArticleWithAuthor();
            articleWithAuthor.setArticle(article);
            String author = userRepository.find(article.getUserId()).getLogin();
            articleWithAuthor.setAuthor(author);
            articlesWithAuthor.add(articleWithAuthor);
        }
        return articlesWithAuthor;
    }

    public List<Article> findAllNotHidden() {
        return articleRepository.findAllNotHidden();
    }

    public List<Article> findByUserId(long userId) {
        return articleRepository.findByUserId(userId);
    }

    public Article find(long id) {
        return articleRepository.find(id);
    }
}
