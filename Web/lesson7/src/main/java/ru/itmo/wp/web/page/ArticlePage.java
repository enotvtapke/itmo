package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.service.ArticleService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

/** @noinspection unused*/
public class ArticlePage {
    private final ArticleService articleService = new ArticleService();

    private void action(HttpServletRequest request, Map<String, Object> view) {
        if (request.getSession().getAttribute("user") == null) {
            request.getSession().setAttribute("message", "Only for logged users!");
            throw new RedirectException("/index");
        }
    }

    private void save(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        Article article = new Article();
        article.setUserId(Long.parseLong(request.getParameter("userId")));
        article.setTitle(request.getParameter("title"));
        article.setText(request.getParameter("text"));

        articleService.validateArticle(article);
        articleService.save(article);
    }

    private void findAll(HttpServletRequest request, Map<String, Object> view) {
        view.put("articles", articleService.findAll());
    }

    private void findAllNotHiddenWithAuthors(HttpServletRequest request, Map<String, Object> view) {
        view.put("articles", articleService.findAllNotHiddenWithAuthors());
    }

    private void findAllNotHidden(HttpServletRequest request, Map<String, Object> view) {
        view.put("articles", articleService.findAllNotHidden());
    }

    private void findArticle(HttpServletRequest request, Map<String, Object> view) {
        view.put("article",
                articleService.find(Long.parseLong(request.getParameter("userId"))));
    }
}
