package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.service.ArticleService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

/** @noinspection unused*/
public class MyArticlesPage {
    private final ArticleService articleService = new ArticleService();

    private void action(HttpServletRequest request, Map<String, Object> view) {
        if (request.getSession().getAttribute("user") == null) {
            request.getSession().setAttribute("message", "Only for logged users!");
            throw new RedirectException("/index");
        } else {
            User user = (User) request.getSession().getAttribute("user");
            view.put("articles", articleService.findByUserId(user.getId()));
        }
    }

    private void updateHidden(HttpServletRequest request, Map<String, Object> view) {
        //long userId = Long.parseLong(request.getParameter("userId"));
        try {
            long articleId = Long.parseLong(request.getParameter("articleId"));
            Article article = articleService.find(articleId);
            if (article == null || ((User) request.getSession().getAttribute("user")) == null) {
                view.put("error", "Wrong id");
            } else {
                if (article.getUserId() == ((User) request.getSession().getAttribute("user")).getId()) {
                    boolean hidden = Boolean.parseBoolean(request.getParameter("hidden"));
                    articleService.updateHiddenById(articleId, hidden);
                } else {
                    view.put("error", "Wrong id");
                }
            }
        } catch (NumberFormatException e) {
            view.put("error", "Wrong id");
        }

    }
}
