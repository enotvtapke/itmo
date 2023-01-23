package ru.itmo.wp.model.repository;

import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.domain.User;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.List;

public interface ArticleRepository {
    Article find(long id);
    List<Article> findAll();
    List<Article> findByUserId(long userId);
    Article toArticle(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException;
    void save(Article article);
    void updateHiddenById(long id, boolean hidden);
    List<Article> findAllNotHidden();
}
