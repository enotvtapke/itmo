package ru.itmo.wp.model.domain;

import java.io.Serializable;
import java.util.Date;

public class ArticleWithAuthor implements Serializable {
    private Article article;
    private String author;

    public Article getArticle() {
        return article;
    }

    public void setArticle(Article article) {
        this.article = article;
    }

    public String getAuthor() {
        return author;
    }

    public void setAuthor(String author) {
        this.author = author;
    }
}
