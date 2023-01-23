package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.RepositoryException;
import ru.itmo.wp.model.repository.UserRepository;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class UserRepositoryImpl extends RepositoryImpl implements UserRepository {

    @Override
    public User find(long id) {
        try {
            ResultSet resultSet = query("SELECT * FROM User WHERE id=?", id);
            return toUser(resultSet);
        } catch (SQLException e) {
            throw new RepositoryException("Can't find User.", e);
        }
    }

    @Override
    public long count() {
        try {
            ResultSet resultSet = query("SELECT count(*) FROM User");
            resultSet.next();
            return resultSet.getLong(1);
        } catch (SQLException e) {
            throw new RepositoryException("Can't count Users.", e);
        }
    }

    @Override
    public User findByLogin(String login) {
        try {
            ResultSet resultSet = query("SELECT * FROM User WHERE login=?", login);
            return toUser(resultSet);
        } catch (SQLException e) {
            throw new RepositoryException("Can't find User.", e);
        }
    }

    @Override
    public User findByEmail(String email) {
        try {
            ResultSet resultSet = query("SELECT * FROM User WHERE email=?", email);
            return toUser(resultSet);
        } catch (SQLException e) {
            throw new RepositoryException("Can't find User.", e);
        }
    }

    @Override
    public User findByLoginOrEmailAndPasswordSha(String loginOrEmail, String passwordSha) {
        try {
            ResultSet resultSet = query("SELECT * FROM User WHERE (login=? OR email=?) AND passwordSha=?",
                    loginOrEmail,
                    loginOrEmail,
                    passwordSha
            );
            return toUser(resultSet);
        } catch (SQLException e) {
            throw new RepositoryException("Can't find User.", e);
        }
    }

    @Override
    public List<User> findAll() {
        List<User> users = new ArrayList<>();
        try {
            ResultSet resultSet = query("SELECT * FROM User ORDER BY id DESC");
            User user;
            while ((user = toUser(resultSet)) != null) {
                users.add(user);
            }
            return users;
        } catch (SQLException e) {
            throw new RepositoryException("Can't find User.", e);
        }
    }

    private User toUser(ResultSet resultSet) throws SQLException {
        if (!resultSet.next()) {
            return null;
        }
        ResultSetMetaData metaData = resultSet.getMetaData();

        User user = new User();
        for (int i = 1; i <= metaData.getColumnCount(); i++) {
            switch (metaData.getColumnName(i)) {
                case "id":
                    user.setId(resultSet.getLong(i));
                    break;
                case "login":
                    user.setLogin(resultSet.getString(i));
                    break;
                case "email":
                    user.setEmail(resultSet.getString(i));
                    break;
                case "creationTime":
                    user.setCreationTime(resultSet.getTimestamp(i));
                    break;
                default:
                    // No operations.
            }
        }

        return user;
    }

    @Override
    public void save(User user, String passwordSha) {
        try {
            update("INSERT INTO `User` (`login`, `passwordSha`, `creationTime`, `email`) VALUES (?, ?, NOW(), ?)",
                    user.getLogin(),
                    passwordSha,
                    user.getEmail()
            );
        } catch (SQLException e) {
            throw new RepositoryException("Can't save User.", e);
        }
    }
}