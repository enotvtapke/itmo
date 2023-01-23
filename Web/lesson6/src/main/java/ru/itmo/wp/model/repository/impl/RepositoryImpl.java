package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.database.DatabaseUtils;

import javax.sql.DataSource;
import java.sql.*;

public abstract class RepositoryImpl {
    protected final DataSource DATA_SOURCE = DatabaseUtils.getDataSource();

    public ResultSet query(String query, Object... args) throws SQLException {
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(query)) {
                for (int i = 0; i < args.length; i++) {
                    statement.setObject(i + 1, args[i], getSqlType(args[i]));
                }
                return statement.executeQuery();
            }
        }
    }

    private int getSqlType(Object object) throws SQLException {
        switch (object.getClass().getName()) {
            case "java.lang.Long":
                return Types.BIGINT;
            case "java.lang.String":
                return Types.VARCHAR;
            default:
                throw new SQLException("Can't get SQL type");
        }
    }

    public void update(String query, Object... args) throws SQLException {
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(query, Statement.RETURN_GENERATED_KEYS)) {
                for (int i = 0; i < args.length; i++) {
                    statement.setObject(i + 1, args[i], getSqlType(args[i]));
                }
                if (statement.executeUpdate() != 1) {
                    throw new SQLException();
                }
                ResultSet r = statement.getGeneratedKeys();
                r.next();
                r.next();
            }
        }
    }
}
