package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.domain.Talk;
import ru.itmo.wp.model.exception.RepositoryException;
import ru.itmo.wp.model.repository.TalkRepository;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class TalkRepositoryImpl extends RepositoryImpl implements TalkRepository {

    @Override
    public List<Talk> findAllBySourceUserIdOrTargetUserId(long sourceUserId, long targetUserId) {
        List<Talk> talks = new ArrayList<>();
        try {
            ResultSet resultSet = query("SELECT * FROM Talk WHERE sourceUserId=? OR targetUserId=? ORDER BY creationTime",
                    sourceUserId,
                    targetUserId
            );
            Talk talk;
            while ((talk = toTalk(resultSet)) != null) {
                talks.add(talk);
            }
            return talks;
        } catch (SQLException e) {
            throw new RepositoryException("Can't find Talk.", e);
        }
    }

    @Override
    public Talk find(long id) {
        try {
            ResultSet resultSet = query("SELECT * FROM Talk WHERE id=?", id);
            return toTalk(resultSet);
        } catch (SQLException e) {
            throw new RepositoryException("Can't find Talk.", e);
        }
    }

    @Override
    public void save(Talk talk) {
        try {
            update("INSERT INTO `Talk` (`sourceUserId`, `targetUserId`, `text`, `creationTime`) VALUES (?, ?, ?, NOW());",
                    talk.getSourceUserId(),
                    talk.getTargetUserId(),
                    talk.getText()
            );
        } catch (SQLException e) {
            throw new RepositoryException("Can't save Talk.", e);
        }
    }

    private Talk toTalk(ResultSet resultSet) throws SQLException {
        if (!resultSet.next()) {
            return null;
        }
        ResultSetMetaData metaData = resultSet.getMetaData();

        Talk talk = new Talk();
        for (int i = 1; i <= metaData.getColumnCount(); i++) {
            switch (metaData.getColumnName(i)) {
                case "id":
                    talk.setId(resultSet.getLong(i));
                    break;
                case "sourceUserId":
                    talk.setSourceUserId(resultSet.getLong(i));
                    break;
                case "targetUserId":
                    talk.setTargetUserId(resultSet.getLong(i));
                    break;
                case "text":
                    talk.setText(resultSet.getString(i));
                    break;
                case "creationTime":
                    talk.setCreationTime(resultSet.getTimestamp(i));
                    break;
                default:
                    // No operations.
            }
        }

        return talk;
    }
}
