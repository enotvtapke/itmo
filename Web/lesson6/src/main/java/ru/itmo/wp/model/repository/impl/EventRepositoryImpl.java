package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.domain.Event;
import ru.itmo.wp.model.exception.RepositoryException;
import ru.itmo.wp.model.repository.EventRepository;

import java.sql.SQLException;

public class EventRepositoryImpl extends RepositoryImpl implements EventRepository {

    @Override
    public void save(Event event) {
        try {
            update("INSERT INTO `Event` (`userId`, `type`, `creationTime`) VALUES (?, ?, NOW());",
                    event.getUserId(),
                    event.getType().name()
            );
        } catch (SQLException e) {
            throw new RepositoryException("Can't save Event.", e);
        }
    }
}
