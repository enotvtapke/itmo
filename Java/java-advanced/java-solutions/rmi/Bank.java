package rmi;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.List;

public interface Bank extends Remote {
    /**
     * Creates a new account with specified identifier if it is not already exists.
     * @param id account id
     * @return created or existing account.
     */
    Account createAccount(String id) throws RemoteException;

    Account createAccount(Person person, String accountId) throws RemoteException;

    /**
     * Returns account by identifier.
     * @param id account id
     * @return account with specified identifier or {@code null} if such account does not exists.
     */
    Account getAccount(String id) throws RemoteException;

    Account getAccount(String passportId, String accountId) throws RemoteException;

    Person registerPerson(String passportNumber, String firstName, String lastName) throws RemoteException;

    Person getRemotePerson(String passportId) throws RemoteException;

    Person getLocalPerson(String passportId) throws RemoteException;

    List<Account> getPersonAccounts(Person person) throws RemoteException;
}
