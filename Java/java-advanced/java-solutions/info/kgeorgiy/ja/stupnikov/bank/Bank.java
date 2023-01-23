package info.kgeorgiy.ja.stupnikov.bank;

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

    /**
     * Creates a new account related to person if it is not already exists.
     * @param person person
     * @param accountId account id
     * @return created or existing account.
     */
    Account createAccount(Person person, String accountId) throws RemoteException;

    /**
     * Returns account by identifier.
     * @param id account id
     * @return account with specified identifier or {@code null} if such account does not exists.
     */
    Account getAccount(String id) throws RemoteException;

    /**
     * Returns account by person passport id and sub id.
     * @param passportId person passport id
     * @param subId account sub id
     * @return account with specified identifier or {@code null} if such account does not exists.
     */
    Account getAccount(String passportId, String subId) throws RemoteException;

    /**
     * Registers new person in bank.
     * @param passportId person passport number
     * @param firstName person fist name
     * @param lastName person last name
     * @return Person instance
     * @throws RemoteException when can not access remote object
     */
    Person registerPerson(String passportId, String firstName, String lastName) throws RemoteException;

    /**
     * Retruns remote person by passport id.
     * @param passportId passport id
     * @return RemotePerson instance
     * @throws RemoteException when can not access remote object
     */
    Person getRemotePerson(String passportId) throws RemoteException;

    /**
     * Returns local person by passport id.
     * @param passportId passport id
     * @return LocalPerson instance
     * @throws RemoteException when can not access remote object
     */
    Person getLocalPerson(String passportId) throws RemoteException;

    /**
     * Returns all person accounts.
     * @param person person
     * @return list of accounts
     * @throws RemoteException when can not access remote object
     */
    List<Account> getPersonAccounts(Person person) throws RemoteException;
}
