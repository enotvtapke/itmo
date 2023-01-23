package info.kgeorgiy.ja.stupnikov.bank;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class RemoteBank implements Bank {
    private final int port;
    private final ConcurrentMap<String, Account> accounts = new ConcurrentHashMap<>();
    private final ConcurrentMap<String, List<Account>> personAccounts = new ConcurrentHashMap<>();
    private final ConcurrentMap<String, Person> persons = new ConcurrentHashMap<>();

    public RemoteBank(final int port) {
        this.port = port;
    }

    @Override
    public Account createAccount(final String id) throws RemoteException {
        System.out.println("Creating account " + id);
        Account account = new RemoteAccount(id);
        if (accounts.putIfAbsent(id, account) == null) {
            UnicastRemoteObject.exportObject(account, port);
            return account;
        } else {
            return getAccount(id);
        }
    }

    @Override
    public Account createAccount(Person person, final String accountId) throws RemoteException {
        String id = person.getPassportId() + ":" + accountId;
        System.out.println("Creating account " + id);
        final Account account = new RemoteAccount(id);
        if (accounts.putIfAbsent(id, account) == null) {
            List<Account> accounts = personAccounts.get(person.getPassportId());
            if (accounts != null) {
                accounts.add(account);
            } else {
                personAccounts.put(person.getPassportId(), new ArrayList<>(List.of(account)));
            }
            UnicastRemoteObject.exportObject(account, port);
            return account;
        } else {
            return getAccount(id);
        }
    }

    @Override
    public Account getAccount(final String id) {
        System.out.println("Retrieving account " + id);
        return accounts.get(id);
    }

    @Override
    public Account getAccount(String passportId, final String subId) {
        String id = passportId + ":" + subId;
        System.out.println("Retrieving account " + id);
        return accounts.get(id);
    }

    @Override
    public Person registerPerson(String passportId, String firstName, String lastName) throws RemoteException {
        System.out.println("Registering account " + passportId);
        final Person person = new RemotePerson(passportId, firstName, lastName);
        if (persons.putIfAbsent(passportId, person) == null) {
            UnicastRemoteObject.exportObject(person, port);
            return person;
        } else {
            return getRemotePerson(passportId);
        }
    }

    @Override
    public Person getRemotePerson(String passportId) {
        System.out.println("Retrieving person " + passportId);
        return persons.get(passportId);
    }

    @Override
    public Person getLocalPerson(String passportId) throws RemoteException {
        Person person = getRemotePerson(passportId);
        if (person == null) {
            return null;
        }
        return new LocalPerson(person, getPersonAccounts(person));
    }

    @Override
    public List<Account> getPersonAccounts(Person person) throws RemoteException {
        return personAccounts.get(person.getPassportId());
    }
}
