package rmi;

import java.io.Serializable;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class LocalPerson extends AbstractPerson implements Serializable {
    Map<String, Account> accounts = new HashMap<>();

    public LocalPerson(String passportId, String firstName, String lastName) {
        super(passportId, firstName, lastName);
    }

    public LocalPerson() {
        super();
    }

    public LocalPerson(Person person, List<Account> accounts) throws RemoteException {
        super(person.getPassportId(), person.getFirstName(), person.getLastName());
        for (Account account : accounts) {
            this.accounts.put(account.getId(), new LocalAccount(account));
        }
    }

    public Account createAccount(final String id) {
        String accountId = this.getPassportId() + ":" + id;
        System.out.println("Creating account " + accountId);
        final Account account = new LocalAccount(id);
        if (accounts.putIfAbsent(accountId, account) == null) {
            return account;
        } else {
            return getAccount(accountId);
        }
    }

    public Account getAccount(final String id) {
        System.out.println("Retrieving account " + id);
        return accounts.get(id);
    }

    public List<Account> getAccounts() {
        System.out.println("Retrieving accounts");
        return new ArrayList<>(accounts.values());
    }
}
