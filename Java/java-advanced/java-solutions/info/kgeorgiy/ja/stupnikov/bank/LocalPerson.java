package info.kgeorgiy.ja.stupnikov.bank;

import java.io.Serializable;
import java.rmi.RemoteException;
import java.util.*;

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
        if (accounts != null) {
            for (Account account : accounts) {
                this.accounts.put(account.getId(), new LocalAccount(account));
            }
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
        List<Account> accounts = new ArrayList<>(this.accounts.values());
        Collections.reverse(accounts);
        return accounts;
    }
}
