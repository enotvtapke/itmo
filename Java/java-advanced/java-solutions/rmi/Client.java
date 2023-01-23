package rmi;

import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.util.List;
import java.util.Objects;

public final class Client {
    /** Utility class. */
    private Client() {}

    public static void main(final String... args) throws RemoteException {
        final Bank bank;
        try {
            bank = (Bank) Naming.lookup("//localhost/bank");
        } catch (final NotBoundException e) {
            System.out.println("Bank is not bound");
            return;
        } catch (final MalformedURLException e) {
            System.out.println("Bank URL is invalid");
            return;
        }

        if (args.length != 5) {
            System.out.println("Invalid number of arguments");
            return;
        }
        String firstName = args[0];
        String lastName = args[1];
        String passportId = args[2];
        String accountId = args[3];
        int amount = Integer.parseInt(args[4]);
        Person person = bank.getRemotePerson(passportId);
        if (person == null) {
            System.out.println("Registering person");
            person = bank.registerPerson(passportId, firstName, lastName);
        } else {
            System.out.println("Person already exists");
            if (!Objects.equals(person.getFirstName(), firstName)) {
                System.out.println("Invalid first name");
                return;
            }
            if (!Objects.equals(person.getLastName(), lastName)) {
                System.out.println("Invalid last name");
                return;
            }
        }

        Account account = bank.getAccount(person.getPassportId(), accountId);
        if (account == null) {
            System.out.println("Creating account");
            account = bank.createAccount(person, accountId);
        } else {
            System.out.println("Account already exists");
        }
        System.out.println("Account id: " + account.getId());
        System.out.println("Money: " + account.getAmount());
        System.out.println("Adding money");
        account.setAmount(account.getAmount() + amount);
        System.out.println("Money: " + account.getAmount());

        Person localPerson = bank.getLocalPerson(passportId);
        List<Account> accounts = ((LocalPerson) localPerson).getAccounts();
        for (var acc : accounts) {
            System.out.println(account.getId());
        }
    }
}
