package info.kgeorgiy.ja.stupnikov.bank.test;

import info.kgeorgiy.ja.stupnikov.bank.*;
import org.junit.Assert;
import org.junit.Test;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.IntStream;

public class BankTest {
    private final Bank bank = new RemoteBank(8088);

    private static class PersonDto {
        private final String passportId;
        private final String firstName;
        private final String lastName;

        public PersonDto(String passportId, String firstName, String lastName) {
            this.passportId = passportId;
            this.firstName = firstName;
            this.lastName = lastName;
        }

        public String getPassportId() {
            return passportId;
        }

        public String getFirstName() {
            return firstName;
        }

        public String getLastName() {
            return lastName;
        }
    }

    private List<Person> registerPersons(List<PersonDto> personDtos) {
        List<Person> persons = new ArrayList<>();
        for (PersonDto personDto : personDtos) {
            try {
                persons.add(bank.registerPerson(
                        personDto.getPassportId(),
                        personDto.getFirstName(),
                        personDto.getLastName()
                ));
            } catch (RemoteException e) {
                throw new AssertionError("Can't register person ", e);
            }
        }
        return persons;
    }

    private List<PersonDto> generatePersonDtos(int n) {
        return IntStream.range(0, n).mapToObj(Integer::toString)
                .map(i -> new PersonDto(i + "a", i + "b", i + "c")).toList();
    }

    private List<Person> getRegisteredPerson() {
        return registerPersons(generatePersonDtos(1));
    }

    @Test
    public void test01_registerPerson() {
        PersonDto personDto = new PersonDto("123412412412", "Al", "Stp");
        Person person;
        person = registerPersons(List.of(personDto)).get(0);
        try {
            Assert.assertEquals(person.getPassportId(), personDto.getPassportId());
            Assert.assertEquals(person.getFirstName(), personDto.getFirstName());
            Assert.assertEquals(person.getLastName(), personDto.getLastName());
        } catch (RemoteException e) {
            throw new AssertionError("Can't access remote object ", e);
        }
    }

    @Test
    public void test02_registerPersons() {
        List<PersonDto> personDtos = generatePersonDtos(10);
        List<Person> persons = registerPersons(personDtos);
        for (int i = 0; i < persons.size(); i++) {
            Person person = persons.get(i);
            PersonDto personDto = personDtos.get(i);
            try {
                Assert.assertEquals(person.getPassportId(), personDto.getPassportId());
                Assert.assertEquals(person.getFirstName(), personDto.getFirstName());
                Assert.assertEquals(person.getLastName(), personDto.getLastName());
            } catch (RemoteException e) {
                throw new AssertionError("Can't access remote object ", e);
            }
        }
    }

    @Test
    public void test03_getRemotePersons() {
        int n = 10;
        List<PersonDto> personDtos = generatePersonDtos(n);
        registerPersons(personDtos);
        try {
            for (PersonDto personDto : personDtos) {
                Person person = bank.getRemotePerson(personDto.getPassportId());
                Assert.assertEquals(person.getPassportId(), personDto.getPassportId());
                Assert.assertEquals(person.getFirstName(), personDto.getFirstName());
                Assert.assertEquals(person.getLastName(), personDto.getLastName());
            }
            for (int i = n; i < 2 * n; i++) {
                Person person = bank.getRemotePerson(Integer.toString(i));
                Assert.assertNull(person);
            }
        } catch (RemoteException e) {
            throw new AssertionError("Can't access remote object ", e);
        }
    }

    private List<Account> createAccounts(List<Person> persons, List<String> subIds) {
        List<Account> accounts = new ArrayList<>();
        for (int i = 0; i < persons.size(); i++) {
            Person person = persons.get(i);
            try {
                accounts.add(bank.createAccount(person, subIds.get(i)));
            } catch (RemoteException e) {
                throw new AssertionError("Can't create account ", e);
            }
        }
        return accounts;
    }

    @Test
    public void test04_createAccounts() {
        int n = 10;
        List<Person> persons = registerPersons(generatePersonDtos(n));
        List<String> subIds = ThreadLocalRandom.current().ints(n).mapToObj(Integer::toString).toList();
        List<Account> accounts = createAccounts(persons, subIds);
        for (int i = 0; i < n; i++) {
            Person person = persons.get(i);
            String subId = subIds.get(i);
            Account account = accounts.get(i);
            try {
                Assert.assertEquals(account.getId(), person.getPassportId() + ":" + subId);
            } catch (RemoteException e) {
                throw new AssertionError("Can't access remote object ", e);
            }
        }
    }

    private String getAccountId(Person person, String subId) {
        try {
            return person.getPassportId() + ":" + subId;
        } catch (RemoteException e) {
            throw new AssertionError("Can't access remote object ", e);
        }
    }

    @Test
    public void test05_getAccounts() {
        int n = 10;
        List<Person> persons = registerPersons(generatePersonDtos(n));
        List<String> subIds = ThreadLocalRandom.current().ints(n).mapToObj(Integer::toString).toList();
        createAccounts(persons, subIds);
        try {
            for (int i = 0; i < n; i++) {
                Person person = persons.get(i);
                String subId = subIds.get(i);
                Account account = bank.getAccount(person.getPassportId(), subId);
                Assert.assertEquals(account.getId(), getAccountId(person, subId));
            }
            for (int i = 0; i < n; i++) {
                Account account = bank.getAccount(Integer.toString(i), Integer.toString(i));
                Assert.assertNull(account);
            }
        } catch (RemoteException e) {
            throw new AssertionError("Can't access remote object ", e);
        }
    }

    @Test
    public void test06_multipleAccounts() {
        int n = 10;
        Person person = getRegisteredPerson().get(0);
        List<Person> repeated = IntStream.range(0, n).mapToObj(i -> person).toList();
        List<String> subIds = ThreadLocalRandom.current().ints(n).mapToObj(Integer::toString).toList();
        List<Account> accounts = createAccounts(repeated, subIds);
        List<Account> personAccounts = null;
        try {
            personAccounts = bank.getPersonAccounts(person);
        } catch (RemoteException e) {
            e.printStackTrace();
        }
        Assert.assertEquals(accounts, personAccounts);
    }

    private boolean compareAccounts(Account a, Account b) {
        try {
            return a.getId().equals(b.getId()) && a.getAmount() == b.getAmount();
        } catch (RemoteException e) {
            throw new AssertionError("Can't access remote object ", e);
        }
    }

    @Test
    public void test07_getLocalPerson() {
        Person person = getRegisteredPerson().get(0);
        try {
            List<Account> accounts = List.of(
                    bank.createAccount(person, "a"),
                    bank.createAccount(person, "b"),
                    bank.createAccount(person, "c")
            );
            LocalPerson localPerson = (LocalPerson) bank.getLocalPerson(person.getPassportId());
            Assert.assertEquals(localPerson.getPassportId(), person.getPassportId());
            Assert.assertEquals(localPerson.getFirstName(), person.getFirstName());
            Assert.assertEquals(localPerson.getLastName(), person.getLastName());
            List<Account> localAccounts = localPerson.getAccounts();
            Assert.assertEquals(accounts.size(), localAccounts.size());
            for (int i = 0; i < accounts.size(); i++) {
                Assert.assertTrue(compareAccounts(localAccounts.get(i), accounts.get(i)));
            }
        } catch (RemoteException e) {
            throw new AssertionError("Can't access remote object ", e);
        }
    }

    @Test
    public void test08_modifyLocalPerson() {
        Person person = getRegisteredPerson().get(0);
        try {
            LocalPerson localPerson = (LocalPerson) bank.getLocalPerson(person.getPassportId());
            localPerson.createAccount("a");
            Assert.assertNull(bank.getAccount(person.getPassportId(), "a"));
            bank.createAccount(person, "b");
            Assert.assertNull(localPerson.getAccount(getAccountId(person, "b")));
        } catch (RemoteException e) {
            throw new AssertionError("Can't access remote object ", e);
        }
    }
}
