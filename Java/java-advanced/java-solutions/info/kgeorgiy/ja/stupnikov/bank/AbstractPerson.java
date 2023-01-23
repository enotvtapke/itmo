package info.kgeorgiy.ja.stupnikov.bank;

abstract public class AbstractPerson implements Person {
    private String passportId;
    private String firstName;
    private String lastName;

    public AbstractPerson(String passportId, String firstName, String lastName) {
        this.passportId = passportId;
        this.firstName = firstName;
        this.lastName = lastName;
    }

    public AbstractPerson() {
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
